require(tidyverse)
require(compiler)
require(raster)
require(hsdar)
require(spectrolab)
require(ranger)
require(stringr)
require(stringi)
require(rjson)
require(snow)
require(doSNOW)
require(stats)
require(rasterVis)


source("Functions/spectral_operations.R")
source("Functions/raster_operations.R")
source("Functions/dataframe_operations.R")
source("Functions/model_support.R")
source("Functions/utilities.R")
source("Functions/validation.R")
source("Functions/visualization.R")
source("Functions/pfts.R")
source("Functions/type_conversion.R")
source("Functions/training_utilities.R")


#################################################################
###     Main Function for tiled processing 
#################################################################



#' equivalent to the old LandCoverEstimator()
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
estimate_land_cover <- function(
    input_filepath,
    model = NULL, 
    outlier_processing = NULL,
    transform_type = NULL,
    config_path = "./config.json",
    cache_filepath = "./",
    output_filepath =  paste(
        "output-",
        format(Sys.time(), "%a-%b-%d-%H-%M-%S-%Y"), ".envi", sep=""),
    use_external_bands = TRUE
) {

    path <- getwd()
    #write terminal output to the log file

    # Read in the configuration file
    config <- rjson::fromJSON(file = config_path)

    


    # Load the model
    if(is.null(model)){
        model <- load_model(config$model_path)
    }

    # load the input datacube and split into tiles
    input_raster <- raster::brick(input_filepath)
    input_crs <- raster::crs(input_raster)
    input_extent <- raster::extent(input_raster)

    if(is.na(input_crs)){
        warning("The input raster does not have a CRS specified.")
    }

    # save the band names since they will be lost now that we are using .envi tiles 
    bandnames <- names(input_raster)
    if(use_external_bands){
        band_count <- raster::nlayers(input_raster)
        bandnames <- read.csv(config$external_bands)$x[1:band_count] %>% as.vector()
        names(input_raster) <- bandnames
    }

    num_tiles_x <- config$x_tiles
    num_tiles_y <- config$y_tiles

    if(config$automatic_tiling){
        num_tiles_x <- calc_num_tiles(
            input_filepath,
            max_size = config$max_size)
        num_tiles_y <- calc_num_tiles(
            input_filepath,
            max_size = config$max_size)
    }

    tile_filenames <- make_tiles(
        input_raster,
        num_x = num_tiles_x,
        num_y = num_tiles_y,
        save_path = config$tile_path,
        verbose = FALSE
    )

    # determine the number of cores to use
    num_cores <- parallel::detectCores() - 1#detect cores on system
    # see if the number of cores to use is specified in the config
    if(is.integer(config$clusterCores)){
        num_cores <- config$clusterCores
    }
    # set up the parallel cluster
    raster::beginCluster(num_cores)
    cl <- raster::getCluster()
    print(cl)


    print(paste0(parallel::detectCores(), " Cores Detected for processing..."))
    print(paste0("Cluster initialized with ", num_cores, " processes"))
    background_blas_threads <- RhpcBLASctl::get_num_procs()
    background_omp_threads <- RhpcBLASctl::omp_get_max_threads()

    # load the outlier processing method if none is specified by user
    outlier_processing_cfg <- outlier_processing
    if(is.null(outlier_processing)){
        outlier_processing_cfg <- config$outlier_processing
    }

    # load transform type if none is specified
    transform_type_cfg <- transform_type
    if(is.null(transform_type)){
        transform_type_cfg <- config$transform_type
    }




    rm(input_raster)
    gc()

    prediction_filenames <- lapply(
        tile_filenames,
        function(tile_filename){
            return(.convert_tile_filename(tile_filename))
    }) %>% as.vector()

    # initialize the variable for the tilewise results
    tile_results <- vector("list", length = length(tile_filenames))
    #edge artifacts?


    # exports <- c()



    if(config$parallelize_by_tiles){
        #doSNOW::registerDoSNOW(cl)
        doParallel::registerDoParallel(cl)
        tile_results <- foreach::foreach(
            i = seq_along(tile_filenames),
            .export = as.vector(ls(.GlobalEnv))
        ) %dopar% {
            gc()
            sink(get_log_filename(tile_filenames[[i]]))
            tile_result <- process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                aggregation = config$aggregation,
                cluster = NULL,
                return_raster = TRUE,
                band_names = bandnames,
                bandwidth = config$bandwidth,
                outlier_processing = outlier_processing_cfg,
                transform_type = transform_type_cfg,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE)
            sink(NULL)
            return(tile_result)
        }
    } else {
        tile_results <- foreach::foreach(
            i=seq_along(tile_filenames)
        ) %do% {
            gc()
            sink(get_log_filename(tile_filenames[[i]]))
            tile_result <- process_tile(
                tile_filename = tile_filenames[[i]],
                ml_model = model, 
                aggregation = config$aggregation,
                cluster = cl,
                return_raster = TRUE,
                band_names = bandnames,
                bandwidth = config$bandwidth,
                outlier_processing = outlier_processing_cfg,
                transform_type = transform_type_cfg,
                return_filename = TRUE,
                save_path = prediction_filenames[[i]],
                suppress_output = TRUE)
        sink(NULL)
        return(tile_result)
        }
    }
    gc() #clean up

    

    print("Tile based processing complete")
    raster::endCluster()
    print(tile_results)

    # return the background thread configuration to its initial state
    if(config$parallelize_by_tiles){
        RhpcBLASctl::blas_set_num_threads(background_blas_threads)
        RhpcBLASctl::omp_set_num_threads(background_omp_threads)
    }

    # merge and save the results.
    results <- merge_tiles(prediction_filenames, output_path = output_filepath)
    # load the results from disk to correct data type issues from float/INT2U (C++ uint16_t) conversion
    results <- raster::raster(output_filepath)

    return(results)
}



#################################################################
###     Main Function for single-file processing 
#################################################################

#' processes a small raster imarge
#'
#' processes the given image in-memory.  This assumes that the image is small enough that tiling is not required.  
#' Functions include data munging, imputation, automated vegetation index calculation, model inference, and data type conversion. 
#' The outputs are optionally saved to disk as well.  
#' Parallization is used if a connection to a parallel (or raster) package cluster is provided
#'
#' @return 
#' @param tile_filename: A string specifying the location of the target raster on the disk
#' @param ml_model: the machine learning model for prediction.  
#' @param cluster: a cluster, from the raster::beginCluster(); raster::getCluster() or parallel::makeCluster().  Default is NULL (no parallelism).
#' @param return_raster: (default: TRUE) returns a rasterLayer object if true, or a data.frame if FALSE.
#' @param save_path: the path to save the output.  If NULL (default) no file is saved.  Otherwise it attempts to save the file to the location specified.
#' @param suppress_output: if TRUE, returns the save location of the output, rather than the output itself.  
#' If FALSE (default), the function returns a raster::rasterLayer or base::data.frame as determined by return_raster parameter.
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
process_tile <- function(
    tile_filename,
    ml_model, 
    aggregation,
    cluster = NULL,
    return_raster = TRUE,
    band_names=NULL,
    bandwidth = 5,
    outlier_processing = "none",
    transform_type = "none",
    return_filename = FALSE,
    save_path = NULL,
    suppress_output = FALSE
    ) {
    set.seed(61718)
    raster_obj <- raster::brick(tile_filename)
    input_crs <- raster::crs(raster_obj)
    print(paste0("preprocessing raster at ", tile_filename))
    base_df <- preprocess_raster_to_df(
        raster_obj,
        ml_model,
        band_names = band_names)

    if(nrow(base_df) < 2) {
        handle_empty_tile(
            raster_obj,
            save_path = save_path,
            target_crs = input_crs)

        if(!suppress_output){
            if(return_raster){
                return(raster_obj)
            } else {
                return(base_df)
            } 

        } 
        return(unlist(save_path))
        # add return value if output is suppressed
    } else {
        # this runs if and only if there is sufficient data
    

            #if there is no data, return the empty tile in the specified format

        rm(raster_obj)
        gc()
        print(colnames(base_df))
        cleaned_df <- drop_zero_rows(base_df)
        rm(base_df)
        gc()

        cleaned_df_no_empty_cols <- drop_empty_columns(cleaned_df) 
        print(summary(cleaned_df_no_empty_cols))
        veg_indices <- get_vegetation_indices(
            cleaned_df_no_empty_cols,
            NULL,
            cluster = cluster)

        try(
            rm(cleaned_df)
        )# sometimes garbage collection gets there first, which is fine
        gc()

        # drop rows that are uniformly zero
      
        resampled_df <- resample_df(
            cleaned_df_no_empty_cols,
            normalize = FALSE,
            delta = bandwidth,
            #max_wavelength = 995.716,
            drop_existing=TRUE)
        gc()

        


        df_full <- cbind(
            subset(cleaned_df_no_empty_cols, select = c("x", "y")),
            resampled_df,
            veg_indices)

        imputed_df <- impute_spectra(
            df_full,
            method = "median",
            cluster = cluster) %>%
            as.data.frame()
 
        # above line should not be needed, testing then deleting
        rm(veg_indices)
        rm(resampled_df)
        rm(cleaned_df_no_empty_cols)
        gc()

        if(!is.function(outlier_processing)){
            print(paste0("Handling Outliers with method: ", outlier_processing))
        } else {
            print("Handling Outliers with User supplied function")
        }
        df_no_outliers <- handle_outliers(
            imputed_df,
            outlier_processing,
            ignore_cols = c("x", "y")
        )
        rm(df_full)

        # replace Inf and NaN values with NA (to be imputed later)
        df_no_outliers <- inf_to_na(df_no_outliers)
        df_no_outliers[is.nan(df_no_outliers)] <- NA

        if(!is.function(outlier_processing)) {
            print(
                paste0(
                    "Transforming the data with transform: ",
                    transform_type
                )
            )
        } else {
            print("Transforming Data with user supplied functions")
        }
        df_preprocessed <- apply_transform(
            df_no_outliers,
            transform_type,
            ignore_cols = c("x", "y")
        )
        rm(df_no_outliers)
        gc()

        
        #print(summary(df_preprocessed))
        imputed_df_2 <- impute_spectra(
                inf_to_na(df_preprocessed),
                method="median")
        #print(summary(imputed_df_2))

        prediction <- apply_model(
            imputed_df_2,
            ml_model)
        
        prediction <- postprocess_prediction(prediction, df_preprocessed)
        rm(df_preprocessed)
        gc()

        prediction <- convert_and_save_output(
            prediction,
            aggregation,
            save_path = save_path,
            return_raster = return_raster,
            target_crs = input_crs)

        
        raster::crs(prediction) <- input_crs

        if(suppress_output){
            #print(save_path)
            return(unlist(save_path))
        }
        return(prediction)
    }
}