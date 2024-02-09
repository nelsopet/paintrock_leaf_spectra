source("Functions/lecospectR.R")
source("Functions/Scan_Metadata_Reader.R")

#paintrock_spectra<-read_spectra(paste("data"),format="sed")

paintrock_files<-read_files("data")
paintrock_spectra<-read_spectra_fix_missing_spec(paintrock_files)
paintrock_spectra<-Reduce(combine, paintrock_spectra)
paintrock_names<-scan_names(paintrock_files)
paintrock_batch<-read_batch(paintrock_files)

##Combine into metadata
paintrock_meta<-cbind(paintrock_names,paintrock_batch) %>% as.data.frame()
rownames(paintrock_meta)<-NULL  
###Set metadata
meta(paintrock_spectra) = data.frame(paintrock_meta, stringsAsFactors = FALSE)
#save spectral object
saveRDS(paintrock_spectra,"output/paintrock_spectra.rds")


