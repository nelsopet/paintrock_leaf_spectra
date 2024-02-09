source("Functions/lecospectR.R")
paintrock_spectra<-readRDS("output/paintrock_spectra.rds")
paintrock_spectra_df<-as.data.frame(paintrock_spectra)
paintrock_spectra_df<-dplyr::select(paintrock_spectra_df, taxon_code, `350`:`2500`) %>% 
  dplyr::filter(`1000`>15) #%>% dim

write.csv(paintrock_spectra_df, "output/paintrock_spectra_clean.csv")

###Code below allows user to review scans if needed
Target_names <- unique(sort(paintrock_spectra_df$taxon_code))

# Creates an empty list
each_target <- list()
meta_columns <- 0

# Function splits the spectral library into spectral objects based on each target (105 Spectral Objects)
for (i in 1:length(Target_names)) {

  # Subset a functional group
  each_target[[i]] <- subset(paintrock_spectra_df, taxon_code == Target_names[i])
  # saves metadata
  metadata <- each_target[[i]][, c(1:(meta_columns))] %>% as.data.frame()
  # Convert to a spectral object
  each_target[[i]] <- as_spectra(each_target[[i]][-1:-(meta_columns)])
  # each_target[[i]] <-normalize(each_target[[i]])
  # Add metadata
  meta(each_target[[i]]) <- data.frame(metadata[, c(1:(meta_columns))], stringsAsFactors = FALSE)
}


# Renames each target in list
#> Target_names
# [1] "ACNE2"    "ACSA3"    "AEFL"     "CACA38"   "CACO15"   "DIVI5"   
# [7] "FAGR"     "FRBI2"    "GLTR"     "JUNI"     "JUVI"     "LIST2"   
#[13] "LITU"     "LITU0004" "OSVI"     "PITA"     "PLOC"     "PRSES"   
#[19] "QUAL"     "QUFA"     "QUMU"     "QUNI"     "QURU"     "QUSH"    
#[25] "SAAL"     "TIAM"     "ULAL"     "ULAM"     "ULRU" 
each_target <- each_target %>% setNames(Target_names)
# Var1 Freq
  plot_interactive(each_target[["ACNE2"]])#1   #Remove scans 1 & 2
  ##each_target[["ACNE2"]] <- each_target[["ACNE2"]][-c(1,2), ]
  #plot_interactive(each_target[["ACSA3"]])#1   #All good
  #plot_interactive(each_target[["AEFL"]])#1   #All good
  #plot_interactive(each_target[["CACA38"]])#1   #All good
  #plot_interactive(each_target[["CACO15"]])#1   #Remove scans 1 & 2
  #plot_interactive(each_target[["DIVI5"]])#1   #All good
  #plot_interactive(each_target[["FAGR"]])#1   #All good
  #plot_interactive(each_target[["FRBI2"]])#1   #All good
  #plot_interactive(each_target[["GLTR"]])#1   #All good
  #plot_interactive(each_target[["JUNI"]])#1   #All good
  #plot_interactive(each_target[["LIST2"]])#1   #All good
  #plot_interactive(each_target[["LITU"]])#1   #One scan much brighter than the others but likely is ok
  #plot_interactive(each_target[["LITU0004"]])#1   #Needs to be combined with LITU
  #plot_interactive(each_target[["OSVI"]])#1   #All good
  #plot_interactive(each_target[["PITA"]])#1   #All good
  #plot_interactive(each_target[["PLOC"]])#1   #All good
  #plot_interactive(each_target[["PRSES"]])#1   #All good
  #plot_interactive(each_target[["QUAL"]])#1   #All good
  #plot_interactive(each_target[["QUFA"]])#1   #All good but are they apart of QUAL?
  #plot_interactive(each_target[["QUNI"]])#1   #All good
  #plot_interactive(each_target[["QURU"]])#1   #All good
  #plot_interactive(each_target[["QUSH"]])#1   #Number 11 needs deleting
  #plot_interactive(each_target[["SAAL"]])#1   #All good
  #plot_interactive(each_target[["TIAM"]])#1   #All good
  #plot_interactive(each_target[["ULAL"]])#1   #Number 10 needs deleting
  #plot_interactive(each_target[["ULAM"]])#1   #All good
  #plot_interactive(each_target[["ULRU"]])#1   #All good
  
  
  
  
