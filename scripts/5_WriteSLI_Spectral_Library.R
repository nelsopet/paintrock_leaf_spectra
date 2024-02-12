setwd("paintrock_leaf_spectra")
source("Functions/writeSLI_source.R")
source("Functions/lecospectR.R")
require(Polychrome)
require(vegan)
require(glue)

paintrock_spectra_df<-read.csv("output/paintrock_spectra_clean.csv")

#Summarize reflectance by speccies and rearrange columns such that the first 
#is wavelength and every subsequent column is a sample, in this case median reflectance by species.

paintrock_spectra_df_median_sp<-paintrock_spectra_df %>% 
    group_by(taxon_code) %>%
    dplyr::select(`X350`:`X2500`) %>% #dim
    pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>% #dim
    mutate(Wavelength = gsub("X","",Wavelength)) %>%
    group_by(taxon_code,Wavelength) %>%  
    dplyr::summarise(Reflectance = median(Reflectance))%>%
    mutate(Wavelength = as.numeric(Wavelength)) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = taxon_code, values_from = Reflectance) %>%
    mutate(Wavelength = as.numeric(Wavelength)) %>%
    dplyr::arrange(Wavelength)

writeSLI(paintrock_spectra_df_median_sp,"output/paintrock_spectra.sli", wavl.units = "Nanometers")

