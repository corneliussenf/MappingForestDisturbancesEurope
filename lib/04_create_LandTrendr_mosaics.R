
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(gdalUtils)
library(rgeos)
library(raster)

# Create mosaics for prediction -------------------------------------------

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

landtredr_path <- "/media/homedrive/data/Public/LandTrendr/greatest_change" # Path to landtrendr runs (stored in folder with naming 'landtrendr_[country]')

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

band_indices <- c(paste0("B", c(1:5, 7)), "NBR", "TCB", "TCG", "TCW")

for (cntr in unique(model_inp$country)) {
  
  dir.create(paste0("data/landtrendr_mosaics/", cntr), showWarnings = FALSE, recursive = TRUE)
  
  print(paste("Running mosaicing for", cntr))
  
  # Fix country polygons
  outline <- shapefile(paste0("data/countries/", cntr, ".shp"))
  outline <- gBuffer(outline, 0, byid = TRUE)
  rgdal::writeOGR(outline, "data/countries", cntr, "ESRI Shapefile", overwrite_layer = TRUE)
  
  for (b in band_indices) {
    
    outfile <- paste0("data/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_", b, ".tif")
    
    print(paste0("Processing for ", b, " ..."))
    
    if (!file.exists(outfile)) {
      
      filelist <- list.files(landtredr_path, pattern = glob2rx(paste0("*", cntr, "*", b, "*.tif")), recursive = TRUE, full.names = TRUE)
        
      gdalwarp(srcfile = filelist,
               dstfile = outfile,
               t_srs = "EPSG:3035",
               tr = c(30, 30),
               output_Raster = FALSE,
               overwrite = TRUE,
               verbose = TRUE,
               cutline = paste0("data/countries/", cntr, ".shp"),
               crop_to_cutline = TRUE,
               dstnodata = "-32768")
    
      print("   ... fisnished!")
      
    } else {
      
      print("   ... skipping as already processed!")
        
    }
  
  }
  
}


