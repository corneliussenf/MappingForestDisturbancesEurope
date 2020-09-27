
library(tidyverse)
library(gdalUtils)
library(rgeos)

### Create mosaics for prediction

countries <- list.files("LandTrendr/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("LandTrendr/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

for (cntr in unique(model_inp$country)[-c(1, 2)]) {
  
  dir.create(paste0("LandTrendr/landtrendr_mosaics/", cntr), showWarnings = FALSE)
  
  if (!(file.exists(paste0("LandTrendr/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_NBR.tif")) &
      file.exists(paste0("LandTrendr/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_TCW.tif")) &
      file.exists(paste0("LandTrendr/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_B5.tif")) &
      file.exists(paste0("LandTrendr/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_B7.tif")))) {
    
    print(paste("Running mosaicing for", cntr))
    
    outline <- shapefile(paste0("LandTrendr/countries/", cntr, ".shp"))
    outline <- gBuffer(outline, 0, byid = TRUE)
    rgdal::writeOGR(outline, "LandTrendr/countries", cntr, "ESRI Shapefile", overwrite_layer = TRUE)
    
    c("NBR", "TCW", "B5", "B7") %>%
      map(~ list.files("LandTrendr/landtrendr_runs", pattern = glob2rx(paste0("*", cntr, "*", ., "*.tif")), recursive = TRUE, full.names = TRUE)) %>%
      map2(.y = c("NBR", "TCW", "B5", "B7"),
           ~ gdalwarp(srcfile = .,
                      dstfile = paste0("LandTrendr/landtrendr_mosaics/", cntr, "/landtrendr_", cntr, "_", .y, ".tif"),
                      t_srs = "EPSG:3035",
                      tr = c(30, 30),
                      output_Raster = FALSE,
                      overwrite = TRUE,
                      verbose = TRUE,
                      cutline = paste0("LandTrendr/countries/", cntr, ".shp"),
                      crop_to_cutline = TRUE,
                      dstnodata = "-32768")) 
    
  }
  
  dir.create(paste0("LandTrendr/habitat_metrics_mosaics/", cntr), showWarnings = FALSE)
  
  if (!(file.exists(paste0("LandTrendr/habitat_metrics_mosaics/", cntr, "/habitat_metrics_", cntr, "_1984-1986.tif")) &
        (file.exists(paste0("LandTrendr/habitat_metrics_mosaics/", cntr, "/habitat_metrics_", cntr, "_2016-2018.tif")) |
         file.exists(paste0("LandTrendr/habitat_metrics_mosaics/", cntr, "/habitat_metrics_", cntr, "_2018-2018.tif"))))) {
    
    c("1984-1986", "2016-2018", "2018-2018") %>%
      map(~ list.files("LandTrendr/habitat_metrics/", pattern = glob2rx(paste0("*", cntr, "*", ., "*.tif")), recursive = TRUE, full.names = TRUE)) %>%
      map2(.y = c("1984-1986", "2016-2018", "2018-2018"),
           ~ gdalwarp(srcfile = .,
                      dstfile = paste0("LandTrendr/habitat_metrics_mosaics/", cntr, "/habitat_metrics_", cntr, "_", .y, ".tif"),
                      t_srs = "EPSG:3035",
                      tr = c(30, 30),
                      output_Raster = FALSE,
                      overwrite = TRUE,
                      verbose = TRUE,
                      cutline = paste0("LandTrendr/countries/", cntr, ".shp"),
                      crop_to_cutline = TRUE,
                      dstnodata = "-32768"))
    
  }
  
}


