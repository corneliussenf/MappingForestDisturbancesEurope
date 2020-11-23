
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)
library(sf)

# Functions ---------------------------------------------------------------

create_plots <- function(cntr, noforest_n) {
  
  ### Get plot data
  
  plots <- shapefile(list.files("data/plots", glob2rx(paste0("*", cntr, "*.shp")), full.names = TRUE))
  
  ### Load forest mask and sample n no-forest pixels
  
  forest_mask <- raster(paste0("data/forestmasks/forestmask_", cntr, ".tif"))
  cntr_shapefile <- read_sf(paste0("data/countries/", cntr, ".shp"))
  
  forest_mask_sample <- st_sample(cntr_shapefile, noforest_n)
  forest_mask_sample <- st_as_sf(forest_mask_sample)
  extr <- raster::extract(forest_mask, forest_mask_sample)
  forest_mask_sample_noforest <- filter(forest_mask_sample, extr == 0)
  
  while (nrow(forest_mask_sample_noforest) < noforest_n) {
    forest_mask_sample <- st_sample(cntr_shapefile, (noforest_n - nrow(forest_mask_sample_noforest)) * 2)
    forest_mask_sample <- st_as_sf(forest_mask_sample)
    extr <- raster::extract(forest_mask, forest_mask_sample)
    forest_mask_sample_noforest <- rbind(forest_mask_sample_noforest, filter(forest_mask_sample, extr == 0))
  }
  
  forest_mask_sample_noforest <- forest_mask_sample_noforest[1:noforest_n, ]
  
  # Return
  
  return(list(st_as_sf(plots), forest_mask_sample_noforest))
  
}

extract_landtrendr <- function(cntr, band, plots, landtrednr_path) {
  
  print(paste0("...", band))
  
  ### Load LandTrendr outputs
  
  landtrendr_change_images <- list.files(landtredr_path, pattern = glob2rx(paste0("*", cntr, "*", band, "*.tif")), 
                                         recursive = TRUE, full.names = TRUE) %>%
    map(stack)
  
  # Extract LandTrendr
  
  landtrendr_extract_forest <- landtrendr_change_images %>% 
    map(~ raster::extract(., plots[[1]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), band, paste, sep = ".")))) %>%
    map(~ mutate(., plotid = plots[[1]]$plotid)) %>%
    map(na.omit) %>%
    bind_rows()
  
  landtrendr_extract_noforest <- landtrendr_change_images %>% 
    map(~ raster::extract(., plots[[2]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), band, paste, sep = ".")))) %>%
    map(~ mutate(., plotid = 99999999)) %>%
    map(na.omit) %>%
    bind_rows()
  
  return(list(landtrendr_extract_forest, landtrendr_extract_noforest))
  
}

# Do the extraction -------------------------------------------------------

samplesizes <- read_delim("data/references/sample_sizes.csv", delim = ";")

for (i in 1:nrow(samplesizes)) {
  
  cntr <- samplesizes[i, "country_name_short"][[1]]
  
  print(cntr)
  
  noforest_n <- samplesizes[i, "noforest_n"][[1]]
  
  plots <- create_plots(cntr, noforest_n)
  
  landtredr_path <- "/media/homedrive/data/Public/LandTrendr" # Path to landtrednr runs (stored in folder with naming 'landtrendr_[country]')
  
  landtrendr_extract <- c("B1", "B2", "B3", "B4", "B5", "B7", "NBR", "TCB", "TCG", "TCW") %>%
    map(~ extract_landtrendr(cntr, ., plots)) %>% 
    map(~ set_names(., c("Forest", "Noforest"))) %>%
    map(bind_rows, .id = "landcover")
  
  plotid <- landtrendr_extract[[1]]$plotid
  landcover <- landtrendr_extract[[1]]$landcover
  
  landtrendr_extract <- landtrendr_extract %>%
    map(~ dplyr::select(., -plotid, -landcover)) %>%
    bind_cols() %>%
    mutate(., plotid = plotid, landcover = landcover)
  
  coords1 <- st_coordinates(plots[[1]]) %>%
    as.data.frame %>%
    set_names(c("x_coord", "y_coord"))
  
  coords2 <- st_coordinates(plots[[2]]) %>%
    as.data.frame %>%
    set_names(c("x_coord", "y_coord"))
  
  coords <- rbind(coords1, coords2)
  
  data_extract <- cbind(coords, landtrendr_extract)
  
  write_csv(data_extract, paste0("data/model_input/model_input_", cntr, ".csv"))
  
}



