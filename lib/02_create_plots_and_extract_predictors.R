
### Libraries

library(tidyverse)
library(raster)

### Function

create_plots <- function(cntr, noforest_n) {
  
  ### Get plot data
  
  plots <- shapefile(list.files("data/plots", glob2rx(paste0("*", cntr, "*.shp")), full.names = TRUE))
  
  ### Load forest mask and sample n no-forest pixels
  
  forest_mask <- raster(paste0("data/forestmasks/forestmask_", cntr, ".tif"))
  
  cntr_shapefile <- shapefile(paste0("data/countries/", cntr, ".shp"))
  
  forest_mask <- mask(forest_mask, cntr_shapefile)
  
  forest_mask_sample <- sampleRandom(forest_mask, noforest_n, na.rm = TRUE, sp = TRUE)
  forest_mask_sample_noforest <- subset(forest_mask_sample, forest_mask_sample@data[, 1] == 0)
  
  while (nrow(forest_mask_sample_noforest@data) < noforest_n) {
    forest_mask_sample <- sampleRandom(forest_mask, noforest_n, na.rm = TRUE, sp = TRUE)
    forest_mask_sample_noforest <- rbind(forest_mask_sample_noforest, subset(forest_mask_sample, forest_mask_sample@data[, 1] == 0))
  }
  
  forest_mask_sample_noforest <- forest_mask_sample_noforest[1:noforest_n, ]
  
  # Return
  
  return(list(plots, forest_mask_sample_noforest))
  
}

extract_landtrendr <- function(cntr, band, plots) {
  
  ### Load LandTrendr outputs
  
  landtrendr_change_images <- list.files("LandTrendr/landtrendr_runs/", pattern = glob2rx(paste0("*", cntr, "*", band, "*.tif")), recursive = TRUE, full.names = TRUE) %>%
    map(stack)
  
  # Extract LandTrendr
  
  landtrendr_extract <- landtrendr_change_images %>% 
    map(~ raster::extract(., plots[[1]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), band, paste, sep = ".")))) %>%
    map(~ mutate(., plotid = plots[[1]]$plotid)) %>%
    map(na.omit) %>%
    bind_rows()
  
  landtrendr_extract_forest <- landtrendr_change_images %>% 
    map(~ raster::extract(., plots[[2]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), band, paste, sep = ".")))) %>%
    map(~ mutate(., plotid = 99999999)) %>%
    map(na.omit) %>%
    bind_rows()
  
  return(list(landtrendr_extract, landtrendr_extract_forest))
  
}

extract_habitat_metrics <- function(cntr, plots) {
   
  ### 1984-1986
  
  habitat_metrics_start <- list.files("LandTrendr/habitat_metrics/", pattern = glob2rx(paste0("*", cntr, "*1984*", "*.tif")), recursive = TRUE, full.names = TRUE) %>%
    map(stack) %>%
    map(~ if (nlayers(.) == 14) {subset(., c(2, 8, 11))} else if (nlayers(.) == 3) {subset(., c(2, 1, 3))})
  
  habitat_metrics_extract_start <- habitat_metrics_start %>% 
    map(~ raster::extract(., plots[[1]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., paste(c("TCG", "TCB", "TCW"), "1985", sep = "."))) %>%
    map(~ mutate(., plotid = plots[[1]]$plotid)) %>%
    map(na.omit) %>%
    bind_rows()
  
  habitat_metrics_extract_start_forest <- habitat_metrics_start %>% 
    map(~ raster::extract(., plots[[2]])) %>%
    map(~ as.data.frame(.)) %>%
    map(~ setNames(., paste(c("TCG", "TCB", "TCW"), "1985", sep = "."))) %>%
    map(~ mutate(., plotid = 99999999)) %>%
    map(na.omit) %>%
    bind_rows()
  
  # ### 2016-2018
  # 
  # habitat_metrics_end <- list.files("LandTrendr/habitat_metrics/", pattern = glob2rx(paste0("*", cntr, "*2018*", "*.tif")), recursive = TRUE, full.names = TRUE) %>%
  #   map(stack) %>%
  #   map(~ if (nlayers(.) == 14) {subset(., c(2, 8, 11))} else if (nlayers(.) == 3) {subset(., c(2, 1, 3))})
  # 
  # habitat_metrics_extract_end <- habitat_metrics_end %>%
  #   map(~ raster::extract(., plots[[1]])) %>%
  #   map(~ as.data.frame(.)) %>%
  #   map(~ setNames(., paste(c("TCG", "TCB", "TCW"), "2017", sep = "."))) %>%
  #   map(~ mutate(., plotid = plots[[1]]$plotid)) %>%
  #   map(na.omit) %>%
  #   bind_rows()
  # 
  # habitat_metrics_extract_end_forest <- habitat_metrics_end %>%
  #   map(~ raster::extract(., plots[[2]])) %>%
  #   map(~ as.data.frame(.)) %>%
  #   map(~ setNames(., paste(c("TCG", "TCB", "TCW"), "2017", sep = "."))) %>%
  #   map(~ mutate(., plotid = 99999999)) %>%
  #   map(na.omit) %>%
  #   bind_rows()
  
  ### Combine
  
  # habitat_metrics_extract <- cbind(habitat_metrics_extract_start, habitat_metrics_extract_end)
  # habitat_metrics_extract_forest <- cbind(habitat_metrics_extract_start_forest, habitat_metrics_extract_end_forest)
  
  habitat_metrics_extract <- habitat_metrics_extract_start
  habitat_metrics_extract_forest <- habitat_metrics_extract_start_forest
  
  return(list(habitat_metrics_extract, habitat_metrics_extract_forest))
  
}

### Do the extraction

samplesizes <- read_delim("data/references/sample_sizes.csv", delim = ";")

processed1a <- list.files("data/habitat_metrics", pattern = glob2rx("*2016-2018*.tif$")) %>%
  str_split(., "_") %>%
  map(~ .[[3]]) %>%
  unlist(.) %>%
  unique(.)

processed1b <- list.files("data/habitat_metrics", pattern = glob2rx("*2018-2018*.tif$")) %>%
  str_split(., "_") %>%
  map(~ .[[3]]) %>%
  unlist(.) %>%
  unique(.)

processed1 <- c(processed1a, processed1b)

processed2 <- list.files("data/habitat_metrics", pattern = glob2rx("*1984-1986*.tif$")) %>%
  str_split(., "_") %>%
  map(~ .[[3]]) %>%
  unlist(.) %>%
  unique(.)

processed <- intersect(processed1, processed2)

processed <- sort(processed)

processed_model_inp <- list.files("LandTrendr/model_input/", ".csv$") %>%
  str_split(., "_") %>%
  map(~ .[[3]]) %>%
  unlist(.) %>%
  gsub(".csv", "", .)

not_processed <- processed[!(processed %in% processed_model_inp)]

for (i in 1:nrow(samplesizes)) {
  
  cntr <- samplesizes[i, "country_name_short"][[1]]
  
  if (cntr %in% not_processed) {
    
    print(cntr)
       
    noforest_n <- samplesizes[i, "noforest_n"][[1]]
    
    plots <- create_plots(cntr, noforest_n)
    
    landtrendr_extract <- c("B5", "B7", "NBR", "TCW") %>%
      map(~ extract_landtrendr(cntr, ., plots)) %>% 
      map(~ set_names(., c("Forest", "Noforest"))) %>%
      map(bind_rows, .id = "landcover")
    
    plotid <- landtrendr_extract[[1]]$plotid
    landcover <- landtrendr_extract[[1]]$landcover
    
    landtrendr_extract <- landtrendr_extract %>%
      map(~ dplyr::select(., -plotid, -landcover)) %>%
      bind_cols() %>%
      mutate(., plotid = plotid, landcover = landcover)
    
    habitat_extract <- extract_habitat_metrics(cntr, plots) %>% 
      bind_rows(., .id = "landcover")
    
    coords1 <- plots[[1]]@coords %>%
      as.data.frame %>%
      set_names(c("x_coord", "y_coord"))
    
    coords2 <- plots[[2]]@coords %>%
      as.data.frame %>%
      set_names(c("x_coord", "y_coord"))
    
    coords <- rbind(coords1, coords2)
    
    data_extract <- cbind(coords, landtrendr_extract, habitat_extract %>% dplyr::select(., -plotid, -landcover))
    
    write_csv(data_extract, paste0("LandTrendr/model_input/model_input_", cntr, ".csv"))
     
  }
  
}



