
library(tidyverse)
library(raster)
library(randomForest)

tiles <- list.files("/media/homedrive/data/Public/LandTrendr/greatest_change/landtrendr_european_russia/", 
                               pattern =glob2rx("*NBR*.tif")) %>%
  gsub("landtrendr_greatest_change_image_european_russia_NBR-", "", .) %>%
  gsub(".tif", "", .)

for (t in tiles) {
  
  landtrednr_stack <- list.files("/media/homedrive/data/Public/LandTrendr/greatest_change/landtrendr_european_russia", 
                                 pattern =glob2rx(paste0("*", t, "*.tif")),
                                 full.names = TRUE) %>%
    .[c(5:7, 9)] %>%
    stack(.)
  
  names(landtrednr_stack) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), 
                                             c(paste0("B", c(5, 7)), "NBR", "TCW"), 
                                             paste, sep = "."))
  
  if (sum(raster::unique(subset(landtrednr_stack, 1))) > 0) {
    
    xy_coords_ras <- subset(landtrednr_stack, 1)
    xy_coords <- xyFromCell(xy_coords_ras, cell = 1:ncell(xy_coords_ras))
    xy_coords_ras_x <- xy_coords_ras
    values(xy_coords_ras_x) <- xy_coords[, 1]
    xy_coords_ras_y <- xy_coords_ras
    values(xy_coords_ras_y) <- xy_coords[, 2]
    xy_coords_ras <- stack(xy_coords_ras_x, xy_coords_ras_y)
    names(xy_coords_ras) <- c("x_coord", "y_coord")
    
    prediction_input <- stack(landtrednr_stack, xy_coords_ras)
    
    load("data/models/randomforest_update.RData")
    
    # Predict disturbance/stable/noforest
    
    print(paste0("...predicting disturbances..."))
    
    # ncores <- ifelse(ncell(prediction_input) < 200000000, 20, 
    #                  ifelse(ncell(prediction_input) < 700000000, 10, 
    #                         ifelse(ncell(prediction_input) < 1000000000, 5, 2)))
    # 
    # beginCluster(n = ncores)
    # prediction_disturbance_forest_noforest <- clusterR(prediction_input, predict, args = list(model = fit), progress = "text")
    # endCluster() 
    
    prediction_disturbance_forest_noforest <- raster::predict(prediction_input, fit)
    
  }
  
}

