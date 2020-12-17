
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(raster)
library(landscapemetrics)
library(doParallel)
library(foreach)

# Settings ----------------------------------------------------------------

rasterOptions(tmptime = 35, tmpdir = "temp/")

mmu <- 3

# Do predictions for each country -----------------------------------------

### Get coutries to predict

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

countries_to_predict <- unique(model_inp$country)

#### Predict noforest-forest-disturbance

countries_to_predict_tmp <- countries_to_predict[1:35]

for(cntr in countries_to_predict_tmp) {
  
  print(paste0("Processing ", cntr, "..."))
  
  ### Load data
  
  landtrednr_stack <- list.files(paste0("data/landtrendr_mosaics/", cntr, "/"), pattern = "*.tif", full.names = TRUE) %>%
    stack(.)
  
  names(landtrednr_stack) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), 
                                             c(paste0("B", c(5, 7)), "NBR", "TCW"), 
                                             paste, sep = "."))
  
  xy_coords_ras <- subset(landtrednr_stack, 1)
  xy_coords <- xyFromCell(xy_coords_ras, cell = 1:ncell(xy_coords_ras))
  xy_coords_ras_x <- xy_coords_ras
  values(xy_coords_ras_x) <- xy_coords[, 1]
  xy_coords_ras_y <- xy_coords_ras
  values(xy_coords_ras_y) <- xy_coords[, 2]
  xy_coords_ras <- stack(xy_coords_ras_x, xy_coords_ras_y)
  names(xy_coords_ras) <- c("x_coord", "y_coord")
  
  prediction_input <- stack(landtrednr_stack, xy_coords_ras)
  
  load("data/models/randomforest_11-04-2020.RData")
  
  ### Predict disturbance/stable/noforest
  
  print(paste0("...predicting disturbances..."))
  
  #prediction_disturbance_forest_noforest <- predict(prediction_input, fit)
  
  ncores <- ifelse(ncell(prediction_input) < 200000000, 20, 
                   ifelse(ncell(prediction_input) < 700000000, 10, 
                          ifelse(ncell(prediction_input) < 1000000000, 5, 2)))
  
  beginCluster(n = ncores)
  prediction_disturbance_forest_noforest <- clusterR(prediction_input, predict, args = list(model = fit), progress = "text")
  endCluster()
  
  dir.create(paste0("results/prediction/", cntr), recursive = TRUE, showWarnings = FALSE)
  
  writeRaster(prediction_disturbance_forest_noforest, paste0("results/prediction/", cntr, "/raw_prediction_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
  
  print(paste0("...predicting forestcover..."))
  
  forestcover <- reclassify(prediction_disturbance_forest_noforest,
                            matrix(c(NA, NA, 0, 0, 1, 1, 2, 1, 3, 0), byrow = TRUE, ncol = 2))
  
  writeRaster(forestcover, paste0("results/prediction/", cntr, "/forestcover_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
  
  ### Apply minimum mapping unit

  #prediction_disturbance_forest_noforest <- raster(paste0("results/prediction/", cntr, "/raw_prediction_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
  
  print(paste0("...filtering by mmu..."))
  
  disturbance_map <- reclassify(prediction_disturbance_forest_noforest,
                                matrix(c(NA, NA, 0, 0, 1, 1, 2, 0, 3, 0), byrow = TRUE, ncol = 2))
  
  disturbance_map_patches <- get_patches(disturbance_map, directions = 8, class = 1)
  disturbance_map_patches <- disturbance_map_patches$`1`
  
  disturbance_map_patches_freq <- freq(disturbance_map_patches)
  
  reclass_matrix <- cbind(disturbance_map_patches_freq[, 1], ifelse(disturbance_map_patches_freq[, 2] < mmu, NA, disturbance_map_patches_freq[, 1]))
  
  disturbance_map_patches <- reclassify(disturbance_map_patches, reclass_matrix)
  disturbance_map <- mask(disturbance_map, disturbance_map_patches)
  
  ### Year of disturbance
  
  print(paste0("...estimating year of disturbance..."))
  
  calculate_mode <- function(x, na.rm = TRUE) {
    if (mean(is.na(x)) < 1) {
      x <- as.integer(x)
      x[x == 1985] <- NA # Filter 1985 disturbances (not reliable as often long-term declines)
      x[x == 0] <- NA
      x <- x[!is.na(x)]
      if (length(x) > 0) {
        ux <- unique(x)
        if (length(ux) < length(x)) {
          return(x[which.max(table(x))])  
        } else {
          return(median(x, na.rm = TRUE))
        }  
      } else {
       return(NA)
      }
    } else (
      return(NA)
    )
  }
  
  disturbance_years <- subset(landtrednr_stack, c("year.B5", "year.B7", "year.NBR", "year.TCW"))
  
  if (!(extent(disturbance_years) == extent(disturbance_map))) {
    disturbance_years <- crop(disturbance_years, disturbance_map)  
  }
  
  disturbance_years <- mask(disturbance_years, disturbance_map)
  
  beginCluster(n = ncores)
  year <- clusterR(disturbance_years, calc, args = list(fun = calculate_mode), progress = "text")
  endCluster()
  
  ### Write our final disturbance map
  
  writeRaster(year, paste0("results/prediction/", cntr, "/disturbance_year_1986-2020_mmu", mmu, "px_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  
}

# Spatial filtering -------------------------------------------------------

countries_to_filter <- unique(model_inp$country)

filtering <- function (x, large = FALSE) {
  
  if (large) {
    
    unique_years <- unique(x)
    unique_years <- sort(na.omit(unique_years))
    
    patch_updater <- 0
    
    dir.create(paste0("LandTrendr/temp/", cntr))
    
    for (y in unique_years) {
      
      map_patches_tmp <- get_patches(x, class = y, direction = 8)
      map_patches_tmp <- map_patches_tmp[[1]] + patch_updater
      writeRaster(map_patches_tmp, paste0("temp/", cntr, "/patches_tmp_", y, ".tif"), datatype = "INT4U", overwrite = TRUE)
      patch_updater <- cellStats(map_patches_tmp, max)
      
    }
    
    map_patches <- list.files(paste0("temp/", cntr, "/"), glob2rx("patches_tmp_*tif"), full.names = TRUE) %>%
      map(raster)
    
  } else {
    
    map_patches <- get_patches(x, direction = 8)
    
  }
  
  map_patches_freq <- map_patches %>%
    map(freq)
  
  for (i in 1:length(map_patches)) {
    map_tmp <- map_patches[[i]]
    map_freq_tmp <- map_patches_freq[[i]]
    reclass_matrix <- map_freq_tmp
    reclass_matrix <- cbind(reclass_matrix[, 1], ifelse(reclass_matrix[, 2] == 1, 999999, reclass_matrix[, 1]))
    map_tmp <- reclassify(map_tmp, reclass_matrix)
    map_patches[[i]] <- map_tmp
  }
  
  rm(map_tmp)
  
  map_filter <- stack(map_patches) %>%
    max(., na.rm = TRUE)
  
  rm(map_patches)
  
  wm <- matrix(1, nrow = 3, ncol = 3)
  wm[2, 2] <- 0
  
  map_ngh <- raster::focal(x, w = wm, fun = function(x, na.rm = TRUE) { if (length(x) > 1) {modal(x, ties = "lowest", na.rm = TRUE)} else {NA}}, na.rm = TRUE)
  
  x[map_filter == 999999] <- map_ngh[map_filter == 999999]
  
  return(x)
  
}

for (cntr in countries_to_filter) {
  
  print(paste0("Filtering ", cntr))
    
  year <- raster(paste0("results/prediction/", cntr, "/prediction_disturbance_year_mmu", mmu, "px_", cntr, ".tif"))
  
  # Determine file-size to decide whether year-by-year processing is necessary
  
  filesize <- file.info(paste0("results/prediction/", cntr, "/prediction_disturbance_year_mmu", mmu, "px_", cntr, ".tif"))$size
  
  # Apply filter
  
  year_filtered_1 <- filtering(year, large = filesize > 20000000)
  
  writeRaster(year_filtered_1, "temp/year_filtered_1_tmp1.tif", overwrite = TRUE)
  #year_filtered_1 <- raster("LandTrendr/temp/year_filtered_1_tmp1.tif")
  
  year_filtered_2 <- filtering(year_filtered_1, large = filesize > 20000000)
  
  # Set remaining 0-values to NA
  
  year_filtered_2 <- reclassify(year_filtered_2, matrix(c(0, 1985:2018, NA, 1985:2018), ncol = 2))
  
  writeRaster(year_filtered_2, "temp/year_filtered_2_tmp1.tif", overwrite = TRUE)
  
  # Re-evaluate MMU
  
  year_filtered_2_binary <- reclassify(year_filtered_2, matrix(c(1985:2018, rep(1, length(1985:2018))), ncol = 2))
  year_filtered_2_patches <- get_patches(year_filtered_2_binary, directions = 4)
  year_filtered_2_patches <- year_filtered_2_patches$`1`
  
  reclass_matrix <- freq(year_filtered_2_patches)
  reclass_matrix <- cbind(reclass_matrix[, 1], ifelse(reclass_matrix[, 2] < mmu, NA, reclass_matrix[, 1]))
  
  year_filtered_2_patches <- reclassify(year_filtered_2_patches, reclass_matrix)
  year_filtered_2 <- mask(year_filtered_2, year_filtered_2_patches)
  
  writeRaster(year_filtered_2, "temp/year_filtered_2_tmp2.tif", overwrite = TRUE)
  
  # Fill holes
  
  year_filtered_2_gapfiller <- focal(year_filtered_2, 
                                     matrix(c(NA, 1, NA, 1, NA, 1, NA, 1, NA), nrow = 3, ncol = 3), 
                                     fun = function(x, ...) {
                                       if (sum(!is.na(x)) == 4) {
                                         return(modal(x, ...))
                                       } else {
                                         return(NA)
                                       }
                                       }, na.rm = TRUE)
  
  writeRaster(year_filtered_2_gapfiller, "temp/year_filtered_2_gapfiller_tmp.tif", overwrite = TRUE)
  
  #year_filtered_2[is.na(year_filtered_2)] <- year_filtered_2_gapfiller[is.na(year_filtered_2)]
  
  year_filtered_2 <- cover(year_filtered_2, year_filtered_2_gapfiller)
  
  # Write to disk
  
  writeRaster(year_filtered_2, paste0("prediction/", cntr, "/disturbance_year_filtered_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  
  removeTmpFiles(h=0)
  unlink(paste0("temp/", cntr), recursive = TRUE)
  
}

#### Extract LandTrendr metrics ####

countries_to_filter <- unique(model_inp$country)

for (cntr in countries_to_filter[12]) {
  
  print(cntr)
  
  landtrednr_stack <- list.files(paste0("landtrendr_mosaics/", cntr, "/"), pattern = "*.tif", full.names = TRUE) %>%
    stack(.)
  
  names(landtrednr_stack) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), c("NBR", "TCW", "B5", "B7"), paste, sep = "."))
  
  mask_ras <- raster(paste0("prediction/", cntr, "/disturbance_year_filtered_", cntr, ".tif"))
  
  landtrednr_stack_disturbance <- mask(landtrednr_stack, mask_ras)
  
  names(landtrednr_stack_disturbance) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), c("NBR", "TCW", "B5", "B7"), paste, sep = "."))
  
  writeRaster(landtrednr_stack_disturbance, paste0("prediction/", cntr, "/disturbance_metrics_", cntr, ".tif"), datatype = "FLT4S", overwrite = TRUE)
  
}

#### Remove mis-classifications for italien po-valley ####

# disturbance_year_filtered_italy <- raster("prediction/italy/unmasked/disturbance_year_filtered_italy.tif")
# disturbance_severity_filtered_italy <- raster("prediction/italy/unmasked/disturbance_severity_italy.tif")
# disturbance_metrics_italy <- stack("prediction/italy/unmasked/disturbance_metrics_italy.tif")
# disturbance_year_italy <- raster("prediction/italy/unmasked/disturbance_year_italy.tif")
# 
# po_mask_corine <- raster("admin/po_mask.tif") 
# po_mask_corine <- resample(po_mask_corine, disturbance_year_filtered_italy, method = "ngb")
# 
# disturbance_year_filtered_italy_masked <- mask(disturbance_year_filtered_italy, po_mask_corine, maskvalue = 1)
# disturbance_severity_filtered_italy_masked <- mask(disturbance_severity_filtered_italy, po_mask_corine, maskvalue = 1)
# disturbance_metrics_italy_masked <- mask(disturbance_metrics_italy, po_mask_corine, maskvalue = 1)
# disturbance_year_italy_masked <- mask(disturbance_year_italy, po_mask_corine, maskvalue = 1)
# 
# writeRaster(disturbance_year_filtered_italy_masked, "prediction/italy/disturbance_year_filtered_italy.tif", datatype = "INT2U")
# writeRaster(disturbance_severity_filtered_italy_masked, "prediction/italy/disturbance_severity_italy.tif", datatype = "INT2U")
# writeRaster(disturbance_metrics_italy_masked, "prediction/italy/disturbance_metrics_italy.tif", datatype = "INT2U", overwrite = TRUE)
# writeRaster(disturbance_year_italy_masked, "prediction/italy/disturbance_year_italy.tif", datatype = "INT2U")


