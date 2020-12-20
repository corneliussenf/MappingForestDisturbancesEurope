
# Libraries and setting ---------------------------------------------------

library(tidyverse)
library(randomForest)
library(tidyverse)
library(gdalUtils)
library(rgeos)
library(raster)
library(landscapemetrics)
library(doParallel)
library(foreach)

rasterOptions(tmptime = 35, tmpdir = "temp/")


# Create mosaics for prediction -------------------------------------------

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

landtredr_path <- "/media/homedrive/data/Public/LandTrendr/greatest_change" # Path to landtrendr runs (stored in folder with naming 'landtrendr_[country]')
habitat_metrics_path <- "/media/homedrive/data/Public/LandTrendr/habitat_metrics" # Path to habitat metrics (stored in folder with naming 'habitat_metrics_[country]_[year]')

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

band_indices <- c(paste0("B", c(1:5, 7)), "NBR", "TCB", "TCG", "TCW")

for (cntr in unique(model_inp$country)) {
  
  dir.create(paste0("data/landtrendr_mosaics/", cntr), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("data/habitat_metrics_mosaics/", cntr), showWarnings = FALSE, recursive = TRUE)
  
  print(paste("Running mosaicing for", cntr))
  
  # Fix country polygons
  
  outline <- shapefile(paste0("data/countries/", cntr, ".shp"))
  outline <- gBuffer(outline, 0, byid = TRUE)
  rgdal::writeOGR(outline, "data/countries", cntr, "ESRI Shapefile", overwrite_layer = TRUE)
  
  ### LandTrendr
  
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
  
  # Habitat Metrics
  
  c("1984-1986", "2016-2018") %>%
    map(~ list.files(habitat_metrics_path, pattern = glob2rx(paste0("*", cntr, "*", ., "*.tif")), recursive = TRUE, full.names = TRUE)) %>%
    map2(.y = c("1984-1986", "2016-2018"),
         ~ gdalwarp(srcfile = .,
                    dstfile = paste0("data/habitat_metrics_mosaics/habitat_metrics_", cntr, "_", .y, ".tif"),
                    t_srs = "EPSG:3035",
                    tr = c(30, 30),
                    output_Raster = FALSE,
                    overwrite = TRUE,
                    verbose = TRUE,
                    cutline = paste0("LandTrendr/countries/", cntr, ".shp"),
                    crop_to_cutline = TRUE,
                    dstnodata = "-32768"))
  
}


# Train random forest model -----------------------------------------------

samplesizes <- read_delim("data/references/sample_sizes.csv", delim = ";")
samplesizes <- samplesizes[-((nrow(samplesizes) - 1):nrow(samplesizes)), ]

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

disturbances_ref <- read_csv("data/references/disturbances.csv")

# Filter long term declines starting in 1985 in Austria

cer_remove <- disturbances_ref %>%
  filter(country == "austria" & 
           year_disturbance_1 == 1985 & 
           is.na(year_disturbance_2) & 
           is.na(year_disturbance_3) & 
           agent_disturbance_1 != "Harvest" |
           country %in% c("austria", "czechia", "germany", "poland", "slovakia", "switzerland") & 
           year_disturbance_1 >= 2017 & 
           is.na(year_disturbance_2) & 
           is.na(year_disturbance_3))

disturbances_ref <- disturbances_ref %>% anti_join(cer_remove)

countries_processed <- intersect(disturbances_ref$country, model_inp$country)

model_inp <- model_inp %>%
  filter(country %in% countries_processed) %>%
  split(.$country) %>%
  map2(.y = disturbances_ref %>% filter(country %in% countries_processed) %>% split(.$country),
       ~ filter(.x, plotid %in% c(.y$plotid, 99999999))) %>%
  bind_rows() %>%
  left_join(disturbances_ref, by = c("plotid", "country")) %>%
  mutate(class = landcover) %>%
  mutate(class = case_when(
    disturbance_n > 0 & !is.na(disturbance_n) ~ "Disturbance",
    TRUE ~ class
  ))

# Match period of reference data with period of LandTrendr

model_inp <- model_inp %>%
  filter(
    
    # Filter period 1986:2016 for Central Europe (i.e., data from Senf et al. 2018)
    
    country %in% c("austria", "czechia", "germany", "poland", "slovakia", "switzerland") &
      year.B5 %in% 1985:2016 & year.B7 %in% 1985:2016 &
      year.NBR %in% 1985:2016 & year.TCW %in% 1985:2016 |
      
      # Filter period 1986:2018 for rest of Europe
      
      country %in% countries[!(countries %in% c("austria", "czechia", "germany", "poland", "slovakia", "switzerland"))] &
      year.B5 %in% 1985:2018 & year.B7 %in% 1985:2018 &
      year.NBR %in% 1985:2018 & year.TCW %in% 1985:2018 |
      
      # And include non-disturbance plots
      
      year.B5 == 0 | year.B7 == 0 | 
      year.NBR == 0 | year.TCW == 0)

model_inp %>%
  filter(class != "Noforest") %>%
  group_by(country) %>%
  summarize(min = min(year_disturbance_1, na.rm = TRUE),
            max = max(year_disturbance_1, na.rm = TRUE))

# Filter Noforest pixels that were sampled outside country areas (water, etc.) and that are 0 (nodata) as this skewes the accuracy assessment

model_inp <- model_inp %>% filter_at(.vars = vars(TCG.1985:TCW.2017), all_vars(. != 0))

table(model_inp$country, model_inp$class)

write_csv(model_inp %>% dplyr::select(x_coord, y_coord, class), "temp/refs.csv")

# Fit model

predictors <-  model_inp %>% 
  dplyr::select(year.B5:dsnr.TCW) %>%
  names() %>%
  grep("year", ., invert = TRUE, value = TRUE)

print(predictors)

model_inp$class <- factor(model_inp$class)

table(model_inp$class)

fit <- randomForest(as.formula(paste0("class ~", paste(predictors, collapse = "+"), " + x_coord + y_coord")),
                    data = model_inp,
                    ntree = 500)

save(fit, file = "data/models/randomforest_update.RData")

conf <- t(fit$confusion[, -4])

conf

1 - diag(conf) / rowSums(conf) # Comission error
1 - diag(conf) / colSums(conf) # Omission error
sum(diag(conf)) / sum(conf) # Overall accuracy

# Prediction --------------------------------------------------------------

# Set minimum mapping unit

mmu <- 3

# Get coutries to predict

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

countries_to_predict <- unique(model_inp$country)

# Predict noforest-forest-disturbance

countries_to_predict_sizes <- list.files("data/landtrendr_mosaics", "B1", recursive = TRUE, full.names = TRUE) %>%
  file.info(.) %>%
  data.frame(country = countries_to_predict, size = .$size) %>%
  arrange(., size)

countries_to_predict_tmp <- as.character(countries_to_predict_sizes[c(14:16, 18:26, 28:35), "country"])

extent_problem <- c("poland", "portugal", "serbia")

countries_to_predict_tmp <- countries_to_predict_tmp[!(countries_to_predict_tmp %in% extent_problem)]

for(cntr in countries_to_predict_tmp) {
  
  print(paste0("Processing ", cntr, "..."))
  
  # Load data
  
  landtrednr_stack <- list.files(paste0("data/landtrendr_mosaics/", cntr, "/"), pattern = "*.tif", full.names = TRUE) %>%
    stack(.)
  
  names(landtrednr_stack) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), 
                                             c(paste0("B", c(1:5, 7)), "NBR", "TCB", "TCG", "TCW"), 
                                             paste, sep = "."))
  
  landtrednr_stack <- subset(landtrednr_stack, as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), 
                                                               c(paste0("B", c(5, 7)), "NBR", "TCW"), 
                                                               paste, sep = ".")))
  
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
  
  ncores <- ifelse(ncell(prediction_input) < 200000000, 20, 
                   ifelse(ncell(prediction_input) < 700000000, 10, 
                          ifelse(ncell(prediction_input) < 1000000000, 5, 2)))
  
  beginCluster(n = ncores)
  prediction_disturbance_forest_noforest <- clusterR(prediction_input, predict, args = list(model = fit), progress = "text")
  endCluster()
  
  dir.create(paste0("results/update/", cntr), recursive = TRUE, showWarnings = FALSE)
  
  if (cntr %in% extent_problem) {
    
    writeRaster(prediction_disturbance_forest_noforest, paste0("results/prediction/", cntr, "/raw_prediction_", cntr, "_TMP.tif"), datatype = "INT1U", overwrite = TRUE)
    
    prediction_disturbance_forest_noforest <- gdalwarp(srcfile = paste0("results/prediction/", cntr, "/raw_prediction_", cntr, "_TMP.tif"),
                                                       dstfile = paste0("results/prediction/", cntr, "/raw_prediction_", cntr, ".tif"),
                                                       t_srs = "EPSG:3035",
                                                       tr = c(30, 30),
                                                       output_Raster = TRUE,
                                                       overwrite = TRUE,
                                                       verbose = TRUE,
                                                       cutline = paste0("data/countries/", cntr, ".shp"),
                                                       crop_to_cutline = TRUE,
                                                       dstnodata = "0")
    
    file.remove(paste0("results/prediction/", cntr, "/raw_prediction_", cntr, "_TMP.tif"))
    
  } else {
    
    writeRaster(prediction_disturbance_forest_noforest, paste0("results/update/", cntr, "/raw_prediction_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
    
  }
  
  # Apply minimum mapping unit
  
  print(paste0("...filtering by mmu..."))
  
  disturbance_map <- reclassify(prediction_disturbance_forest_noforest,
                                matrix(c(NA, NA, 0, 0, 1, 1, 2, 0, 3, 0), byrow = TRUE, ncol = 2))
  
  disturbance_map_patches <- get_patches(disturbance_map, directions = 8, class = 1)
  disturbance_map_patches <- disturbance_map_patches$`1`
  
  disturbance_map_patches_freq <- freq(disturbance_map_patches)
  
  reclass_matrix <- cbind(disturbance_map_patches_freq[, 1], ifelse(disturbance_map_patches_freq[, 2] < mmu, NA, disturbance_map_patches_freq[, 1]))
  
  disturbance_map_patches <- reclassify(disturbance_map_patches, reclass_matrix)
  disturbance_map <- mask(disturbance_map, disturbance_map_patches)
  
  # Year of disturbance
  
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
  
  writeRaster(year, paste0("results/update/", cntr, "/disturbance_year_update_", mmu, "px_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  
}

# Update previous map with newest disturbances ----------------------------

# Settings

update_years <- 2017:2020

mmu <- 3

base_version <- "version1.0"

update_version <- "version1.1"

# Get coutries to update

countries_update <- list.files("results/update")

skip <- list.files("results/version1.1")

for (cntr in countries_update[!(countries_update %in% skip)]) {
  
  print(paste0(cntr, "..."))
  
  year <- raster(paste0("results/update/", cntr, "/disturbance_year_update_", mmu, "px_", cntr, ".tif"))
  
  year_min <- minValue(year)
  year_max <- maxValue(year)
  
  rcl_update <- matrix(c(year_min:year_max, 
                       ifelse((year_min:year_max) %in% update_years, (year_min:year_max), NA)), 
                     nrow = length(year_min:year_max))
  
  year_update <- reclassify(year, rcl_update)
  
  year_base <- raster(paste0("results/", base_version, "/", cntr, "/disturbance_year_filtered_", cntr, ".tif"))
  
  rcl_base <- matrix(c(year_min:year_max, 
                       ifelse((year_min:year_max) %in% update_years, NA, (year_min:year_max))), 
                     nrow = length(year_min:year_max))
  
  year_base <- reclassify(year_base, rcl_base)
  
  if (extent(year_base) != extent(year_update)) {
    
    year_update <- crop(year_update, year_base)
    
    if (extent(year_base) != extent(year_update)) {
      
      year_update <- projectRaster(year_update, to = year_base)
    
    }
  
  }
  
  update <- max(stack(year_base, year_update), na.rm = TRUE)
  
  ### TODO! ###
  #
  # Mask by forest mask to avoid disturbances in water areas (e.g., Switzerland, Netherlands) and other non-treed areas!
  #
  ###
  
  ### TODO! ###
  #
  # Apply spatial filters before updating to avoid double-filtering of old disturbances
  #
  ###
  
  dir.create(paste0("results/", update_version, "/", cntr), recursive = TRUE, showWarnings = FALSE)
  
  writeRaster(update, paste0("results/", update_version, "/", cntr, "/disturbance_year_", cntr, ".tif"), overwrite = TRUE, datatype = "INT2U")
  
}

# Spatial filtering -------------------------------------------------------





