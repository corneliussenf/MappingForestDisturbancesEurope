
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(randomForest)
library(tidyverse)
library(gdalUtils)
library(rgeos)
library(raster)
library(landscapemetrics)
library(doParallel)
library(foreach)

# Settings ----------------------------------------------------------------

# Raster options
rasterOptions(tmptime = 35, tmpdir = "temp/")

# LandTrendr path
landtredr_path <- "/media/homedrive/data/Public/LandTrendr/greatest_change" # Path to landtrendr runs (stored in folder with naming 'landtrendr_[country]')

# Set minimum mapping unit
mmu <- 3

# update periods
update_years <- 2017:2020

# Versioning
base_version <- "version1.0"
update_version <- "version1.1"

# Create mosaics for prediction -------------------------------------------

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

band_indices <- c(paste0("B", c(5, 7)), "NBR", "TCW")

for (cntr in unique(model_inp$country)) {
  
  dir.create(paste0("data/landtrendr_mosaics/", cntr), showWarnings = FALSE, recursive = TRUE)
  
  print(paste("Running mosaiking for", cntr))
  
  # Fix country polygons
  
  outline <- shapefile(paste0("data/gis/countries/", cntr, ".shp"))
  outline <- gBuffer(outline, 0, byid = TRUE)
  rgdal::writeOGR(outline, "data/gis/countries", cntr, "ESRI Shapefile", overwrite_layer = TRUE)
  
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
               cutline = paste0("data/gis/countries/", cntr, ".shp"),
               crop_to_cutline = TRUE,
               dstnodata = "-32768")
      
      print("   ... fisnished!")
      
    } else {
      
      print("   ... skipping as already processed!")
      
    }
    
  }
  
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

save(fit, file = "data/models/randomforest_add_countries.RData")

conf <- t(fit$confusion[, -4])

conf

1 - diag(conf) / rowSums(conf) # Comission error
1 - diag(conf) / colSums(conf) # Omission error
sum(diag(conf)) / sum(conf) # Overall accuracy

# Prediction --------------------------------------------------------------

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

countries_to_predict_sizes <- list.files("data/landtrendr_mosaics", "NBR", recursive = TRUE, full.names = TRUE) %>%
  file.info(.) %>%
  data.frame(country = countries_to_predict, size = .$size) %>%
  arrange(., size)

countries_to_predict_tmp <- as.character(countries_to_predict_sizes[c(30), "country"])

for(cntr in countries_to_predict_tmp) {
  
  print(paste0("Processing ", cntr, "..."))
  
  # Load data
  
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
  
  writeRaster(prediction_disturbance_forest_noforest, paste0("results/update/", cntr, "/raw_prediction_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
  #prediction_disturbance_forest_noforest <- raster(paste0("results/update/", cntr, "/raw_prediction_", cntr, ".tif"), datatype = "INT1U", overwrite = TRUE)
  
  # Apply minimum mapping unit
  
  print(paste0("...filtering by mmu..."))
  
  beginCluster(n = 20)
  disturbance_map <- clusterR(prediction_disturbance_forest_noforest, reclassify, 
                              args = list(rcl = matrix(c(NA, NA, 0, 0, 1, 1, 2, 0, 3, 0), byrow = TRUE, ncol = 2)), 
                              progress = "text")
  endCluster()
  
  disturbance_map_patches <- clump(disturbance_map, directions = 8)
  
  disturbance_map_patches_freq <- freq(disturbance_map_patches)
  
  reclass_matrix <- cbind(disturbance_map_patches_freq[, 1], ifelse(disturbance_map_patches_freq[, 2] < mmu, NA, disturbance_map_patches_freq[, 1]))
  
  beginCluster(n = 20)
  disturbance_map_patches <- clusterR(disturbance_map_patches, reclassify, 
                                      args = list(rcl = reclass_matrix), 
                                      progress = "text")
  endCluster()
  
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
  
  beginCluster(n = 20)
  year <- clusterR(disturbance_years, calc, args = list(fun = calculate_mode), progress = "text")
  endCluster()
  
  ### Write our final disturbance map
  
  writeRaster(year, paste0("results/update/", cntr, "/disturbance_year_update_", mmu, "px_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  
}
