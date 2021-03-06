
library(tidyverse)
library(raster)

options(rasterMaxMemory = 1e10)
options(rasterTmpTime = 1)

# Disturbance year and severity

dist_years <- list.files("results/version1.1", pattern = "tif$", recursive = TRUE, full.names = TRUE) %>%
  grep("year", ., value = TRUE)

# dist_severities <- list.files("results/maps", pattern = "tif$", recursive = TRUE, full.names = TRUE) %>%
#   grep("severity", ., value = TRUE) %>%
#   grep("unmasked", ., value = TRUE, invert = TRUE)

forest <- list.files("results/version1.0/", pattern = "tif$", recursive = TRUE, full.names = TRUE) %>%
  grep("forest", ., value = TRUE)

for (i in 1:length(dist_years)) {
  
  cntr <- strsplit(dist_years[i], "/")[[1]][3]
  
  print(cntr)
  
  dir.create(paste0("temp/upload/", cntr), recursive = TRUE)
  
  file.copy(dist_years[i], paste0("temp/upload/", cntr, "/disturbance_year_1986-2020_", cntr, ".tif"))
  file.copy(forest[i], paste0("temp/upload/", cntr, "/forestcover_", cntr, ".tif"))
  
  zip(zipfile = paste0("temp/upload/", cntr), files = paste0("temp/upload/", cntr))
  
  # map <- raster(dist_years[i])
  
  # sev <- raster(dist_severities[i])
  # sev <- mask(sev, map)
  # sev <- sev * 100
  
  # forst <- raster(forest[i])
  # forst <- reclassify(forst, rcl = matrix(c(0, 1, NA, 1), ncol = 2))
  
  # writeRaster(map, paste0("upload/", cntr, "/disturbance_year_1986-2016_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  # writeRaster(sev, paste0("upload/", cntr, "/disturbance_severity_1986-2016_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  # writeRaster(forst, paste0("upload/", cntr, "/forestcover_", cntr, ".tif"), datatype = "INT2U", overwrite = TRUE)
  

}
