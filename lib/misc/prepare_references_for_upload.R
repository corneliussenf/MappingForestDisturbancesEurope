
library(tidyverse)
library(sf)

### Full sample

countries <- list.files("data/plots", pattern = "*shp$") %>%
  gsub("samples_", "", .) %>%
  gsub(".shp", "", .) %>%
  gsub("_full", "", .)

reference_plots <- list.files("data/plots", pattern = "*shp$", full.names = TRUE) %>%
  map(read_sf) %>%
  map2(.y = countries, ~ mutate(.x, country = .y)) %>%
  map(., ~ mutate(., plotid2 = NULL)) %>%
  do.call("rbind", .)

references <- read_csv("data/references/disturbances.csv")

dat <- reference_plots %>%
  right_join(references)

xy <- st_coordinates(dat)

dat$x_coord <- xy[, 1]
dat$y_coord <- xy[, 2]

dat <- dat %>%
  st_drop_geometry(.) %>%
  dplyr::select(plotid, country, x_coord, y_coord, 
                disturbance_n, 
                year_disturbance_1, year_disturbance_2, year_disturbance_3, 
                agent_disturbance_1, agent_disturbance_2, agent_disturbance_3, 
                severity_disturbance_1, severity_disturbance_2, severity_disturbance_3)
  
write_csv(dat, "data/references/disturbances_references_europe.csv")
  
