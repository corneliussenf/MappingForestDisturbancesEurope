
library(sf)
library(tidyverse)
library(patchwork)
library(raster)
library(landscapemetrics)
library(fasterize)

#### Aggregate data to grid/hexagon ####

country_master <- read_delim("data/countries_master.csv", delim = ";") %>%
  filter(country_name_short != "russia")

ending <- "hexagon_50km"

reference_grid <- read_sf(paste0("data/admin/referencegrid/", ending, ".shp"))

ecoregions <- read_sf("data/admin/ecoregions/terrestrial_ecoregions_olson.shp")
ecoregions <- st_transform(ecoregions, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

### Disturbance patches

for (i in 1:nrow(country_master)) {
  
  cntr <- country_master[i, "country_name_short"][[1]]
  
  print(cntr)
  
  disturbance <- raster(paste0("prediction/", cntr, "/disturbance_year_filtered_", cntr, ".tif"))
  severity <- raster(paste0("prediction/", cntr, "/disturbance_severity_", cntr, ".tif"))
  severity_values <- values(severity)
  
  ext <- as(extent(disturbance), 'SpatialPolygons')
  proj4string(ext) <- projection(disturbance)
  grid_sel <- st_intersection(reference_grid, st_as_sf(ext))
  
  grid_sel_ras <- fasterize(grid_sel, disturbance, "id")
  grid_values <- values(grid_sel_ras)
  
  ecoregions_sel <- st_crop(ecoregions, st_as_sf(ext))
  ecoregions_sel <- st_cast(ecoregions_sel)
  ecoregions_sel_ras <- fasterize(ecoregions_sel, disturbance, field = "ECO_NUM")
  ecoregions_values <- values(ecoregions_sel_ras)
  
  dat <- vector("list", length(1986:2018))
  
  k <- 0
  
  for (y in 1986:2018) {
    
    k <- k + 1
    
    print(y)
    
    dist_patches <- get_patches(disturbance, class = y, direction = 8)
    
    dat[[k]] <- data.frame(grid_id = grid_values,
                           patch_id = values(dist_patches[[1]]),
                           severity = severity_values) %>%
      filter(!is.na(patch_id)) %>%
      group_by(grid_id, patch_id) %>%
      summarize(size = n() * 0.09,
                severity = mean(severity)) %>%
      ungroup(.)
    
  }
  
  dat <- dat %>% 
    set_names(c(1986:2018)) %>%
    bind_rows(.id = "year")
  
  write_csv(dat, paste0("results/indicators/indicators_patch_", ending, "_", cntr, ".csv"))
  
}

### Forest cover

for (i in 1:nrow(country_master)) {
  
  cntr <- country_master[i, "country_name_short"][[1]]
  
  print(cntr)
  
  forest <- raster(paste0("prediction/", cntr, "/prediction_forestcover_", cntr, ".tif"))
  forest_values <- values(forest)
  
  ext <- as(extent(forest), 'SpatialPolygons')
  proj4string(ext) <- projection(forest)
  grid_sel <- st_intersection(reference_grid, st_as_sf(ext))
  
  grid_sel_ras <- fasterize(grid_sel, forest, "id")
  grid_values <- values(grid_sel_ras)
  
  dat <- data.frame(grid_id = grid_values,
                    forest = forest_values) %>%
    filter(!is.na(forest)) %>%
    group_by(grid_id) %>%
    summarize(forest_ha = sum(forest == 1, na.rm = TRUE) * 0.09,
              land_ha = n() * 0.09) %>%
    ungroup(.)
  
  write_csv(dat, paste0("results/indicators/forest_land_", ending, "_", cntr, ".csv"))
  
}

### Ecoregion

for (i in 1:nrow(country_master)) {
  
  cntr <- country_master[i, "country_name_short"][[1]]
  
  print(cntr)
  
  disturbance <- raster(paste0("prediction/", cntr, "/disturbance_year_filtered_", cntr, ".tif"))
  ext <- as(extent(disturbance), 'SpatialPolygons')
  proj4string(ext) <- projection(disturbance)
  grid_sel <- st_intersection(reference_grid, st_as_sf(ext))
  
  grid_sel_ras <- fasterize(grid_sel, disturbance, "id")
  grid_values <- values(grid_sel_ras)
  
  ecoregions_sel <- st_crop(ecoregions, st_as_sf(ext))
  ecoregions_sel <- st_cast(ecoregions_sel)
  ecoregions_sel_ras <- fasterize(ecoregions_sel, disturbance, field = "ECO_ID")
  ecoregions_values <- values(ecoregions_sel_ras)
  
  dat <- data.frame(grid_id = grid_values,
                    ecoregions = ecoregions_values) %>%
    filter(!is.na(ecoregions)) %>%
    group_by(grid_id) %>%
    summarize(ecoregions = names(which.max(tableC(ecoregions)))) %>%
    ungroup(.)
  
  
  
  write_csv(dat, paste0("results/indicators/ecoregions_patch_", ending, "_", cntr, ".csv"))
  
}

#### Load data ####

ecoregions <- read_sf("data/admin/ecoregions/terrestrial_ecoregions_olson.shp")
ecoregions <- st_transform(ecoregions, CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

reference_grid <- read_sf("data/admin/referencegrid/hexagon_50km.shp")

countries <- read_sf("data/admin/borders/countries_europe_simplyfied.shp")

country_master <- read_delim("data/countries_master.csv", delim = ";") %>%
  filter(country_name_short != "russia")

names <- list.files("results/indicators", pattern = glob2rx("*indicators*.csv")) %>%
  grep(., pattern = "hexagon", inv = FALSE, value = TRUE) %>%
  strsplit(., "_") %>%
  map(., ~ substring(.[5], 1, nchar(.[5]) - 4)) %>%
  unlist()

dat <- list.files("results/indicators", pattern = glob2rx("*indicators*.csv"), full.names = TRUE) %>%
  grep(., pattern = "hexagon", inv = FALSE, value = TRUE) %>%
  map(read_csv) %>%
  set_names(names) %>%
  bind_rows(.id = "country")

dat <- dat %>% 
  filter(year < 2017) %>%
  mutate(size = ifelse(size == 0.09, 0.18, size))

forest <- list.files("results/indicators", pattern = glob2rx("*forest_land*.csv"), full.names = TRUE) %>%
  grep(., pattern = "hexagon", inv = FALSE, value = TRUE) %>%
  map(read_csv) %>%
  set_names(names) %>%
  bind_rows(.id = "country")

forest_grid <- forest %>%
  group_by(grid_id) %>%
  summarize(forest_ha = sum(forest_ha, na.rm = TRUE),
            land_ha = sum(land_ha, na.rm = TRUE))

forest_country <- forest %>%
  group_by(country) %>%
  summarize(forest_ha = sum(forest_ha, na.rm = TRUE),
            land_ha = sum(land_ha, na.rm = TRUE))

library(Rcpp)
sourceCpp("lib/misc/fasttable.cpp")

ecoregions_files <- list.files("results/indicators", pattern = glob2rx("*ecoregions*.csv"), full.names = TRUE) %>%
  grep(., pattern = "hexagon", inv = FALSE, value = TRUE) %>%
  map(read_csv) %>%
  set_names(names) %>%
  bind_rows(.id = "country")

ecoregions_grid <- ecoregions_files %>%
  group_by(grid_id) %>%
  summarize(ECO_ID = as.integer(names(which.max(tableC(ecoregions)))))

ecoregions_names <- unique(ecoregions %>% st_drop_geometry() %>% dplyr::select(ECO_ID, ECO_NAME))

ecoregions_grid <- ecoregions_grid %>%
  left_join(ecoregions_names)

#### Aggregate to hexagon-grid ####

dat_grid <- dat %>%
  group_by(grid_id, year) %>%
  summarize(n = n(),
            area = sum(size),
            maxsize = quantile(size, 0.99),
            intermedsize = quantile(size, 0.75),
            mediansize = quantile(size, 0.5),
            size = mean(size),
            prop_sr = mean(severity > 0.5),
            severity = mean(severity)) %>%
  arrange(grid_id, year) %>%
  left_join(forest_grid, by = "grid_id") %>%
  left_join(ecoregions_grid, by = "grid_id") %>%
  mutate(rate = area / forest_ha) %>%
  mutate(frequency = n / forest_ha) %>%
  mutate(forestcover = forest_ha / land_ha)
  
selector <- dat_grid %>% 
  group_by(grid_id) %>%
  summarize(yearsdisturbed = sum(n > 0),
            land_ha = unique(land_ha),
            forestcover = unique(forestcover)) %>%
  filter(yearsdisturbed > 10 & land_ha > 0.5 * median(unique(st_area(reference_grid))) / 10000)

dat_grid_summary <- dat_grid %>%
  filter(grid_id %in% selector$grid_id) %>%
  na.omit(.) %>%
  group_by(grid_id) %>%
  summarize(forest_ha = unique(forest_ha),
            n_mean = mean(n),
            frequency_mean = mean(frequency * 100),
            area_mean = mean(area),
            size_mean = mean(size),
            mediansize_mean = mean(mediansize),
            intermedsize_mean = mean(intermedsize),
            maxsize_mean = mean(maxsize),
            prop_sr_mean = mean(prop_sr),
            severity_mean = mean(severity),
            rate_mean = mean(rate),
            frequency_trend = trend::sens.slope(frequency * 100)$estimate / frequency_mean * 100,
            n_trend = trend::sens.slope(n)$estimate / n_mean * 100,
            area_trend = trend::sens.slope(area)$estimate / area_mean * 100,
            size_trend = trend::sens.slope(size)$estimate / size_mean * 100,
            maxsize_trend = trend::sens.slope(maxsize)$estimate / maxsize_mean * 100,
            mediansize_trend = trend::sens.slope(mediansize)$estimate / mediansize_mean * 100,
            intermedsize_trend = trend::sens.slope(intermedsize)$estimate / intermedsize_mean * 100,
            prop_sr_trend = trend::sens.slope(prop_sr)$estimate / prop_sr_mean  * 100,
            severity_trend = trend::sens.slope(severity)$estimate / severity_mean  * 100,
            rate_trend = trend::sens.slope(rate)$estimate / rate_mean * 100)

reference_grid <- read_sf("data/admin/referencegrid/hexagon_50km.shp")
europe_outline <- read_sf("data/admin/borders/countries_europe_outline_simplyfied.shp")
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, projection(reference_grid))
st_crs(world) <- st_crs(reference_grid)
world <- st_crop(world, st_bbox(reference_grid) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(reference_grid)))

reference_grid <- reference_grid %>%
  right_join(dat_grid_summary, by = c("id" = "grid_id"))

### Averages

p_averages <- list(var = c("size_mean",
                           "frequency_mean",
                           "severity_mean"),
     title = c("Average disturbance size",
               "Average disturbance frequency",
               "Average disturbance severity"),
     unit = list(NULL, NULL, NULL)) %>%
  pmap(function(var, title, unit) {
    ggplot() +
      geom_sf(data = world, color = "black", fill = gray(0.99)) +
      geom_sf(data = reference_grid, aes_string(fill = var), col = NA) +
      geom_sf(data = countries, color = gray(0.5), fill = NA, alpha = 0.75, size = 0.25) +
      #geom_sf(data = ecoregions, color = gray(0.01), fill = NA) +
      geom_sf(data = europe_outline, color = gray(0.01), fill = NA, size = 0.5) +
      scale_fill_gradient(low = "white", 
                          high = RColorBrewer::brewer.pal(3, "Set1")[[1]],
                          na.value = gray(0.95)) +
      theme(#panel.grid.major = element_line(color = "black", linetype = "dashed", size = 0.25), 
            panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "lightblue", color = NULL),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            legend.position = "bottom",
            #legend.position = c(0, 1),
            #legend.justification = c(-0.1, 1.3),
            #legend.background = element_rect(color = "black", fill = gray(0.99), size = 0.5),
            legend.direction = "horizontal",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8, vjust = 0.135),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
            plot.title = element_text(size = 9, hjust = 0.5),
            # legend.margin = ggplot2::margin(0.75, 0.75, 0.75, 0.75),
            # legend.box.margin = ggplot2::margin(0.15, 0.15, 0.15, 0.15),
            legend.margin = ggplot2::margin(0.75, 0.75, 0.75, 0.75),
            legend.box.margin = ggplot2::margin(-10, 0.15, 0.15, 0.15)) +
      guides(fill = guide_colorbar(title.position = "right", barwidth = 5, barheight = 0.5)) +
      labs(fill = unit, 
           title = title) +
      coord_sf(expand = FALSE)
  })

ggsave("results/disturbance_regime_average.pdf", p_averages %>%
         wrap_plots(ncol = 3), width = 7, height = 3, device = cairo_pdf)

ggsave("results/disturbance_regime_average.png", p_averages %>%
         wrap_plots(ncol = 3), width = 7, height = 3)

ggplot() +
  geom_sf(data = reference_grid, aes(fill = rate_mean), col = NA) +
  geom_sf(data = countries, color = "black", fill = NA) +
  scale_fill_gradient(low = "white", 
                      high = RColorBrewer::brewer.pal(3, "Set1")[[1]])

# Histrograms

size_distribution <- dat %>%
  group_by(cut = cut(size, c(seq(0, 5, 1), 20000), labels = c(paste0(seq(0, 4, 1), "-", seq(1, 5, 1)), ">5"))) %>%
  summarize(value = mean(size),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

frequency_distribution <- dat_grid %>%
  group_by(cut = cut(frequency * 100, breaks = c(0, 1, 2, 3, 4, 40), labels = c("0-1", "1-2", "2-3", "3-4", ">4"))) %>%
  na.omit() %>%
  summarize(value = mean(frequency),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

severity_distribution <- dat %>%
  group_by(cut = cut(severity, seq(0, 1, 0.2), labels = paste0(seq(0, 0.8, 0.2), "-", seq(0.2, 1, 0.2)))) %>%
  na.omit() %>%
  summarize(value = mean(severity),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

p_averages_hist <-  pmap(list(d = list(size_distribution,
                                       frequency_distribution,
                                       severity_distribution),
                              var = c("size_mean",
                                      "frequency_mean",
                                      "severity_mean"),
                              xlab = c("Hectare", bquote("Patches per km"^2*" forest"), "Probability of stand-replacing"),
                              ylab = list("Proportion of patches", "Proportion of landscapes", "Proportion of patches")),
                         function(d, var, xlab, ylab) {
                           ggplot(d, aes(x = cut, y = prop, fill = as.integer(cut))) +
                             geom_bar(stat = "identity", col = "black") +
                             scale_fill_gradient(low = "white", 
                                                 high = RColorBrewer::brewer.pal(3, "Set1")[[1]],
                                                 na.value = gray(0.95)) +
                             theme_minimal() +
                             theme(panel.grid = element_blank(), 
                                   panel.border = element_blank(),
                                   legend.position = "none",
                                   axis.ticks.y = element_line(),
                                   axis.title = element_text(size = 9),
                                   axis.text = element_text(size = 7)) +
                             labs(x = xlab, y = ylab)
                         })

ggsave("results/disturbance_regime_average_hist.pdf", p_averages_hist %>%
         wrap_plots(ncol = 3), width = 6.75, height = 2, device = cairo_pdf)

ggsave("results/disturbance_regime_average_hist.png", p_averages_hist %>%
         wrap_plots(ncol = 3), width = 6.75, height = 2)

### Trends

p_trends <- list(var = c("size_trend",
                         "frequency_trend",
                         "severity_trend"),
          title = c("Trend in disturbance size",
                    "Trend in disturbance frequency",
                    "Trend in disturbance severity"),
          unit = c(NULL, "Change per year (%)", NULL),
          leg_pos = c("none", "bottom", "none")) %>%
  pmap(function(var, title, unit, leg_pos) {
    ggplot() +
      geom_sf(data = world, color = "black", fill = gray(0.99)) +
      geom_sf(data = reference_grid, aes_string(fill = var), col = NA) +
      geom_sf(data = countries, color = gray(0.5), fill = NA, size = 0.25) +
      geom_sf(data = europe_outline, color = gray(0.01), fill = NA, size = 0.5) +
      scale_fill_gradientn(
        colors = c(RColorBrewer::brewer.pal(3, "Set1")[[2]], "white", RColorBrewer::brewer.pal(3, "Set1")[[1]]),
        values = scales::rescale(c(-4, -3, -2, -1, 0, 1, 2, 4, 8)),
        limits = c(-4, 8)) +
      theme(#panel.grid.major = element_line(color = "black", linetype = "dashed", size = 0.25), 
            panel.grid.major = element_blank(), 
            panel.background = element_rect(fill = "lightblue", color = NULL),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            legend.position = leg_pos,
            #legend.position = c(0, 1),
            #legend.justification = c(-0.1, 1.3),
            #legend.background = element_rect(color = "black", fill = gray(0.99), size = 0.5),
            legend.direction = "horizontal",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8, vjust = 0.135),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
            plot.title = element_text(size = 9, hjust = 0.5),
            # legend.margin = ggplot2::margin(0.75, 0.75, 0.75, 0.75),
            # legend.box.margin = ggplot2::margin(0.15, 0.15, 0.15, 0.15),
            legend.margin = ggplot2::margin(0.75, 0.75, 0.75, 0.75),
            legend.box.margin = ggplot2::margin(-10, 0.15, 0.15, 0.15)) +
      guides(fill = guide_colorbar(title.position = "top", barwidth = 5, barheight = 0.5)) +
      labs(fill = unit, title = title) +
      coord_sf(expand = FALSE)
  })

ggsave("results/disturbance_regime_trend.pdf", p_trends %>%
         wrap_plots(ncol = 3), width = 7, height = 3, device = cairo_pdf)

ggsave("results/disturbance_regime_trend.png", p_trends %>%
         wrap_plots(ncol = 3), width = 7, height = 3)

# Histrograms

p_trends_hist <- pmap(list(var = c("size_trend",
                  "frequency_trend",
                  "severity_trend"),
                  xlab = list("% change per year",
                              "% change per year",
                              "% change per year"),
                  ylab = list("Proportion of forest area", NULL, NULL)),
     function(var, xlab, ylab) {
       reference_grid %>%
         st_drop_geometry() %>%
         mutate_("target" = var) %>%
         group_by("trend" = cut(target, 
                              breaks = c(seq(-15.5, -0.5, 1), seq(0.5, 15.5, 1)), 
                              labels = c(seq(-15, -1, 1), 0, seq(1, 15, 1)))) %>%
         summarize(forest = sum(forest_ha)) %>%
         ungroup() %>%
         mutate(p = forest / sum(forest)) %>%
         ggplot(., aes(x = as.integer(as.character(trend)), y = p, fill = as.integer(as.character(trend)))) +
         geom_bar(stat = "identity", col = "black") +
         scale_fill_gradientn(colors = c(RColorBrewer::brewer.pal(3, "Set1")[[2]], "white", RColorBrewer::brewer.pal(3, "Set1")[[1]]),
                              values = scales::rescale(c(-4, -3, -2, -1, 0, 1, 2, 4, 10)),
                              limits = c(-4, 10)) +
         theme_minimal() +
         theme(panel.grid = element_blank(), 
               panel.border = element_blank(),
               legend.position = "none",
               axis.ticks = element_line(),
               axis.title = element_text(size = 9),
               axis.text = element_text(size = 8)) +
         labs(x = xlab, y = ylab) +
         xlim(-5, 11)
     })

ggsave("results/disturbance_regime_trend_hist.pdf", p_trends_hist %>%
         wrap_plots(ncol = 3), width = 6.25, height = 2, device = cairo_pdf)

ggsave("results/disturbance_regime_trend_hist.png", p_trends_hist %>%
         wrap_plots(ncol = 3), width = 6.25, height = 2)

#### Separate frequency and size trends ####

p <- ggplot(reference_grid, aes(x = frequency_trend, y = rate_trend)) + 
  geom_point(aes(col = size_trend), alpha = 0.25, shape = 19, stroke = NA) + 
  geom_abline(intercept = 0, slope = 1) + 
  scale_colour_gradient2(low = RColorBrewer::brewer.pal(3, "Set1")[2],
                         mid = "grey",
                         high = RColorBrewer::brewer.pal(3, "Set1")[1],
                         limits = c(-5, 5)) +
  theme_minimal() +
  theme(legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(x = "Trend in disturbance frequency (% per year)", 
       y = "Trend in disturbance rate (% per year)", 
       col = "Trend in disturbance size (% per year)") +
  guides(colour = guide_colorbar(title.position = "top", barwidth = 6, barheight = 0.5)) +
  xlim(-5, 11) + ylim(-5, 11)

ggsave("results/frequency_size_rate.pdf", p, width = 3.42, height = 3.42)

ggplot(reference_grid) + 
  geom_point(aes(x = n_trend, y = size_trend, col = rate_trend)) + 
  #geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  geom_segment(x = 0, xend = 0.05 * 100, y = 0, yend = 0, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  geom_segment(x = 0, xend = 0, y = 0, yend = 0.05 * 100, arrow = arrow(type = "closed", length = unit(0.25, "cm"))) +
  scale_colour_gradient2(low = RColorBrewer::brewer.pal(3, "Set1")[2],
                         mid = "grey",
                         high = RColorBrewer::brewer.pal(3, "Set1")[1],
                         limits = c(-8, 8)) +
  xlim(-0.075 * 100, 0.075 * 100) + ylim(-0.075 * 100, 0.075 * 100) +
  #geom_smooth(aes(x = n_trend, y = size_trend), method = "lm", se = FALSE, col = "black", size = 1, arrow = arrow()) +
  theme_minimal() +
  theme(legend.position = c(0, 0),
        legend.justification = c(-0.05, -0.125),
        legend.direction = "horizontal",
        axis.text.y = element_text(size = 10, color = "black")) +
  labs(x = "Trend in disturbance frequency (% per yr.)", y = "Trend in disturbance size (% per yr.)", col = "Trend in disturbance rate (% per yr.)") +
  guides(colour = guide_colorbar(title.position = "top", barwidth = 15))

summary(lm(rate_trend ~ n_trend, data = reference_grid))
summary(lm(rate_trend ~ size_trend, data = reference_grid))
summary(lm(rate_trend ~ size_trend + n_trend, data = reference_grid))

anova(lm(rate_trend ~ size_trend + n_trend, data = reference_grid))

#### Stats ####

### Size

size_distribution <- dat %>%
  #group_by(size_cut = cut(size, seq(0, 100000, 1), labels = paste0(seq(0, 99999, 1), "-", seq(1, 100000, 1)))) %>%
  group_by(size_cut = cut(size, c(seq(0, 5, 1), 20000), labels = c(paste0(seq(0, 4, 1), "-", seq(1, 5, 1)), ">5"))) %>%
  summarize(size = mean(size),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

size_stat <- dat %>%
  summarise(mean = mean(size),
            q0 = min(size, na.rm = TRUE),
            q1 = quantile(size, 0.01, na.rm = TRUE),
            q25 = quantile(size, 0.25, na.rm = TRUE),
            q50 = median(size, na.rm = TRUE),
            q75 = quantile(size, 0.75, na.rm = TRUE),
            q99 = quantile(size, 0.99, na.rm = TRUE),
            q100 = max(size, na.rm = TRUE))

p_size <- ggplot(size_distribution, aes(x = size_cut, y = prop)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(title = "A) Disturbance size",
       x = "hectare", 
       y = "Proportion of patches")

# By country

size_stat_cntr <- dat %>%
  group_by(country) %>%
  summarise(mean = mean(size),
            q0 = min(size, na.rm = TRUE),
            q1 = quantile(size, 0.01, na.rm = TRUE),
            q25 = quantile(size, 0.25, na.rm = TRUE),
            q50 = median(size, na.rm = TRUE),
            q75 = quantile(size, 0.75, na.rm = TRUE),
            q99 = quantile(size, 0.99, na.rm = TRUE),
            q100 = max(size, na.rm = TRUE))

p_size_cntr <- size_stat_cntr %>%
  left_join(country_master, by = c("country" = "country_name_short"))  %>%
  mutate(country_name = case_when(country_name == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                                  TRUE ~ country_name)) %>%
  ggplot() +
  geom_point(aes(x = reorder(country_name, mean), y = mean)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(y = "ha", x = NULL, title = "Mean size")

### Severity

severity_distribution <- dat %>%
  group_by(severity_cut = cut(severity, seq(0, 1, 0.2), labels = paste0(seq(0, 80, 20), "-", seq(20, 100, 20)))) %>%
  na.omit() %>%
  summarize(severity = mean(severity),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

severity_stat <- dat %>%
  summarise(mean = mean(severity, na.rm = TRUE),
            q0 = min(severity, na.rm = TRUE),
            q1 = quantile(severity, 0.01, na.rm = TRUE),
            q25 = quantile(severity, 0.25, na.rm = TRUE),
            q50 = median(severity, na.rm = TRUE),
            q75 = quantile(severity, 0.75, na.rm = TRUE),
            q99 = quantile(severity, 0.99, na.rm = TRUE),
            q100 = max(severity, na.rm = TRUE))

p_severity <- ggplot(severity_distribution, aes(x = severity_cut, y = prop)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(title = "C) Disturbance severity",
       x = "Probability of stand-replacing", 
       y = "Proportion of patches")

# By country

severity_stat_cntr <- dat %>%
  group_by(country) %>%
  summarise(mean = mean(severity, na.rm = TRUE),
            q0 = min(severity, na.rm = TRUE),
            q1 = quantile(severity, 0.01, na.rm = TRUE),
            q25 = quantile(severity, 0.25, na.rm = TRUE),
            q50 = median(severity, na.rm = TRUE),
            q75 = quantile(severity, 0.75, na.rm = TRUE),
            q99 = quantile(severity, 0.99, na.rm = TRUE),
            q100 = max(severity, na.rm = TRUE))

p_severity_cntr <- severity_stat_cntr %>%
  left_join(country_master, by = c("country" = "country_name_short"))  %>%
  mutate(country_name = case_when(country_name == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                                  TRUE ~ country_name)) %>%
  ggplot() +
  geom_point(aes(x = reorder(country_name, mean), y = mean)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(y = "Prob. of stand-replacing", x = NULL, title = "Mean severity")

### Frequency

frequency_distribution <- dat_grid %>%
  #group_by(frequency_cut = cut(frequency * 100, seq(0, 40, 1), labels = paste0(seq(0, 39, 1), "-", seq(1, 40, 1)))) %>%
  group_by(frequency_cut = cut(frequency * 100, breaks = c(0, 1, 2, 3, 4, 40), labels = c("0-1", "1-2", "2-3", "3-4", ">4"))) %>%
  na.omit() %>%
  summarize(frequency = mean(frequency),
            n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

frequency_stat <- dat_grid %>%
  ungroup() %>%
  summarise(mean = mean(frequency * 100, na.rm = TRUE),
            q0 = min(frequency * 100, na.rm = TRUE),
            q1 = quantile(frequency * 100, 0.01, na.rm = TRUE),
            q25 = quantile(frequency * 100, 0.25, na.rm = TRUE),
            q50 = median(frequency * 100, na.rm = TRUE),
            q75 = quantile(frequency * 100, 0.75, na.rm = TRUE),
            q99 = quantile(frequency * 100, 0.99, na.rm = TRUE),
            q100 = max(frequency * 100, na.rm = TRUE))

p_frequency <- ggplot(frequency_distribution, aes(x = frequency_cut, y = prop)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(title = "B) Disturbance frequency",
       x = bquote("Patches per km"^2*" forest"), 
       y = "Proportion of landscapes")

# By country

frequency_stat_cntr <- dat %>%
  group_by(grid_id, year, country) %>%
  summarize(n = n(),
            area = sum(size)) %>%
  arrange(grid_id, year) %>%
  left_join(forest_grid, by = "grid_id") %>%
  mutate(frequency = n / forest_ha) %>%
  group_by(country) %>%
  summarise(mean = mean(frequency * 100, na.rm = TRUE),
            q0 = min(frequency * 100, na.rm = TRUE),
            q1 = quantile(frequency * 100, 0.01, na.rm = TRUE),
            q25 = quantile(frequency * 100, 0.25, na.rm = TRUE),
            q50 = median(frequency * 100, na.rm = TRUE),
            q75 = quantile(frequency * 100, 0.75, na.rm = TRUE),
            q99 = quantile(frequency * 100, 0.99, na.rm = TRUE),
            q100 = max(frequency * 100, na.rm = TRUE))

p_frequency_cntr <- frequency_stat_cntr %>%
  left_join(country_master, by = c("country" = "country_name_short"))  %>%
  mutate(country_name = case_when(country_name == "The Former Yugoslav Republic of Macedonia" ~ "North Macedonia",
                                  TRUE ~ country_name)) %>%
  ggplot() +
  #geom_errorbar(aes(x = reorder(country_name, mean), ymin = q25, ymax = q75), width = 0) +
  geom_point(aes(x = reorder(country_name, mean), y = mean)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(size = 9),
        legend.position = c(0.05, 1),
        legend.justification = c(0, 1),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8)) +
  labs(y = bquote("Patches per km"^2*" forest"), x = NULL, title = "Mean frequency")

### Combine

p <- p_size + p_frequency + p_severity + plot_layout(ncol = 3)

p_cntr <- p_size_cntr + p_frequency_cntr + p_severity_cntr + plot_layout(ncol = 3)

ggsave("results/size_frequency_severity_distributions.pdf", p, width = 7.5, height = 2)
ggsave("results/size_frequency_severity_averages_country.pdf", p_cntr, width = 8, height = 4.5)

list(size = size_stat,
     frequency = frequency_stat,
     severity = severity_stat) %>%
  bind_rows(.id = "indicator") %>%
  write_csv("results/average_stats.csv")

list(size = size_stat_cntr,
     frequency = frequency_stat_cntr,
     severity = severity_stat_cntr) %>%
  bind_rows(.id = "indicator") %>%
  write_csv("results/average_stats_country.csv")

# Trends

dat_grid_summary %>% 
  dplyr::select(size_trend, frequency_trend, severity_trend, maxsize_trend, mediansize_trend, intermedsize_trend, forest_ha) %>%
  mutate(weight = forest_ha / sum(forest_ha)) %>%
  gather(key = key, value = value, -forest_ha, -weight) %>%
  group_by(key) %>% 
  summarize(mean = mean(value),
            mean_weighted = sum(value * weight),
            sd = sd(value),
            se = sd / sqrt(n()),
            median = median(value),
            prop_gt_0 = mean(value > 0),
            prop_0 = mean(value == 0),
            prop_forest_gt_0 = sum(forest_ha[value > 0]) / sum(forest_ha),
            prop_forest_0 = sum(forest_ha[value == 0]) / sum(forest_ha)) %>%
  write_csv("results/trend_stats.csv")

dat_grid_summary %>% 
  dplyr::select(size_trend, frequency_trend, severity_trend, maxsize_trend, mediansize_trend, intermedsize_trend) %>% 
  gather() %>%
  group_by(key) %>%
  ggplot(., aes(x = value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free") +
  geom_vline(xintercept = 0)

#### Changes in patch-sizes ####

patch_freq_rel <- dat %>%
  group_by(size, year) %>%
  summarize(n = n()) %>%
  group_by(year) %>%
  mutate(freq = n / sum(n))

ggplot(patch_freq_rel, aes(x = size, y = freq, col = year, group = year)) +
  geom_point(alpha = 0.15, size = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c() +
  geom_smooth(aes(x = size, y = freq, col = year, group = year), se = FALSE)

patch_freq_rel <- patch_freq_rel %>%
  split(.$year) %>%
  map(~ arrange(., size)) %>%
  map(~ mutate(., freq_cum = cumsum(freq))) %>%
  bind_rows()

ggplot(patch_freq_rel, aes(x = size, y = freq_cum, col = year, group = year)) +
  geom_line() +
  scale_x_log10() +
  scale_color_viridis_c()

#### Test for differences between countries vs. ecoregions ####

dat_grid <- dat %>%
  group_by(grid_id, year) %>%
  summarize(n = n(),
            area = sum(size),
            maxsize = quantile(size, 0.99),
            intermedsize = quantile(size, 0.75),
            mediansize = quantile(size, 0.5),
            size = mean(size),
            severity = mean(severity),
            country = names(which.max(table(country)))) %>%
  left_join(forest_grid, by = "grid_id") %>%
  left_join(ecoregions_grid, by = "grid_id") %>%
  mutate(rate = area / forest_ha) %>%
  mutate(frequency = n / forest_ha) %>%
  mutate(forestcover = forest_ha / land_ha)

selector <- dat_grid %>% 
  group_by(grid_id) %>%
  summarize(yearsdisturbed = sum(n > 0),
            land_ha = unique(land_ha),
            forestcover = unique(forestcover)) %>%
  filter(yearsdisturbed > 5)

dat_grid_summary <- dat_grid %>%
  na.omit(.) %>%
  filter(grid_id %in% selector$grid_id) %>%
  group_by(grid_id) %>%
  summarize(country = names(which.max(table(country))),
            ECO_NAME = unique(ECO_NAME),
            n_mean = mean(n),
            frequency_mean = mean(frequency * 100),
            area_mean = mean(area),
            size_mean = mean(size),
            mediansize_mean = mean(mediansize),
            intermedsize_mean = mean(intermedsize),
            maxsize_mean = mean(maxsize),
            severity_mean = mean(severity * 100),
            rate_mean = mean(rate),
            frequency_trend = trend::sens.slope(frequency * 100)$estimate / frequency_mean * 100,
            n_trend = trend::sens.slope(n)$estimate / n_mean * 100,
            area_trend = trend::sens.slope(area)$estimate / area_mean * 100,
            size_trend = trend::sens.slope(size)$estimate / size_mean * 100,
            maxsize_trend = trend::sens.slope(maxsize)$estimate / maxsize_mean * 100,
            mediansize_trend = trend::sens.slope(mediansize)$estimate / mediansize_mean * 100,
            intermedsize_trend = trend::sens.slope(intermedsize)$estimate / intermedsize_mean * 100,
            severity_trend = trend::sens.slope(severity * 100)$estimate / severity_mean * 100,
            rate_trend = trend::sens.slope(rate)$estimate / rate_mean * 100)

reference_grid <- read_sf("data/admin/referencegrid/hexagon_50km.shp")

reference_grid <- reference_grid %>%
  #right_join(ecoregions_grid, by = c("id" = "grid_id"))
  right_join(dat_grid_summary, by = c("id" = "grid_id"))

plot(reference_grid["country"])
plot(reference_grid["ECO_NAME"])

ggplot() +
  geom_sf(data = reference_grid %>% filter(ECO_NAME == "Scandinavian Montane Birch forest and grasslands"), 
          aes(fill = ECO_NAME, col = NULL))

ggplot() +
  geom_sf(data = reference_grid %>% filter(country == "switzerland"), 
          aes(fill = ECO_NAME, col = NULL))

library(lme4)
library(MuMIn)
library(DHARMa)
library(effects)
library(lmerTest)

biomes <- ecoregions %>%
  st_drop_geometry() %>%
  group_by(ECO_NAME = ECO_NAME) %>%
  summarize(biome = unique(BIOME))

dat_grid <- dat_grid %>%
  left_join(biomes)

dat_grid_filtered <- dat_grid %>%
  filter((forest_ha / land_ha) > 0.1 & !is.na(ECO_NAME))

fit_size <- lmer(log(size) ~ 1 + (1 | year) + (1 | ECO_NAME / country), 
            data = dat_grid_filtered,
            control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

fit_freq <- lmer(log(frequency) ~ 1 + (1 | year) + (1 | ECO_NAME / country), 
            data = dat_grid_filtered,
            control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

fit_severity <- lmer(severity ~ 1 + (1 | year) + (1 | ECO_NAME / country), 
            data = dat_grid_filtered,
            control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

VarCorr(fit_size) %>%
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  dplyr::select(grp, icc)

rand(fit)

### Plot average size, frequency and severity

preddat <- unique(dat_grid_filtered[, c("country", "ECO_NAME")]) %>%
  as.data.frame()

preddat$size_pred <- exp(predict(fit_size, preddat, re.form = ~ (1 | ECO_NAME / country)))
preddat$frequency_pred <- exp(predict(fit_freq, preddat, re.form = ~ (1 | ECO_NAME / country)))
preddat$severity_pred <- predict(fit_severity, preddat, re.form = ~ (1 | ECO_NAME / country))

countires_abbrev <- read_csv("data/countries_master.csv")

preddat <- preddat %>%
  left_join(countires_abbrev, by = c("country" = "country_name_short"))

getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(size_pred %in% c(max(size_pred), min(size_pred)))

p_size <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), size_pred, mean), 
                    y = size_pred)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = "Hectares", title = "A) Disturbance size\n")

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(frequency_pred %in% c(max(frequency_pred), min(frequency_pred)))

p_frequency <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), frequency_pred, mean), 
                              y = frequency_pred * 100)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = bquote("Patches per km"^2), title = "B) Disturbance frequency\n")

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(severity_pred %in% c(max(severity_pred), min(severity_pred)))

p_severity <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), severity_pred, mean), 
                              y = severity_pred * 100)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = "Percent", title = "C) Disturbance severity\n")

p <- p_size + p_frequency + p_severity + plot_layout(ncol = 1)

ggsave("results/size_frequency_severity_ecoregions_country.pdf", p, width = 7, height = 9)

# Plot as maps

ecoregions_map <- st_crop(ecoregions, st_bbox(reference_grid) + c(-0.05, -0.05, 0.01, 0.01) * as.double(st_bbox(reference_grid)))
ecoregions_countries <- st_intersection(ecoregions_map, countries)

ecoregions_countries <- ecoregions_countries %>%
  left_join(preddat, by = c("ISO_CC" = "iso_code", "ECO_NAME"))

ggplot() +
  geom_sf(data = ecoregions_countries, aes(fill = size_pred), col = gray(0.75, alpha = 0.25)) +
  geom_sf(data = countries, color = gray(0.25), fill = NA) +
  #geom_sf(data = ecoregions_map, color = gray(0.85), fill = NA, alpha = 0.2) +
  #geom_sf(data = europe_outline, color = gray(0.01), fill = NA) +
  scale_fill_gradient2(low = RColorBrewer::brewer.pal(3, "Set1")[[2]], 
                       mid = "white", 
                       high = RColorBrewer::brewer.pal(3, "Set1")[[1]]) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "lightblue", color = NULL),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, vjust = 0.135),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1, 0, 0, 0), "cm"),
        plot.title = element_text(size = 9, hjust = 0.5)
        #legend.margin = margin(0.75, 0.75, 0.75, 0.75),
        #legend.box.margin = margin(-10, 0.15, 0.15, 0.15)
        ) +
  guides(fill = guide_colorbar(title.position = "right", barwidth = 5, barheight = 0.5)) +
  #labs(fill = unit, title = title) +
  coord_sf(expand = FALSE)

### Trends

fit_size <- lmer(size_trend ~ 1 + (1 | ECO_NAME / country), 
                 data = dat_grid_summary,
                 control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

fit_freq <- lmer(frequency_trend ~ 1 + (1 | ECO_NAME / country), 
                 data = dat_grid_summary,
                 control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

fit_severity <- lmer(severity_trend ~ 1 + (1 | ECO_NAME / country), 
                     data = dat_grid_summary,
                     control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

VarCorr(fit_severity) %>%
  as_tibble() %>%
  mutate(icc = vcov / sum(vcov)) %>%
  select(grp, icc)

rand(fit_severity)

### Plot trend in size, frequency and severity

preddat <- unique(dat_grid_summary[, c("country", "ECO_NAME")]) %>%
  as.data.frame()

preddat$size_pred <- exp(predict(fit_size, preddat, re.form = ~ (1 | ECO_NAME / country)))
preddat$frequency_pred <- exp(predict(fit_freq, preddat, re.form = ~ (1 | ECO_NAME / country)))
preddat$severity_pred <- predict(fit_severity, preddat, re.form = ~ (1 | ECO_NAME / country))

countires_abbrev <- read_csv("data/countries_master.csv")

preddat <- preddat %>%
  left_join(countires_abbrev, by = c("country" = "country_name_short"))

getPalette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(size_pred %in% c(max(size_pred), min(size_pred)))

p_size <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), size_pred, mean), 
                              y = size_pred)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = "Percent change per year", title = "A) Trend in disturbance size\n")

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(frequency_pred %in% c(max(frequency_pred), min(frequency_pred)))

p_frequency <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), frequency_pred, mean), 
                                   y = frequency_pred)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = "Percent change per year", title = "B) Trend in disturbance frequency\n")

preddat_min_max <- preddat %>%
  group_by(ECO_NAME) %>%
  filter(severity_pred %in% c(max(severity_pred), min(severity_pred)))

p_severity <- ggplot(preddat, aes(x = reorder(abbreviate(ECO_NAME), severity_pred, mean), 
                                  y = severity_pred)) +
  geom_line(aes(group = ECO_NAME)) +
  geom_point(aes(col = country), size = 1) +
  geom_point(data = preddat_min_max, aes(col = country), size = 4) +
  geom_text(data = preddat_min_max, aes(label = iso_code), size = 2) +
  scale_color_manual(values = getPalette(length(unique(reference_grid$country)))) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.75, 0.25, 0.25), "cm"),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8, color = "black")) +
  labs(x = NULL, y = "Percent change per year", title = "C) Trend in disturbance severity\n")

p <- p_size + p_frequency + p_severity + plot_layout(ncol = 1)

ggsave("results/size_frequency_severity_trend_ecoregions_country.pdf", p, width = 7, height = 9)

### Patch size

ggplot(dat_grid, aes(x = factor(year), y = log10(size), fill = year)) +
  geom_violin() +
  scale_fill_viridis()

ggplot(dat_grid, aes(x = year, y = severity)) +
  geom_line(aes(group = interaction(grid_id, country), col = country), alpha = 0.3) +
  facet_wrap(~country) +
  theme(legend.position = "none") +
  geom_smooth(se = FALSE, col = "black")
