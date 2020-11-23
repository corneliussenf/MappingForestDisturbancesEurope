
library(tidyverse)

# Function ----------------------------------------------------------------

create_reference <- function(dat) {
  
  dat <- as_tibble(dat)
  
  dat_processed <- mutate(dat,
                          agent = dplyr::lead(change_process),
                          post_disturbance_lc = dplyr::lead(landcover))
  
  dat_processed <- filter(dat_processed, landuse == "Forest" & !is.na(agent))
  
  dat_processed <- dat_processed %>%
    group_by(plotid, year) %>%
    summarise(disturbance = sum(!(agent %in% c("Growth/Recovery", "Stable", "Regrowth"))),
              agent = unique(agent),
              post_disturbance_lc = unique(post_disturbance_lc))
  
  dat_processed <- ungroup(dat_processed)
  
  dat_processed <- dat_processed %>%
    mutate(agent = case_when(
      agent == "Growth/Recovery" ~ "Regrowth",
      agent == "Debris" ~ "Gravitational event",
      agent == "Decline" ~ "Biotic",
      agent == "Hydrology" ~ "Unknown canopy disturbance",
      agent == "Other" ~ "Unknown canopy disturbance",
      agent == "Wind" ~ "Uprooting and breakage",
      agent == "Harvest" ~ "Harvest",
      agent == "Stable" ~ "Stable",
      TRUE ~ agent
    ))
  
  dat_processed <- mutate(dat_processed, year = year + 1)
  
  dat_processed <- mutate(dat_processed, severity = ifelse(post_disturbance_lc %in% c("Trees", "Treed"), "NSR", "SR"))
  
  dat_processed <- dat_processed %>%
    group_by(plotid) %>%
    summarize(disturbance_n = sum(disturbance),
              year = paste(year[disturbance == 1], collapse = "."),
              agent = paste(agent[disturbance == 1], collapse = "."),
              severity = paste(severity[disturbance == 1], collapse = "."))
  
  max_dist <- max(dat_processed$disturbance_n)
  
  dat_processed <- dat_processed %>%
    separate("year", paste0("year_disturbance_", 1:max_dist), "\\.") %>%
    separate("agent", paste0("agent_disturbance_", 1:max_dist), "\\.") %>%
    separate("severity", paste0("severity_disturbance_", 1:max_dist), "\\.") %>%
    mutate(year_disturbance_1 = ifelse(year_disturbance_1 == "", NA, year_disturbance_1)) %>%
    mutate(agent_disturbance_1 = ifelse(agent_disturbance_1 == "", NA, agent_disturbance_1)) %>%
    mutate(severity_disturbance_1 = ifelse(severity_disturbance_1 == "", NA, severity_disturbance_1))
  
  return(dat_processed)
}

# Get data ----------------------------------------------------------------

names <- list.files("../TimeSync/data/timesync/databases_export/", "export.csv") %>%
  strsplit(., "_") %>%
  map(~ .[2]) %>%
  unlist()

country_information <- read_csv("../TimeSync/data/countries_master.csv")

timesync_exports <- list.files("../TimeSync/data/timesync/databases_export/", "export.csv", full.names = TRUE) %>%
  map(read_csv)

disturbances <- timesync_exports %>%
  map(create_reference) %>%
  set_names(names) %>%
  bind_rows(.id = "country")

disturbances <- disturbances %>%
  dplyr::select(-year_disturbance_4, -agent_disturbance_4, -severity_disturbance_4)

write_csv(disturbances, "data/references/disturbances.csv")
