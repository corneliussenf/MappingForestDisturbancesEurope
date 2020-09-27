
library(tidyverse)
library(randomForest)

samplesizes <- read_delim("data/reference/sample_sizes.csv", delim = ";")

countries <- list.files("data/model_input") %>%
  str_split("_") %>%
  map(~ .[3] %>% gsub(".csv", "", .)) %>%
  unlist()

model_inp <- countries %>%
  map(~ read_csv(paste0("data/model_input/model_input_", ., ".csv"))) %>%
  set_names(countries) %>%
  bind_rows(.id = "country")

disturbances_ref <- read_csv("data/references/disturbances.csv") %>%
  filter(!(country == "Austria" & year_disturbance_1 == 1985))

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

model_inp <- model_inp %>% filter(!((year.B5 == 0 & year.B7 == 0 & year.NBR == 0 & year.TCW == 0 & class == "Disturbance") & country %in% c("austria", "czechia", "germany", "slowakia", "switzerland", "poland") & class == "Disturbance"))

table(model_inp$country, model_inp$class)

model_val <- model_inp %>% 
  split(.$country) %>% 
  map(~ sample_n(., samplesizes[samplesizes$country_name_short == unique(.$country), "val_n"][[1]])) %>% 
  bind_rows()

table(model_val$country, model_val$class) %>%
  as.data.frame(.) %>%
  spread(key = Var2, value = Freq) %>%
  dplyr::rename(country = Var1) %>%
  write_csv(., "temp/model_validation_samples_by_country.csv")

model_cal <- anti_join(model_inp, model_val)

model_cal <- model_cal %>%
  mutate(country_class = as.factor(as.integer(factor(country)))) %>%
  mutate(class = factor(class))

model_val <- model_val %>%
  mutate(country_class = as.factor(as.integer(factor(country)))) %>%
  mutate(class = factor(class))

predictors <-  model_cal %>% 
  dplyr::select(year.B5:dsnr.TCW, TCG.1985:TCW.2017) %>%
  #dplyr::select(year.B5:dsnr.TCW, TCG.1985:TCW.1985) %>%
  names()

fit <- randomForest(as.formula(paste0("class ~", paste(predictors, collapse = "+"), " + x_coord + y_coord")),
                    data = model_cal,
                    ntree = 500)

plot(fit)

save(fit, file = paste0("models/randomforest_", format(Sys.Date(), format="%m-%d-%Y"), ".RData"))

# Validation

predict <- predict(fit, newdata = model_val)

model_val$predicted <- predict

conf <- table(model_val$predicted, model_val$class)

conf

1 - diag(conf) / rowSums(conf) # Comission error
1 - diag(conf) / colSums(conf) # Omission error
sum(diag(conf)) / sum(conf) # Overall accuracy

### Validate year

calculate_mode <- function(x, na.rm = TRUE) {
  x <- as.integer(x)
  if(na.rm){
    x <- x[!is.na(x)]
    x[x == 1985] <- NA
    x[x == 0] <- NA
  }
  ux <- unique(x)
  if (length(ux) < length(x)) {
    return(x[which.max(table(x))])  
  } else {
    return(median(x, na.rm = TRUE))
  }
}

model_inp <- model_inp %>%
  mutate_at(.vars = vars(year.B5, year.B7, year.NBR, year.TCW),
            .fun = function(x) ifelse(x %in% c(0, 1985), NA, x))

model_inp$year_mode <- apply(model_inp[, c("year.B5", "year.B7", "year.NBR", "year.TCW")], 1, calculate_mode)

model_inp_nona <- model_inp[!is.na(model_inp$year_disturbance_1), ]

model_inp_nona$disturbance_year_matched <- apply(model_inp_nona[, c("year_disturbance_1", "year_disturbance_2", "year_disturbance_3", "year_mode")], 1, 
                                                 function(x) na.omit(x[1:3])[MALDIquant::match.closest(x[4], na.omit(x[1:3]))])

rmse <- sqrt(mean((model_inp_nona$year_mode - model_inp_nona$disturbance_year_matched)^2, na.rm = TRUE))
mae <- mean(abs(model_inp_nona$year_mode - model_inp_nona$disturbance_year_matched), na.rm = TRUE)

p <- ggplot(model_inp_nona, aes(x = disturbance_year_matched, y = year_mode)) +
  geom_abline(intercept = 0, slope = 1, col = "grey") +
  geom_point(aes(), shape = 16, alpha = 0.05, stroke = 0, size = 2.5) +
  xlim(1985, 2018) + ylim(1985, 2018) +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  labs(x = "Manually interpreted year of disturbance",
       y = "Autmatically assigned year of disturbance")

ggsave("results/temporal_validation.pdf", p, width = 3.5, height = 3.5)

res1 <- c()
res2 <- c()
res3 <- c()
for (i in 1:nrow(model_inp_nona)) {
  res1 <- c(res1, model_inp_nona[i, "year_mode"][[1]] %in% (model_inp_nona[i, "disturbance_year_matched"][[1]] + c(-1, 0, 1)))
  res2 <- c(res2, model_inp_nona[i, "year_mode"][[1]] %in% (model_inp_nona[i, "disturbance_year_matched"][[1]] + c(-2, -1, 0, 1, 2)))
  res3 <- c(res3, model_inp_nona[i, "year_mode"][[1]] %in% (model_inp_nona[i, "disturbance_year_matched"][[1]] + c(-3, -2, -1, 0, 1, 2, 3)))
}

mean(res1)
mean(res2)
mean(res3)

### Analyse errors

model_val <- model_val %>%
  mutate(error_type = case_when(
    predicted == "Forest" & class == "Disturbance" ~ "Omission",
    predicted == "Disturbance" & class == "Forest" ~ "Commission",
    predicted == "Disturbance" & class == "Disturbance" ~ "None"
    ))

p <- ggplot(model_val %>% 
         filter(!is.na(error_type)) %>%
         dplyr::select(magnitude.B5, magnitude.B7, magnitude.NBR, magnitude.TCW,
                       error_type) %>%
         gather(key = band, value = value, -error_type) %>%
         separate("band", c("metric", "band"), "\\."), 
       aes(x = band, y = value, fill = error_type)) +
  geom_boxplot() +
  theme_minimal() +
  coord_flip() +
  labs(x = NULL, y = "Spectral change magnitude", fill = "Error type") +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set1")[c(1, 3, 2)])

ggsave("results/analyzing_errors.pdf", p, width = 4.5, height = 3.5)
