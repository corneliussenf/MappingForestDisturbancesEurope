
library(tidyverse)
library(raster)

disturbances <- read_csv("data/references/disturbances.csv")

dat_cal <- vector("list", 2)
dat_val <- vector("list", 2)

k <- 0

for (cntr in unique(disturbances$country)) {
  
  k <- k + 1
  
  print(cntr)
  
  dist <- disturbances %>%
    filter(country == cntr) %>%
    dplyr::select(country, plotid, disturbance_n, severity_disturbance_1) %>%
    filter(disturbance_n <= 1 & (severity_disturbance_1 == "SR" | is.na(severity_disturbance_1)))
  
  mod_inp <- read_csv(paste0("data/model_input/model_input_", cntr, ".csv"))
  
  dat_cal[[k]] <- mod_inp %>%
    filter(landcover == "Forest") %>%
    right_join(dist, by = c("plotid"))
  
  dist <- disturbances %>%
    filter(country == cntr) %>%
    dplyr::select(country, plotid, disturbance_n, severity_disturbance_1)
  
  dat_val[[k]] <- mod_inp %>%
    filter(landcover == "Forest") %>%
    right_join(dist, by = c("plotid"))
    
}

dat_cal <- dat_cal %>% bind_rows()
dat_val <- dat_val %>% bind_rows()

fit0 <- glm(disturbance_n ~ 1, data = dat_cal, family = binomial("logit"))

fit1 <- glm(disturbance_n ~ magnitude.B5 + magnitude.B7 + magnitude.NBR + magnitude.TCW, 
            data = dat_cal, family = binomial("logit"))

AIC(fit0, fit1)

fit <- fit1

p <- ggplot(data = dat_cal %>% filter(magnitude.NBR > 0), aes(x = magnitude.NBR, y = disturbance_n)) +
  geom_point(position = position_jitter(height = 0.015), alpha = 0.1,
             aes(shape = ifelse(disturbance_n == 1, "Stand-replacing", "No disturbance"))) +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE, col = "red") +
  theme_minimal() +
  labs(x = "Normalized Burn Ratio", y = "Disturbance severity", shape = NULL)

dat_val$pred <- predict(fit, newdata = dat_val, type = "response")

dat_val_disturbance <- dat_val %>% filter(!is.na(severity_disturbance_1))

sequence <- seq(0, 1, 0.001)

oa <- c()
ce <- c()
oe <- c()

for (p in sequence) {
  conf <- table(factor(ifelse(dat_val_disturbance$pred > p, "SR", "NSR"), levels = c("NSR", "SR")),
                factor(dat_val_disturbance$severity_disturbance_1, levels = c("NSR", "SR")))
  oa <- c(oa, sum(diag(conf)) / sum(conf))
  ce <- c(ce, conf[4] / sum(conf[3:4]))
  oe <- c(oe, conf[4] / sum(conf[c(2, 4)]))
}

p_sr_nsr <- sequence[which.min(abs(oe-ce))]
oa[which.min(abs(oe-ce))]
ce[which.min(abs(oe-ce))]
oe[which.min(abs(oe-ce))]

p <- data.frame(p = sequence, Overall = 1 - oa, Omission = 1 - oe, Commision = 1 - ce) %>%
  gather(key = key, value = value, -p) %>%
  ggplot(., aes(x = p, y = value, col = key)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(x = "Probability threshold", y = "Error rate", col = NULL)

ggsave("results/sr_nsr_disturbance_severity.pdf", p, width = 4.5, height = 3.5)

oa <- c()
ce <- c()
oe <- c()

for (p in sequence) {
  conf <- table(factor(ifelse(dat_val$pred > p, "D", "ND"), levels = c("D", "ND")),
                factor(ifelse(is.na(dat_val$severity_disturbance_1), "ND", "D"), levels = c("D", "ND")))
  oa <- c(oa, sum(diag(conf)) / sum(conf))
  ce <- c(ce, conf[4] / sum(conf[3:4]))
  oe <- c(oe, conf[4] / sum(conf[c(2, 4)]))
}

p_dist_nodist <- sequence[which.min(abs(oe-ce))]
oa[which.min(abs(oe-ce))]
ce[which.min(abs(oe-ce))]
oe[which.min(abs(oe-ce))]

data.frame(p = sequence, oa, oe, ce) %>%
  gather(key = key, value = value, -p) %>%
  ggplot(., aes(x = p, y = value, col = key)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

p <- ggplot(dat_val %>% 
         mutate(label = ifelse(is.na(severity_disturbance_1), "Stable forest", ifelse(severity_disturbance_1 == "SR", "Stand-replacing\ndisturbance", "Non-stand-replacing\ndisturbance")),
                label = factor(label, levels = c("Stable forest", "Non-stand-replacing\ndisturbance", "Stand-replacing\ndisturbance"))), 
       aes(x = label, y = pred, fill = label)) +
  geom_boxplot(outlier.colour = NA) +
  coord_flip() +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set1")[c(3, 2, 1)]) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(size = 10, color = "black")) +
  labs(y = "Severity", x = NULL)
  # geom_hline(yintercept = p_dist_nodist, linetype = "dashed") +
  # geom_hline(yintercept = p_sr_nsr, linetype = "dashed")

ggsave("results/analyzing_disturbance_severity.pdf", p, width = 3.5, height = 2.5)


### Apply to rasters

for (cntr in unique(disturbances$country)) {
  
  print(cntr)
  
  dist_metrics <- stack(paste0("prediction/", cntr, "/disturbance_metrics_", cntr, ".tif"))
  
  names(dist_metrics) <- as.vector(outer(c("year", "magnitude", "duration", "pre", "rate", "dsnr"), c("NBR", "TCW", "B5", "B7"), paste, sep = "."))
  
  beginCluster(30)
  severity_prediction <- clusterR(dist_metrics, predict, args = list(model = fit, type = "response"))
  endCluster()
  
  #severity_prediction <- predict(dist_metrics, fit, type = "response")
  
  writeRaster(severity_prediction, paste0("prediction/", cntr, "/disturbance_severity_", cntr, ".tif"))
    
}



