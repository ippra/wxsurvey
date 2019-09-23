# Bayes example: https://timmastny.rbind.io/blog/multilevel-mrp-tidybayes-brms-stan/ 
# Sims: https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
# example edit

# Libraries and Options -------------------------
library(arm)
library(rgdal)
library(tidyverse)
library(rmapshaper)
library(data.table)
library(scales) 
library(mapproj)
options(scipen = 999)
options(max.print = 99999)
"%ni%" <- Negate("%in%")

# Census Data -------------------------
census_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/census_data.csv")

# Survey Data -------------------------
survey_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/survey_data.csv")

# Proportion of Variability Attributable to CWAs -------------------------
survey_data <- filter(survey_data, sample == "WX")
round(MuMIn::r.squaredGLMM(lmer(risk_heat ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_drought ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_cold ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_snow ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_tor ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_flood ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_hur ~ 1 + (1|CWA), data = survey_data)), 2)
round(MuMIn::r.squaredGLMM(lmer(risk_fire ~ 1 + (1|CWA), data = survey_data)), 2)

# Estimate Models -------------------------
fit1 <- lmer(risk_heat ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + HEAT, data = survey_data)
fit2 <- lmer(risk_drought ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + DROUGHT, data = survey_data)
fit3 <- lmer(risk_cold ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + COLD, data = survey_data)
fit4 <- lmer(risk_snow ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + SNOW, data = survey_data)
fit5 <- lmer(risk_tor ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + TORN, data = survey_data)
fit6 <- lmer(risk_flood ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + FLOOD, data = survey_data)
fit7 <- lmer(risk_hur ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + HURR, data = survey_data)
fit8 <- lmer(risk_fire ~ (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|CWA_REGION) + FIRE, data = survey_data)

# Generate Predictions for Each Demographic Group, Weight, and Aggregate to CWA -------------------------
heat_predictions <- census_data %>%
  mutate(PRED = predict(fit1, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "HEAT")

drought_predictions <- census_data %>%
  mutate(PRED = predict(fit2, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "DROUGHT")

cold_predictions <- census_data %>%
  mutate(PRED = predict(fit3, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "COLD")

snow_predictions <- census_data %>%
  mutate(PRED = predict(fit4, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "SNOW")

tor_predictions <- census_data %>%
  mutate(PRED = predict(fit5, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "TORN")

flood_predictions <- census_data %>%
  mutate(PRED = predict(fit6, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "FLOOD")

hurricane_predictions <- census_data %>%
  mutate(PRED = predict(fit7, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "HURR")

wildfire_predictions <- census_data %>%
  mutate(PRED = predict(fit8, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(PRED = PRED * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(PRED = sum(PRED)) %>% 
  mutate(Z = (PRED - mean(PRED))/sd(PRED), measure = "FIRE")

all_predictions <- bind_rows(heat_predictions, drought_predictions, cold_predictions, snow_predictions, tor_predictions, flood_predictions, 
                             hurricane_predictions, wildfire_predictions)

# Plot Maps ----------------------------------------
storm_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Storm_Data_by_CWA.csv")
long_storm_data <- storm_data %>% 
  reshape2::melt(id = "WFO") %>% 
  rename("CWA" = "WFO") %>% 
  rename("measure" = "variable")
long_storm_data <- filter(long_storm_data, CWA %in% census_data$CWA)
long_storm_data <- left_join(long_storm_data, all_predictions, by = c("CWA", "measure"))
names(long_storm_data) <- c("CWA", "HAZARD", "EXPOSURE_Z", "RISK_PRED", "RISK_Z")

setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile")
cwa.shp <- readOGR('.','w_11au16_simplify')
cwa.shp <- subset(cwa.shp, !ST %in% c("HI", "AK", "PR", "AS", "GU"))

cwa.shp@data$id <- rownames(cwa.shp@data)
cwa.shp.points <- fortify(cwa.shp, region = "id")
cwa.shp.df <- plyr::join(cwa.shp.points, cwa.shp@data, by = "id")
cwa.shp.df <- left_join(cwa.shp.df, long_storm_data, by = "CWA")
cwa.shp.df <- cwa.shp.df %>% filter(!is.na(HAZARD))

plot_theme <- theme(panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    legend.position = "bottom", 
                    legend.key.width = unit(1, "cm"), 
                    legend.key.height = unit(0.5, "cm"), 
                    legend.text = element_text(size = 8),
                    legend.title = element_text(size = 8), 
                    strip.text = element_text(size = 7),
                    axis.ticks = element_blank(), 
                    axis.title = element_blank(), 
                    axis.text = element_blank(),
                    plot.title = element_text(face = "bold", size = 10))

cwa.shp.df <- cwa.shp.df %>% 
  mutate(HAZARD = recode(HAZARD, HEAT = "Extreme heat waves",
                         COLD = "Extreme cold temperatures",
                         DROUGHT = "Drought",
                         FIRE = "Wildfires",
                         FLOOD = "Floods",
                         HURR = "Hurricanes",
                         SNOW = "Extreme snow (or ice) storms",
                         TORN = "Tornadoes"))

fig_1a <- ggplot(cwa.shp.df) +
  aes(long, lat, group = group, fill = EXPOSURE_Z) +
  geom_polygon(color = "black", size = 0.02) +
  scale_fill_gradient2(midpoint = 0, low = "green", high = "red", mid = "white", limits = c(-2, 2), oob = squish,
                       labels = c(expression(-2*sigma), NA, expression(0*sigma), NA, expression(2*sigma))) +
  facet_wrap(~HAZARD, ncol = 3, nrow = 3) +
  theme_bw() +
  labs(fill = "Z-Score", title = "(a)") +
  plot_theme + 
  coord_map("polyconic")

fig_1b <- ggplot(cwa.shp.df) +
  aes(long, lat, group = group, fill = RISK_Z) +
  geom_polygon(color = "black", size = 0.02) +
  scale_fill_gradient2(midpoint = 0, low = "green", high = "red", mid = "white", limits = c(-2, 2), oob = squish,
                       labels = c(expression(-2*sigma), NA, expression(0*sigma), NA, expression(2*sigma))) +
  facet_wrap(~HAZARD, ncol = 3, nrow = 3) +
  theme_bw() +
  labs(fill = "Z-Score", title = "(b)") +
  plot_theme + 
  coord_map("polyconic")

fig_1 <- gridExtra::grid.arrange(fig_1a, fig_1b, ncol = 2)

ggsave(file = "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Figures/fig_1.png", fig_1, width = 10, height = 5, dpi = 800)

# Survey Validation ----------------------------------------
validation_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/survey_data.csv")
validation_data <- filter(validation_data, sample == "OS")
validation_data <- validation_data %>% 
  dplyr::select(CWA, risk_wind:risk_fire, -c(risk_wind, risk_rain)) %>%
  group_by(CWA) %>% 
  summarise_all(funs(mean)) %>%
  mutate_at(vars(-CWA), list(~scale(.) %>% as.vector)) %>% 
  rename("HEAT" = "risk_heat") %>% 
  rename("DROUGHT" = "risk_drought") %>% 
  rename("COLD" = "risk_cold") %>% 
  rename("SNOW" = "risk_snow") %>% 
  rename("TORN" = "risk_tor") %>% 
  rename("FLOOD" = "risk_flood") %>% 
  rename("HURR" = "risk_hur") %>% 
  rename("FIRE" = "risk_fire") %>% 
  reshape2::melt(id = "CWA") %>% 
  rename("HAZARD" = "variable") %>% 
  rename("RISK_Z_VALID" = "value") 

validation_data <- validation_data %>% 
  mutate(HAZARD = recode(HAZARD, HEAT = "Extreme heat waves",
                         COLD = "Extreme cold temperatures",
                         DROUGHT = "Drought",
                         FIRE = "Wildfires",
                         FLOOD = "Floods",
                         HURR = "Hurricanes",
                         SNOW = "Extreme snow (or ice) storms",
                         TORN = "Tornadoes"))

long_storm_data <- long_storm_data %>% 
  mutate(HAZARD = recode(HAZARD, HEAT = "Extreme heat waves",
                         COLD = "Extreme cold temperatures",
                         DROUGHT = "Drought",
                         FIRE = "Wildfires",
                         FLOOD = "Floods",
                         HURR = "Hurricanes",
                         SNOW = "Extreme snow (or ice) storms",
                         TORN = "Tornadoes"))

validation_data <- left_join(validation_data, long_storm_data, by = c("CWA", "HAZARD"))
cors <- validation_data %>%
  group_by(HAZARD) %>%
  summarize(COR = cor(RISK_Z, RISK_Z_VALID))

dat_text <- data.frame(label = paste0("r = ", sprintf("%.2f", round(cors$COR, 2))), HAZARD = cors$HAZARD)

fig_2a <- ggplot(validation_data, aes(x = RISK_Z_VALID, y = RISK_Z)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  facet_wrap(~HAZARD) +
  geom_text(data = dat_text, mapping = aes(x = 2.5, y = -3.8, label = label)) +
  theme_bw() + 
  labs(y = "Risk Perception Estimates", x = "Independent Survey Data", title = "(a)") +
  theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         legend.position = "bottom", 
         legend.key.width = unit(1, "cm"), 
         legend.key.height = unit(0.5, "cm"), 
         legend.text = element_text(size = 8),
         legend.title = element_text(size = 8), 
         strip.text = element_text(size = 7),
         plot.title = element_text(face = "bold", size = 10))

# Howe Validation ----------------------------------------
howe_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Howe_Data_by_CWA.csv")
heat_data <- long_storm_data %>% 
  filter(HAZARD == "Extreme heat waves") %>% 
  left_join(., howe_data, by = "CWA") %>% 
  mutate(RISKP_EST_Z = scale(riskp_est))
heat_data %>% summarize(cor(RISK_Z, RISKP_EST_Z))

fig_2b <- ggplot(heat_data, aes(x = RISKP_EST_Z, y = RISK_Z)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  theme_bw() + 
  annotate("text", x = 3.5, y = -4, label = paste0("r = 0.74")) +
  labs(y = "Risk Perception Estimates", x = "Previous Study Estimates", title = "(b)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(0.5, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8), 
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 10))

fig_2 <- gridExtra::grid.arrange(fig_2a, fig_2b, ncol = 2)

ggsave(file = "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Figures/fig_2.png", fig_2, width = 10, height = 5, dpi = 800)

# Exposure vs. Risk Perception ----------------------------------------
cors2 <- long_storm_data %>%
  group_by(HAZARD) %>%
  summarize(COR = cor(RISK_Z, EXPOSURE_Z))

dat_text2 <- data.frame(label = paste0("r = ", sprintf("%.2f", round(cors2$COR, 2))), HAZARD = cors2$HAZARD)

fig_3a <- ggplot(long_storm_data, aes(x = EXPOSURE_Z, y = RISK_Z)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", fullrange = TRUE, se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  facet_wrap(~HAZARD) +
  geom_text(data = dat_text2, mapping = aes(x = 2.5, y = -3.8, label = label)) +
  theme_bw() + 
  labs(y = "Risk Perception Estimates", x = "Exposure", title = "(a)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(0.5, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8), 
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 10))

CWA_NAMES <- tibble(CWA = filter(long_storm_data, HAZARD == "Floods")$CWA)
CWA_NAMES <- left_join(CWA_NAMES, cwa.shp@data, by = "CWA")

flood_resids <- broom::augment(lm(RISK_Z ~ EXPOSURE_Z, filter(long_storm_data, HAZARD == "Floods"))) %>% 
  mutate(CWA = CWA_NAMES$CityState) 
pos_flood_resids <- flood_resids %>% 
  arrange(-.resid) %>% 
  slice(1:5)
neg_flood_resids <- flood_resids %>% 
  arrange(.resid) %>% 
  slice(1:5)
flood_outliers <- bind_rows(pos_flood_resids, neg_flood_resids)
flood_outliers$HAZARD <- "Floods"

heat_resids <- broom::augment(lm(RISK_Z ~ EXPOSURE_Z, filter(long_storm_data, HAZARD == "Extreme heat waves"))) %>% 
  mutate(CWA = CWA_NAMES$CityState) 
pos_heat_resids <- heat_resids %>% 
  arrange(-.resid) %>% 
  slice(1:5)
neg_heat_resids <- heat_resids %>% 
  arrange(.resid) %>% 
  slice(1:5)
heat_outliers <- bind_rows(pos_heat_resids, neg_heat_resids)
heat_outliers$HAZARD <- "Extreme heat waves"

outliers <- bind_rows(flood_outliers, heat_outliers)

fig_3b <- ggplot(outliers, aes(y = .resid, x = fct_reorder(CWA, .resid), fill = .resid)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  facet_wrap(~HAZARD, scale = "free_y", nrow = 2) +
  scale_y_continuous(breaks = seq(-3, 4, 1), labels = seq(-3, 4, 1)) +
  labs(y = "Risk Perception Estimate v. Exposure", x = "County Warning Area", title = "(b)") +
  guides(fill = FALSE) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(0.5, "cm"), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8), 
        strip.text = element_text(size = 7),
        plot.title = element_text(face = "bold", size = 10))

fig_3 <- gridExtra::grid.arrange(fig_3a, fig_3b, ncol = 2)

ggsave(file = "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Figures/fig_3.png", fig_3, width = 10, height = 5, dpi = 800)

  