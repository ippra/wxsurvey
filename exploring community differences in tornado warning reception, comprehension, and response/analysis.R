# Libraries, Options, and Themes -------------------------
library(rgdal)
library(arm)
library(rstanarm)
library(ltm)
library(scales)
library(tidyverse)
library(ggrepel)
library(robustbase)
options(scipen = 999)
options(max.print = 99999)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
plot_theme <- theme(strip.text = element_text(size = 14),
                    axis.title = element_text(size = 14),
                    axis.text = element_text(size = 14),
                    plot.title = element_text(face = "bold", size = 16))

map_theme <- theme(panel.grid = element_blank(), 
                    plot.margin = unit(c(t = 0.2, l = -0.2, r = -0.2, b = 0), "cm"),
                    legend.position = "bottom",
                    legend.key.width = unit(1.5, "cm"), 
                    legend.key.height = unit(0.75, "cm"), 
                    legend.text = element_text(size = 14),
                    legend.title = element_text(size = 15), 
                    strip.text = element_text(size = 14),
                    axis.ticks = element_blank(), 
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    plot.title = element_text(face = "bold", size = 16))

# File Path - set locally!
file_path <- paste0("~/Dropbox/Exploring Geographic Differences in Tornado Warning Reception, Comprehension, and Response/Github/")

# Census Data -------------------------
census_data <- read_csv(paste0(file_path, "data/census_data.csv"))

# Survey Data -------------------------
survey_data <- read_csv(paste0(file_path, "data/survey_data.csv"))
survey_data <- survey_data %>% filter(survey_year %in% c(2018, 2019) | is.na(survey_year)) # INCLUDES CWA OVERSAMPLE FROM 2018 (n = 1543)

# Measures -------------------------
recep_data <- survey_data %>% dplyr::select(rec_all, rec_most, rec_soon, rec_sleep, rec_driving, rec_work, rec_store, rec_small_group, rec_large_group,
                                            rec_morn, rec_aft, rec_eve)
recep_fit <- grm(recep_data)
survey_data$recep <- ltm::factor.scores(recep_fit, resp.patterns = recep_data)$score.dat$z1

subj_comp_data <- survey_data %>% dplyr::select(alert_und, tor_watchwarn_und, tor_map_und, tor_radar_und, svr_watchwarn_und, und_morn, und_aft, und_eve)
subj_comp_fit <- grm(subj_comp_data)
survey_data$subj_comp <- ltm::factor.scores(subj_comp_fit, resp.patterns = subj_comp_data)$score.dat$z1

survey_data$watch_warn_group <- ifelse(is.na(survey_data$torwatch) == FALSE, "watch", "warn")
survey_data$watch_warn_correct <- NA
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "watch" & survey_data$torwatch == 1, 1, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "watch" & survey_data$torwatch != 1, 0, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "warn" & survey_data$torwarn == 2, 1, survey_data$watch_warn_correct)
survey_data$watch_warn_correct <- ifelse(survey_data$watch_warn_group == "warn" & survey_data$torwarn != 2, 0, survey_data$watch_warn_correct)
survey_data$warn_time_correct <- ifelse(survey_data$warn_time == 1 & survey_data$warn_time_minutes < 30, 1, 0)
survey_data$watch_time_correct <- ifelse(survey_data$watch_time == 2 & survey_data$watch_time_hours >= 1 & survey_data$watch_time_hours <= 3, 1, 0)
survey_data$warn_size_correct <- ifelse(survey_data$warn_size == 1 | survey_data$warn_size == 2, 1, 0)
survey_data$watch_size_correct <- ifelse(survey_data$watch_size == 3 | survey_data$watch_size == 4 | survey_data$watch_size == 5, 1, 0)
obj_comp_data <- survey_data %>% dplyr::select(watch_warn_correct, warn_time_correct, watch_time_correct, watch_size_correct)
obj_comp_fit <- ltm(obj_comp_data ~ z1)
survey_data$obj_comp <- ltm::factor.scores(obj_comp_fit, resp.patterns = obj_comp_data)$score.dat$z1

resp_data <- survey_data %>% dplyr::select(resp_prot, resp_sleep, resp_driving, resp_work, resp_store, resp_small_group, resp_large_group, resp_morn, resp_aft, resp_eve)
resp_fit <- grm(resp_data)
survey_data$resp <- ltm::factor.scores(resp_fit, resp.patterns = resp_data)$score.dat$z1

fit_data <- survey_data %>% filter(!sample == "OS")

# Estimate Models ------------------------- Note: models take many hours to run; load pre-run models below.
# sfit1 <- stan_lmer(recep ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP)  + TORN,
#                     data = fit_data,
#                     warmup = 500,
#                     iter = 1000,
#                     cores = 4,
#                     adapt_delta = 0.99999,
#                     refresh = 10)
# saveRDS(sfit1, "~/Dropbox/Exploring Geographic Differences in Tornado Warning Reception, Comprehension, and Response/Models/sfit1.Rds")
# 
# sfit2 <- stan_lmer(subj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) + TORN,
#                     data = fit_data,
#                     warmup = 500,
#                     iter = 1000,
#                     cores = 4,
#                     adapt_delta = 0.99999,
#                     refresh = 10)
# saveRDS(sfit2, "~/Dropbox/Exploring Geographic Differences in Tornado Warning Reception, Comprehension, and Response/Models/sfit2.Rds")
# 
# sfit3 <- stan_lmer(obj_comp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) + TORN,
#                     data = fit_data,
#                     warmup = 500,
#                     iter = 1000,
#                     cores = 4,
#                     adapt_delta = 0.99999,
#                     refresh = 10)
# saveRDS(sfit3, "~/Dropbox/Exploring Geographic Differences in Tornado Warning Reception, Comprehension, and Response/Models/sfit3.Rds")
# 
# sfit4 <- stan_lmer(resp ~ 1 + (1|MALE) + (1|AGE_GROUP) + (1|HISP) + (1|RACE_GROUP) + (1|CWA) + (1|MALE:AGE_GROUP) + TORN,
#                     data = fit_data,
#                     warmup = 500,
#                     iter = 1000,
#                     cores = 4,
#                     adapt_delta = 0.99999,
#                     refresh = 10)
# saveRDS(sfit4, "~/Dropbox/Exploring Geographic Differences in Tornado Warning Reception, Comprehension, and Response/Models/sfit4.Rds")

# Load Models  -------------------------
sfit1 <- readRDS(paste0(file_path, "models/sfit1.Rds"))
sfit2 <- readRDS(paste0(file_path, "models/sfit2.Rds"))
sfit3 <- readRDS(paste0(file_path, "models/sfit3.Rds"))
sfit4 <- readRDS(paste0(file_path, "models/sfit4.Rds"))
print(sfit1, digits = 2)
print(sfit2, digits = 2)
print(sfit3, digits = 2)
print(sfit4, digits = 2)

# Plot Estimates  -------------------------
s1 <- bind_cols(as_tibble(ranef(sfit1)), as_tibble(summary(sfit1)) %>% slice(3:132)) # make dataset of beta predictions
s2 <- bind_cols(as_tibble(ranef(sfit2)), as_tibble(summary(sfit2)) %>% slice(3:132))
s3 <- bind_cols(as_tibble(ranef(sfit3)), as_tibble(summary(sfit3)) %>% slice(3:132))
s4 <- bind_cols(as_tibble(ranef(sfit4)), as_tibble(summary(sfit4)) %>% slice(3:132))
s1$grpvar <- factor(s1$grpvar, levels = c("MALE", "AGE_GROUP", "MALE:AGE_GROUP", "RACE_GROUP", "HISP", "CWA"), 
                    labels = c("Gender", "Age", "Gender x Age", "Race", "Ethnicity", "CWA"))
s2$grpvar <- factor(s2$grpvar, levels = c("MALE", "AGE_GROUP", "MALE:AGE_GROUP", "RACE_GROUP", "HISP", "CWA"), 
                    labels = c("Gender", "Age", "Gender x Age", "Race", "Ethnicity", "CWA"))
s3$grpvar <- factor(s3$grpvar, levels = c("MALE", "AGE_GROUP", "MALE:AGE_GROUP", "RACE_GROUP", "HISP", "CWA"), 
                    labels = c("Gender", "Age", "Gender x Age", "Race", "Ethnicity", "CWA"))
s4$grpvar <- factor(s4$grpvar, levels = c("MALE", "AGE_GROUP", "MALE:AGE_GROUP", "RACE_GROUP", "HISP", "CWA"), 
                    labels = c("Gender", "Age", "Gender x Age", "Race", "Ethnicity", "CWA"))
s1 <- s1 %>% mutate_at(vars(`2.5%`:`97.5%`), ~pnorm(.) * 100) # convert z-scores to percentiles
s2 <- s2 %>% mutate_at(vars(`2.5%`:`97.5%`), ~pnorm(.) * 100)
s3 <- s3 %>% mutate_at(vars(`2.5%`:`97.5%`), ~pnorm(.) * 100)
s4 <- s4 %>% mutate_at(vars(`2.5%`:`97.5%`), ~pnorm(.) * 100)
s1$grp_text <- factor(c(as.character(s1$grp[1:114]), "Female, 18-34", "Female, 35-59", "Female, 60+", "Male, 18-34", "Male, 35-59", "Male, 60+", 
                        "White", "Black", "Other", "18-34", "35-59", "60+", "Non-Hisp", "Hispanic", "Female", "Male"))
s1$grp_text <- factor(s1$grp_text, levels = c(as.character(s1$grp[1:114]), "Female, 60+", "Female, 35-59", "Female, 18-34", "Male, 60+", "Male, 35-59", "Male, 18-34", 
                                              "Other", "Black", "White", "60+", "35-59", "18-34", "Non-Hisp", "Hispanic", "Female", "Male"))
s2$grp_text <- factor(c(as.character(s2$grp[1:114]), "Female, 18-34", "Female, 35-59", "Female, 60+", "Male, 18-34", "Male, 35-59", "Male, 60+", 
                        "White", "Black", "Other", "18-34", "35-59", "60+", "Non-Hisp", "Hispanic", "Female", "Male"))
s2$grp_text <- factor(s2$grp_text, levels = c(as.character(s2$grp[1:114]), "Female, 60+", "Female, 35-59", "Female, 18-34", "Male, 60+", "Male, 35-59", "Male, 18-34", 
                                              "Other", "Black", "White", "60+", "35-59", "18-34", "Non-Hisp", "Hispanic", "Female", "Male"))
s3$grp_text <- factor(c(as.character(s3$grp[1:114]), "Female, 18-34", "Female, 35-59", "Female, 60+", "Male, 18-34", "Male, 35-59", "Male, 60+", 
                        "White", "Black", "Other", "18-34", "35-59", "60+", "Non-Hisp", "Hispanic", "Female", "Male"))
s3$grp_text <- factor(s3$grp_text, levels = c(as.character(s3$grp[1:114]), "Female, 60+", "Female, 35-59", "Female, 18-34", "Male, 60+", "Male, 35-59", "Male, 18-34", 
                                              "Other", "Black", "White", "60+", "35-59", "18-34", "Non-Hisp", "Hispanic", "Female", "Male"))
s4$grp_text <- factor(c(as.character(s4$grp[1:114]), "Female, 18-34", "Female, 35-59", "Female, 60+", "Male, 18-34", "Male, 35-59", "Male, 60+", 
                        "White", "Black", "Other", "18-34", "35-59", "60+", "Non-Hisp", "Hispanic", "Female", "Male"))
s4$grp_text <- factor(s4$grp_text, levels = c(as.character(s4$grp[1:114]), "Female, 60+", "Female, 35-59", "Female, 18-34", "Male, 60+", "Male, 35-59", "Male, 18-34", 
                                              "Other", "Black", "White", "60+", "35-59", "18-34", "Non-Hisp", "Hispanic", "Female", "Male"))

p1 <- bind_rows(
  s1 %>% filter(grpvar == "CWA") %>% top_n(5, `50%`),
  s1 %>% filter(grpvar == "CWA") %>% top_n(-5, `50%`)) %>% 
  mutate(grp_text = fct_reorder(grp_text, `50%`)) %>% 
  bind_rows(., s1 %>% filter(grpvar != "CWA")) %>% 
  ggplot(., aes(x = `50%`, y = grp_text)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `25%`, xmax = `75%`), height = 0.2) +
  xlim(30, 70) +
  facet_wrap(~grpvar, scales = "free_y") +
  theme_bw() +
  labs(x = "Estimate", y = "") +
  ggtitle("(a) Tornado Warning Reception")
p2 <- bind_rows(
  s2 %>% filter(grpvar == "CWA") %>% top_n(5, `50%`),
  s2 %>% filter(grpvar == "CWA") %>% top_n(-5, `50%`)) %>% 
  mutate(grp_text = fct_reorder(grp_text, `50%`)) %>% 
  bind_rows(., s2 %>% filter(grpvar != "CWA")) %>% 
  ggplot(., aes(x = `50%`, y = grp_text)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `25%`, xmax = `75%`), height = 0.2) +
  xlim(30, 70) +
  facet_wrap(~grpvar, scales = "free_y") +
  theme_bw() +
  labs(x = "Estimate", y = "") +
  ggtitle("(b) Tornado Warning Comprehension (Subjective)")
p3 <- bind_rows(
  s3 %>% filter(grpvar == "CWA") %>% top_n(5, `50%`),
  s3 %>% filter(grpvar == "CWA") %>% top_n(-5, `50%`)) %>% 
  mutate(grp_text = fct_reorder(grp_text, `50%`)) %>% 
  bind_rows(., s3 %>% filter(grpvar != "CWA")) %>% 
  ggplot(., aes(x = `50%`, y = grp_text)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `25%`, xmax = `75%`), height = 0.2) +
  xlim(30, 70) +
  facet_wrap(~grpvar, scales = "free_y") +
  theme_bw() +
  labs(x = "Estimate", y = "") +
  ggtitle("(c) Tornado Warning Comprehension (Objective)")
p4 <- bind_rows(
  s4 %>% filter(grpvar == "CWA") %>% top_n(5, `50%`),
  s4 %>% filter(grpvar == "CWA") %>% top_n(-5, `50%`)) %>% 
  mutate(grp_text = fct_reorder(grp_text, `50%`)) %>% 
  bind_rows(., s4 %>% filter(grpvar != "CWA")) %>% 
  ggplot(., aes(x = `50%`, y = grp_text)) +
  geom_point() +
  geom_errorbarh(aes(xmin = `25%`, xmax = `75%`), height = 0.2) +
  xlim(30, 70) +
  facet_wrap(~grpvar, scales = "free_y") +
  theme_bw() +
  labs(x = "Estimate", y = "") +
  ggtitle("(d) Tornado Warning Response")

fig_1 <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave(paste0(file_path, "figures/Fig_1.png"), fig_1, width = 17, height = 7, dpi = "retina")

# Example Predictions in Norman -------------------------
set.seed(1)
round(pnorm(colMedians(posterior_predict(sfit1, newdata = tibble(MALE = 0, AGE_GROUP = 1, HISP = 0, RACE_GROUP = 1, TORN = 2.18, CWA = "OUN")))) * 100, 0)
round(pnorm(colMedians(posterior_predict(sfit2, newdata = tibble(MALE = 0, AGE_GROUP = 1, HISP = 0, RACE_GROUP = 1, TORN = 2.18, CWA = "OUN")))) * 100, 0)
round(pnorm(colMedians(posterior_predict(sfit3, newdata = tibble(MALE = 0, AGE_GROUP = 1, HISP = 0, RACE_GROUP = 1, TORN = 2.18, CWA = "OUN")))) * 100, 0)
round(pnorm(colMedians(posterior_predict(sfit4, newdata = tibble(MALE = 0, AGE_GROUP = 1, HISP = 0, RACE_GROUP = 1, TORN = 2.18, CWA = "OUN")))) * 100, 0)
census_data %>% filter(CWA == "OUN")
census_data %>% filter(CWA == "OUN")
census_data %>% filter(CWA == "TSA")

# Generate Predictions for Each Demographic Group, Weight, and Aggregate to CWA -------------------------
recep_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(sfit1, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(cwa_z = (person_z - mean(person_z))/sd(person_z), measure = "Reception")

subj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(sfit2, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(cwa_z = (person_z - mean(person_z))/sd(person_z), measure = "Comprehension (Subjective)")

obj_comp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(sfit3, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(cwa_z = (person_z - mean(person_z))/sd(person_z), measure = "Comprehension (Objective)")

resp_predictions <- census_data %>%
  mutate(person_z = colMedians(posterior_predict(sfit4, newdata = ., allow.new.levels = TRUE))) %>%
  mutate(person_z = person_z * DEMGRP_PROP) %>%
  group_by(CWA) %>%
  summarise(person_z = sum(person_z)) %>%
  mutate(cwa_z = (person_z - mean(person_z))/sd(person_z), measure = "Response")

all_predictions <- bind_rows(recep_predictions, subj_comp_predictions, obj_comp_predictions, resp_predictions)
all_predictions$person_percentile <- pnorm(all_predictions$person_z) * 100
all_predictions$cwa_percentile <- pnorm(all_predictions$cwa_z) * 100
all_predictions %>% 
  group_by(measure) %>% 
  summarize(min = min(person_percentile), max = max(person_percentile), range = min(person_percentile) - max(person_percentile), sd = sd(person_percentile)) %>% 
  mutate_if(is.numeric, round, 0)

# Plot Maps ----------------------------------------
setwd(paste0(file_path, "shapefile"))
cwa_shp <- readOGR('.','cwa_shapefile')

cwa_shp@data$id <- rownames(cwa_shp@data)
cwa_shp_points <- fortify(cwa_shp, region = "id")
cwa_shp_df <- plyr::join(cwa_shp_points, cwa_shp@data, by = "id")
cwa_shp_df <- left_join(cwa_shp_df, all_predictions, by = "CWA")
cwa_shp_df <- cwa_shp_df %>% drop_na(measure)
cwa_shp_df$measure <- factor(cwa_shp_df$measure, levels = c(
  "Reception",
  "Comprehension (Subjective)",
  "Comprehension (Objective)",
  "Response"
))  

fig2a <- cwa_shp_df %>% 
  ggplot(., aes(x = long, y = lat, group = group, fill = person_percentile)) +
  geom_polygon(color = "grey", size = 0.2) +
  scale_fill_gradient2(midpoint = 50, low = "darkred", high = "darkblue", mid = "white", limits = c(30, 70)) +
  facet_wrap(~measure, ncol = 2) +
  theme_bw() +
  labs(fill = "Average Person Percentile", title = "(a)") +
  map_theme + 
  coord_map("polyconic")

fig2b <- cwa_shp_df %>% 
  ggplot(., aes(x = person_percentile, y = ..density..)) +
  geom_histogram(alpha = 0.8, binwidth = 5, color = "grey20", fill = "grey20") +
  xlim(30, 70) +
  facet_wrap(~measure, ncol = 2) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.15, 0.05), labels = percent_format(accuracy = 5), limits = c(0, 0.15)) +
  labs(title = "(b)", y = "Density", x = "Average Person Percentile") +
  plot_theme

fig_2 <- gridExtra::grid.arrange(fig2a, fig2b, ncol = 2)
ggsave(paste0(file_path, "figures/Fig_2.png"), fig_2, width = 17, height = 6, dpi = "retina")

# Explore Differences within Regions -------------------------
all_predictions %>% filter(measure == "Comprehension (Subjective)", CWA %in% "OUN")
all_predictions %>% filter(measure == "Comprehension (Subjective)", CWA %in% "FWD")
all_predictions %>% filter(measure == "Comprehension (Objective)", CWA %in% "BMX")
all_predictions %>% filter(measure == "Comprehension (Objective)", CWA %in% "FFC")

# Validation -------------------------
os_data <- survey_data %>% filter(sample == "OS")

recep_means <- tibble(
  CWA = unique(os_data$CWA),
  measure = "Reception", 
  person_z_validation = predict(lmer(recep ~ 1 + (1|CWA), data = os_data), newdata = tibble(CWA = CWA)),
  cwa_z_validation = c(scale(person_z_validation)),
  person_percentile_validation = pnorm(person_z_validation) * 100, 
  cwa_percentile_validation = pnorm(cwa_z_validation) * 100)

subj_comp_means <- tibble(
  CWA = unique(os_data$CWA),
  measure = "Comprehension (Subjective)", 
  person_z_validation = predict(lmer(subj_comp ~ 1 + (1|CWA), data = os_data), newdata = tibble(CWA = CWA)),
  cwa_z_validation = c(scale(person_z_validation)),
  person_percentile_validation = pnorm(person_z_validation) * 100, 
  cwa_percentile_validation = pnorm(cwa_z_validation) * 100)

obj_comp_means <- tibble(
  CWA = unique(os_data$CWA),
  measure = "Comprehension (Objective)", 
  person_z_validation = predict(lmer(obj_comp ~ 1 + (1|CWA), data = os_data), newdata = tibble(CWA = CWA)),
  cwa_z_validation = c(scale(person_z_validation)),
  person_percentile_validation = pnorm(person_z_validation) * 100, 
  cwa_percentile_validation = pnorm(cwa_z_validation) * 100)

resp_means <- tibble(
  CWA = unique(os_data$CWA),
  measure = "Response", 
  person_z_validation = predict(lmer(resp ~ 1 + (1|CWA), data = os_data), newdata = tibble(CWA = CWA)), 
  cwa_z_validation = c(scale(person_z_validation)),
  person_percentile_validation = pnorm(person_z_validation) * 100, 
  cwa_percentile_validation = pnorm(cwa_z_validation) * 100)

agg_data <- bind_rows(recep_means, subj_comp_means, obj_comp_means, resp_means)
agg_data <- agg_data %>% 
  left_join(., all_predictions, by = c("CWA", "measure"))

agg_data <- fit_data %>% 
  group_by(CWA) %>% 
  summarise(n = n(), TORN = mean(TORN), region = getmode(CWA_REGION)) %>% 
  right_join(., agg_data, by = "CWA") %>% 
  mutate(person_percentile_difference = person_percentile - person_percentile_validation) %>% 
  mutate(cwa_percentile_difference = cwa_percentile - cwa_percentile_validation) 

agg_data$measure <- factor(agg_data$measure, levels = c(
  "Reception",
  "Comprehension (Subjective)",
  "Comprehension (Objective)",
  "Response"
))  

cors <- agg_data %>% 
  group_by(measure) %>% 
  summarise(cor = cor(person_z, person_z_validation, method = 'pearson'))
maes <- agg_data %>% 
  group_by(measure) %>% 
  summarise(mae = mean(abs(person_percentile_difference)))
stats_text <- tibble(label = paste0("r = ", sprintf("%.2f", round(cors$cor, 2)), "\nMD = ", sprintf("%.2f", round(maes$mae, 2))), measure = cors$measure)

agg_data$region <- as.factor(agg_data$region)
agg_data$measure <- as.factor(agg_data$measure)

diffs_fit <- lm(person_percentile_difference ~ region + measure, agg_data)
plot(effects::allEffects(diffs_fit))

top_diffs_recep <- agg_data %>% 
  filter(measure == "Reception") %>% 
  top_n(., n = 5, person_percentile_difference)
top_diffs_subj_comp <- agg_data %>% 
  filter(measure == "Comprehension (Subjective)") %>% 
  top_n(., n = 5, person_percentile_difference)
top_diffs_obj_comp <- agg_data %>% 
  filter(measure == "Comprehension (Objective)") %>% 
  top_n(., n = 5, person_percentile_difference)
top_diffs_resp <- agg_data %>% 
  filter(measure == "Response") %>% 
  top_n(., n = 5, person_percentile_difference)

bottom_diffs_recep <- agg_data %>% 
  filter(measure == "Reception") %>% 
  top_n(., n = -5, person_percentile_difference)
bottom_diffs_subj_comp <- agg_data %>% 
  filter(measure == "Comprehension (Subjective)") %>% 
  top_n(., n = -5, person_percentile_difference)
bottom_diffs_obj_comp <- agg_data %>% 
  filter(measure == "Comprehension (Objective)") %>% 
  top_n(., n = -5, person_percentile_difference)
bottom_diffs_resp <- agg_data %>% 
  filter(measure == "Response") %>% 
  top_n(., n = -5, person_percentile_difference)

diff_data <- bind_rows(top_diffs_recep, top_diffs_subj_comp, top_diffs_obj_comp, top_diffs_resp, 
                    bottom_diffs_recep, bottom_diffs_subj_comp, bottom_diffs_obj_comp, bottom_diffs_resp)
diff_data <- diff_data %>% 
  arrange(measure, person_percentile_difference) %>% 
  mutate(order = 1:40)

stats_text$measure <- factor(stats_text$measure, levels = c(
  "Reception",
  "Comprehension (Subjective)",
  "Comprehension (Objective)",
  "Response"
))  

stats_text$label <- factor(stats_text$label, levels = c(
  "r = 0.68\nMD = 4.40",
  "r = 0.75\nMD = 5.44",
  "r = 0.75\nMD = 3.06",
  "r = 0.43\nMD = 4.03"
)) 

fig3a <- ggplot(agg_data, aes(x = person_percentile_validation, y = person_percentile, label = CWA)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(color = "grey20", fill = "grey20", alpha = 0.8, size = 4) +
  facet_wrap(~measure, ncol = 2, scales = "free") +
  theme_bw() +
  geom_text(data = stats_text, aes(x = 65, y = 30, label = label), hjust = 0, size = 4, inherit.aes = FALSE) +
  plot_theme +
  labs(y = "Estimate", x = "Independent Survey Observation", title = "(a)") +
  lims(x = c(25, 75), y = c(25, 75))

fig3b <- ggplot(diff_data, aes(x = person_percentile_difference, y = order)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "grey20", fill = "grey20", alpha = 0.8, size = 5) +
  facet_wrap(~measure, nrow = 2, scales = "free_y") +
  scale_y_continuous(breaks = diff_data$order, labels = diff_data$CWA) +
  theme_bw() +
  plot_theme +
  labs(y = "", x = "Difference (Estimate vs. Observation)", title = "(b)")

fig_3 <- gridExtra::grid.arrange(fig3a, fig3b, ncol = 2)
ggsave(paste0(file_path, "figures/Fig_3.png"), fig_3, width = 17, height = 6, dpi = "retina")