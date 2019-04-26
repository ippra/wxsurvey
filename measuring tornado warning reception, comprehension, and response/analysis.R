# Content Order: Table 1-5, Scale Development, Figure 1, Table 6, Figure 2, Footnote 2, Table 7, Figure 3, Appendix Table A1
library(survey)
library(car)
library(Rcpp)
library(data.table)
library(ltm)
library(psych)
library(stargazer)
library(zeligverse)
library(weights)
library(tidyverse)
options(scipen = 9999)

# Import Survey Data -----------------------------
WX18 <- read_csv("https://raw.githubusercontent.com/oucrcm/wxsurvey/master/measuring%20tornado%20warning%20reception%2C%20comprehension%2C%20and%20response/data.csv")

# Table 1: Demographics -----------------------------
round(prop.table(table(WX18$gend)) * 100, 1) # Gender
WX18$age_category <- NA # Make Age Categories
WX18$age_category <- ifelse(WX18$age >= 18 & WX18$age <= 24, 1, WX18$age_category)
WX18$age_category <- ifelse(WX18$age >= 25 & WX18$age <= 34, 2, WX18$age_category)
WX18$age_category <- ifelse(WX18$age >= 35 & WX18$age <= 44, 3, WX18$age_category)
WX18$age_category <- ifelse(WX18$age >= 45 & WX18$age <= 54, 4, WX18$age_category)
WX18$age_category <- ifelse(WX18$age >= 55 & WX18$age <= 64, 5, WX18$age_category)
WX18$age_category <- ifelse(WX18$age >= 65, 6, WX18$age_category)
round(prop.table(table(WX18$age_category)) * 100, 1) # age
round(prop.table(table(WX18$hisp)) * 100, 1) # hispanic
round(prop.table(table(WX18$race)) * 100, 1) # race
round(prop.table(table(WX18$nws_region)) * 100, 1) # region

# Add Weights to Data for Tables 2-5 -----------------------------
data.wtd <- svydesign(ids = ~1, weights = ~weightfactor, data = WX18)

# Table 2: Tornado Warning Reception -----------------------------
round(prop.table(svytable(~rec_all, design = data.wtd)) * 100, 1) # rec_all
round(prop.table(svytable(~rec_most, design = data.wtd)) * 100, 1) # rec_most
round(prop.table(svytable(~rec_soon, design = data.wtd)) * 100, 1) # rec_soon
round(prop.table(svytable(~rec_sleep, design = data.wtd)) * 100, 1) # rec_sleep
round(prop.table(svytable(~rec_driving, design = data.wtd)) * 100 , 1) # rec_car
round(prop.table(svytable(~rec_work, design = data.wtd)) * 100, 1) #rec_work
round(prop.table(svytable(~rec_store,design = data.wtd)) * 100, 1)# rec_store
round(prop.table(svytable(~rec_small_group, design = data.wtd)) * 100, 1) # rec_sm_group
round(prop.table(svytable(~rec_large_group, design = data.wtd)) * 100, 1) # rec_lg_group
round(prop.table(svytable(~rec_morn, design = data.wtd)) * 100,1) # rec_morn
round(prop.table(svytable(~rec_aft, design = data.wtd)) * 100,1) # rec_aft
round(prop.table(svytable(~rec_eve, design = data.wtd)) * 100,1) # rec_eve

# Table 3: Objective Tornado Warning Comprehension -----------------------------
round(prop.table(svytable(~torwatch, design = data.wtd)) * 100, 1) # ocomp_ww_difference
round(prop.table(svytable(~torwarn, design = data.wtd)) * 100, 1) # ocomp_watch_warn
round(prop.table(svytable(~warn_time, design = data.wtd)) * 100, 1) # ocomp_warn_time
data.wtd$variables$warn_time_correct <- ifelse(data.wtd$variables$warn_time == 1 & 
                                                 data.wtd$variables$warn_time_minutes < 30, 1, 0)
round(prop.table(svytable(~warn_time_correct, design = data.wtd)) * 100, 1) # ocomp_warn_time
round(prop.table(svytable(~warn_size,design = data.wtd)) * 100, 1) # ocomp_warn_size
round(prop.table(svytable(~watch_time,design = data.wtd)) * 100,1) # ocomp_watch_time
data.wtd$variables$watch_time_correct <- ifelse(data.wtd$variables$watch_time == 2 & data.wtd$variables$watch_time_hours >= 1 & 
                                                  data.wtd$variables$watch_time_hours <= 3, 1, 0)
round(prop.table(svytable(~watch_time_correct, design = data.wtd)) * 100, 1) # ocomp_watch_time
round(prop.table(svytable(~watch_size,design = data.wtd)) * 100, 1) # ocomp_watch_size

# Table 4: Subjective Tornado Warning Comprehension -----------------------------
round(prop.table(svytable(~alert_und,design=data.wtd)) * 100, 1) # scomp_ww_difference
round(prop.table(svytable(~tor_watchwarn_und,design = data.wtd)) * 100, 1) # scomp_ww_understanding
round(prop.table(svytable(~svr_watchwarn_und,design = data.wtd)) * 100, 1) # scomp_severe_thund
round(prop.table(svytable(~tor_map_und,design = data.wtd)) * 100, 1) # scomp_maps
round(prop.table(svytable(~tor_radar_und,design = data.wtd)) * 100, 1) # scomp_radar
round(prop.table(svytable(~und_morn, design = data.wtd)) * 100,1) # scomp_morn
round(prop.table(svytable(~und_aft, design = data.wtd))* 100,1) # scomp_aft
round(prop.table(svytable(~und_eve, design = data.wtd)) * 100,1) # scomp_eve

# Table 5: Tornado Warning Response -----------------------------
round(prop.table(svytable(~resp_prot,design = data.wtd)) * 100, 1) # resp_always
round(prop.table(svytable(~resp_sleep, design = data.wtd)) * 100, 1) # resp_sleep
round(prop.table(svytable(~resp_driving, design = data.wtd)) * 100, 1) # resp_car
round(prop.table(svytable(~resp_work, design = data.wtd)) * 100, 1) # resp_work
round(prop.table(svytable(~resp_store, design = data.wtd)) * 100, 1) # resp_store
round(prop.table(svytable(~resp_small_group, design = data.wtd)) * 100, 1) # resp_sm_group
round(prop.table(svytable(~resp_large_group, design = data.wtd)) * 100, 1) # resp_lg_group
round(prop.table(svytable(~resp_morn, design = data.wtd)) * 100, 1) # resp_morn
round(prop.table(svytable(~resp_aft, design = data.wtd)) * 100, 1) # resp_aft
round(prop.table(svytable(~resp_eve, design = data.wtd)) * 100, 1) # resp_eve

# Subjective Reception Scale -----------------------------
recep_data <- WX18 %>% 
  dplyr::select(rec_all, rec_most, rec_soon, rec_sleep, rec_driving, rec_work, rec_store, rec_small_group, rec_large_group, 
                rec_morn, rec_aft, rec_eve)
recep_eigen_values <- c(scree(recep_data, factors = TRUE, pc = FALSE))$fv
recep_fact_loadings <- loadings(factanal(na.omit(recep_data), factors = 1))
recep_fit <- grm(recep_data)
summary(recep_fit)
recep_scores <- ltm::factor.scores(recep_fit, resp.patterns = recep_data)
WX18$reception <- recep_scores$score.dat$z1

# Subjective Comprehension Scale -----------------------------
subj_comp_data <- WX18 %>% 
  dplyr::select(alert_und, tor_watchwarn_und, tor_map_und, tor_radar_und, svr_watchwarn_und, und_morn, und_aft, und_eve)
subj_comp_eigen_values <- c(scree(subj_comp_data, factors = TRUE, pc = FALSE))$fv
subj_comp_fact_loadings <- loadings(factanal(na.omit(subj_comp_data), factors = 1))
subj_comp_fit <- grm(subj_comp_data)
subj_comp_scores <- ltm::factor.scores(subj_comp_fit, resp.patterns = subj_comp_data)
WX18$subj_comprehension <- subj_comp_scores$score.dat$z1

# Objective Comprehension Scale -----------------------------
WX18$watch_warn_group <- ifelse(is.na(WX18$torwatch) == FALSE, "watch", "warn")
WX18$watch_warn_correct <- NA
WX18$watch_warn_correct <- ifelse(WX18$watch_warn_group == "watch" & WX18$torwatch == 1, 1, WX18$watch_warn_correct)
WX18$watch_warn_correct <- ifelse(WX18$watch_warn_group == "watch" & WX18$torwatch != 1, 0, WX18$watch_warn_correct)
WX18$watch_warn_correct <- ifelse(WX18$watch_warn_group == "warn" & WX18$torwarn == 2, 1, WX18$watch_warn_correct)
WX18$watch_warn_correct <- ifelse(WX18$watch_warn_group == "warn" & WX18$torwarn != 2, 0, WX18$watch_warn_correct)
WX18$warn_time_correct <- ifelse(WX18$warn_time == 1 & WX18$warn_time_minutes < 30, 1, 0)
WX18$watch_time_correct <- ifelse(WX18$watch_time == 2 & WX18$watch_time_hours >= 1 & WX18$watch_time_hours <= 3, 1, 0)
WX18$warn_size_correct <- ifelse(WX18$warn_size == 1 | WX18$warn_size == 2, 1, 0)
WX18$watch_size_correct <- ifelse(WX18$watch_size == 3 | WX18$watch_size == 4 | WX18$watch_size == 5, 1, 0)
obj_comp_data <- WX18 %>% 
  dplyr::select(watch_warn_correct, warn_time_correct, watch_time_correct, watch_size_correct)
obj_comp_eigen_values <- c(scree(obj_comp_data, factors = TRUE, pc = FALSE))$fv
obj_comp_fact_loadings <- loadings(factanal(na.omit(obj_comp_data), factors = 1, rotation = "varimax"))
obj_comp_fit <- ltm(obj_comp_data ~ z1)
obj_comp_scores <- ltm::factor.scores(obj_comp_fit, resp.patterns = obj_comp_data)
WX18$obj_comprehension <- obj_comp_scores$score.dat$z1

# Subjective Response Scale -----------------------------
resp_data <- WX18 %>% 
  dplyr::select(resp_prot, resp_sleep, resp_driving, resp_work, resp_store, resp_small_group, resp_large_group,
                resp_morn, resp_aft, resp_eve)
resp_eigen_values <- c(scree(resp_data, factors = TRUE, pc = FALSE))$fv
resp_fact_loadings <- loadings(factanal(na.omit(resp_data), factors = 1))
resp_fit <- grm(resp_data)
resp_scores <- ltm::factor.scores(resp_fit, resp.patterns = resp_data)
WX18$response <- resp_scores$score.dat$z1

# Figure 1: Scree Plots -----------------------------
scree_data <- data_frame(
  Variable = c(rep("Reception", 8), rep("Comprehension (Subjective)", 8), rep("Response", 8)),
  Factors = rep(1:8, 3),
  Value = c(recep_eigen_values[1:8], subj_comp_eigen_values[1:8], resp_eigen_values[1:8]))
scree_data$Variable <- factor(scree_data$Variable, levels = c("Reception", "Comprehension (Subjective)", "Response"))

fig_1 <- ggplot(scree_data, aes(x = Factors, y = Value)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  scale_x_continuous(1:8, 1:8, name = "Factors") +
  scale_y_continuous(0:5, 0:5, name = "Eigenvalues") +
  theme_bw() +
  facet_wrap(~ Variable)
ggsave(file = "~/Dropbox/WX Summer Fun/paper_1_figures/fig_1.png", fig_1, width = 10, height = 2, dpi = 500)

# Table 6: Objective Comprehension Correlations -----------------------------
polycor::polychor(WX18$watch_warn_correct, WX18$warn_time_correct, std.err = TRUE)
polycor::polychor(WX18$watch_warn_correct, WX18$warn_size_correct, std.err = TRUE)
polycor::polychor(WX18$watch_warn_correct, WX18$watch_time_correct, std.err = TRUE)
polycor::polychor(WX18$watch_warn_correct, WX18$watch_size_correct, std.err = TRUE)
polycor::polychor(WX18$warn_time_correct, WX18$warn_size_correct, std.err = TRUE)
polycor::polychor(WX18$warn_time_correct, WX18$watch_time_correct, std.err = TRUE)
polycor::polychor(WX18$warn_time_correct, WX18$watch_size_correct, std.err = TRUE)
polycor::polychor(WX18$warn_size_correct, WX18$watch_time_correct, std.err = TRUE)
polycor::polychor(WX18$warn_size_correct, WX18$watch_size_correct, std.err = TRUE)
polycor::polychor(WX18$watch_time_correct, WX18$watch_size_correct, std.err = TRUE)

# Figure 2: IRT Plots -----------------------------
x1 <- data.frame(plot(recep_fit, "IIC", item = 0, zrange = c(-5, 5)))
information(recep_fit, range = c(-100, 0))
information(recep_fit, range = c(0, 100))
names(x1) <- c("z", "info")
x2 <- data.frame(plot(recep_fit, "IIC", item = 1, zrange = c(-5, 5)))
information(recep_fit, item = 1, range = c(-100, 0))
information(recep_fit, item = 1, range = c(0, 100))
names(x2) <- c("z", "info")
x1$group <- "Test Information Function"
x2$group <- "Item Information Function"
recep_lines <- rbind(x1, x2)

x1 <- data.frame(plot(obj_comp_fit, "IIC", item = 0, zrange = c(-5, 5)))
information(obj_comp_fit, range = c(-100, 0))
information(obj_comp_fit, range = c(0, 100))
names(x1) <- c("z", "info")
x2 <- data.frame(plot(obj_comp_fit, "IIC", item = 1, zrange = c(-5, 5)))
information(obj_comp_fit, item = 1, range = c(-100, 0))
information(obj_comp_fit, item = 1, range = c(0, 100))
names(x2) <- c("z", "info")
x1$group <- "Test Information Function"
x2$group <- "Item Information Function"
obj_comp_lines <- rbind(x1, x2)

x1 <- data.frame(plot(subj_comp_fit, "IIC", item = 0, zrange = c(-5, 5)))
information(subj_comp_fit, range = c(-100, 0))
information(subj_comp_fit, range = c(0, 100))
names(x1) <- c("z", "info")
x2 <- data.frame(plot(subj_comp_fit, "IIC", item = 1, zrange = c(-5, 5)))
information(subj_comp_fit, item = 1, range = c(-100, 0))
information(subj_comp_fit, item = 1, range = c(0, 100))
names(x2) <- c("z", "info")
x1$group <- "Test Information Function"
x2$group <- "Item Information Function"
subj_comp_lines <- rbind(x1, x2)

x1 <- data.frame(plot(resp_fit, "IIC", item = 0, zrange = c(-5, 5)))
information(resp_fit, range = c(-100, 0))
information(resp_fit, range = c(0, 100))
names(x1) <- c("z", "info")
x2 <- data.frame(plot(resp_fit, "IIC", item = 1, zrange = c(-5, 5)))
information(resp_fit, item = 1, range = c(-100, 0))
information(resp_fit, item = 1, range = c(0, 100))
names(x2) <- c("z", "info")
x1$group <- "Test Information Function"
x2$group <- "Item Information Function"
resp_lines <- rbind(x1, x2)

recep_lines$Variable <- "Reception"
obj_comp_lines$Variable <- "Comprehension (Objective)"
subj_comp_lines$Variable <- "Comprehension (Subjective)"
resp_lines$Variable <- "Response"

lines <- rbind(recep_lines, obj_comp_lines, subj_comp_lines, resp_lines)
lines$Variable <- factor(lines$Variable, levels = c("Reception",
                                                    "Comprehension (Objective)",
                                                    "Comprehension (Subjective)",
                                                    "Response"))

lines1 <- filter(filter(lines, group == "Test Information Function"))
lines2 <- filter(filter(lines, group == "Item Information Function"))
lines2$Variable <- car::recode(lines2$Variable, 
                               "'Reception' = 'Rec_All';
                               'Comprehension (Objective)' = 'Ocomp_WW_Difference';
                               'Comprehension (Subjective)' = 'Scomp_WW_Difference';
                               'Response' = 'Resp_Always'")
lines2$Variable <- factor(lines2$Variable, levels = c("Rec_All",
                                                      "Ocomp_WW_Difference",
                                                      "Scomp_WW_Difference",
                                                      "Resp_Always"))

p1 <- ggplot(lines1, aes(x = z, y = info)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(y = "Information", x = "Scale Score") +
  theme(legend.position = "none") +
  ggtitle("(a) Full Test Information Functions") +
  facet_wrap(~ Variable, scale = "free", nrow = 1)

p2 <- ggplot(lines2, aes(x = z, y = info)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(y = "Information", x = "Scale Score") +
  theme(legend.position = "none") +
  ggtitle("(b) Single Item Information Functions") +
  facet_wrap(~ Variable, scale = "free", nrow = 1)
fig_2 <- gridExtra::grid.arrange(p1, p2)
ggsave(file = "~/Dropbox/WX Summer Fun/paper_1_figures/fig_2.png", fig_2, width = 10, height = 4, dpi = 500)

# Footnote 2: Dimensionality for Objective Comprehension -----------------------------
obj_comp_data <- WX18 %>% # scale with warning size
  dplyr::select(watch_warn_correct, 
                warn_time_correct, watch_time_correct,
                watch_size_correct, warn_size_correct)
obj_comp_fit <- ltm(obj_comp_data ~ z1)

obj_comp_data <- WX18 %>% # scale without warning size
  dplyr::select(watch_warn_correct, 
                warn_time_correct, watch_time_correct,
                watch_size_correct)
obj_comp_fit <- ltm(obj_comp_data ~ z1)

unidimTest(obj_comp_fit)

# Recode/Rename Survey Variables -----------------------------
WX18$MALE <- factor(WX18$gend)
WX18$AGE <- WX18$age
WX18$HISP <-factor(WX18$hisp)
WX18$RACE_GROUP <- factor(car::recode(WX18$race, "1 = 1; 2 = 2; else = 3"))

WX18$MALE <- factor(WX18$MALE, labels = c("Female", "Male"))
WX18$HISP <- factor(WX18$HISP, labels = c("Non-Hisp", "Hispanic"))
WX18$RACE_GROUP <- factor(WX18$RACE_GROUP, labels = c("White", "Black", "Other"))

# Table 7: Fit Linear Models -----------------------------
reception_fit <- lm(reception ~ MALE + poly(AGE, 3) + HISP + RACE_GROUP + poly(WARNING_AVG, 3), data = WX18)
obj_comprehension_fit <- lm(obj_comprehension ~ MALE + poly(AGE, 3) + HISP + RACE_GROUP + poly(WARNING_AVG, 3), data = WX18)
subj_comprehension_fit <- lm(subj_comprehension ~ MALE + poly(AGE, 3) + HISP + RACE_GROUP + poly(WARNING_AVG, 3), data = WX18)
response_fit <- lm(response ~ MALE + poly(AGE, 3) + HISP + RACE_GROUP + poly(WARNING_AVG, 3), data = WX18)
stargazer(reception_fit, obj_comprehension_fit, subj_comprehension_fit, response_fit, digits = 2, type = "text", single.row = TRUE)

# Figure 3: Plot Linear Models -----------------------------
reception_fit <- zelig(reception ~ MALE + AGE + I(AGE^2) + I(AGE^3) + HISP + RACE_GROUP + WARNING_AVG + I(WARNING_AVG^2) + I(WARNING_AVG^3), data = WX18, model = "ls")
obj_comprehension_fit <- zelig(obj_comprehension ~ MALE + AGE + I(AGE^2) + I(AGE^3) + HISP + RACE_GROUP + WARNING_AVG + I(WARNING_AVG^2) + I(WARNING_AVG^3), data = WX18, model = "ls")
subj_comprehension_fit <- zelig(subj_comprehension ~ MALE + AGE + I(AGE^2) + I(AGE^3) + HISP + RACE_GROUP + WARNING_AVG + I(WARNING_AVG^2) + I(WARNING_AVG^3), data = WX18, model = "ls")
response_fit <- zelig(response ~ MALE + AGE + I(AGE^2) + I(AGE^3) + HISP + RACE_GROUP + WARNING_AVG + I(WARNING_AVG^2) + I(WARNING_AVG^3), data = WX18, model = "ls")

quantile(WX18$WARNING_AVG, c(0.05, 0.95))
quantile(WX18$AGE, c(0.05, 0.95))

warn_est <- rbind(
  setx(reception_fit, WARNING_AVG = 0:60) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Reception"),
  setx(obj_comprehension_fit, WARNING_AVG = 0:60) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Comprehension (Objective)"),
  setx(subj_comprehension_fit, WARNING_AVG = 0:60) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Comprehension (Subjective)"),
  setx(response_fit, WARNING_AVG = 0:60) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Response"))
warn_est %>% filter(WARNING_AVG %in% c(5, 40))

age_est <- rbind(
  setx(reception_fit, AGE = 20:75) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Reception"),
  setx(obj_comprehension_fit, AGE = 20:75) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Comprehension (Objective)"),
  setx(subj_comprehension_fit, AGE = 20:75) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Comprehension (Subjective)"),
  setx(response_fit, AGE = 20:75) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer() %>% mutate(measure = "Response"))
age_est %>% filter(AGE %in% c(35, 60))

reception_est <- rbind(
  setx(reception_fit, MALE = c("Male", "Female")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(reception_fit, HISP = c("Non-Hisp", "Hispanic")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(reception_fit, RACE_GROUP = c("White", "Black", "Other")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer())
reception_est$grp <- c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other")
reception_est$measure <- "Reception"

obj_comprehension_est <- rbind(
  setx(obj_comprehension_fit, MALE = c("Male", "Female")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(obj_comprehension_fit, HISP = c("Non-Hisp", "Hispanic")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(obj_comprehension_fit, RACE_GROUP = c("White", "Black", "Other")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer())
obj_comprehension_est$grp <- c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other")
obj_comprehension_est$measure <- "Comprehension (Objective)"

subj_comprehension_est <- rbind(
  setx(subj_comprehension_fit, MALE = c("Male", "Female")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(subj_comprehension_fit, HISP = c("Non-Hisp", "Hispanic")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(subj_comprehension_fit, RACE_GROUP = c("White", "Black", "Other")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer())
subj_comprehension_est$grp <- c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other")
subj_comprehension_est$measure <- "Comprehension (Subjective)"

response_est <- rbind(
  setx(response_fit, MALE = c("Male", "Female")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(response_fit, HISP = c("Non-Hisp", "Hispanic")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer(),
  setx(response_fit, RACE_GROUP = c("White", "Black", "Other")) %>% sim() %>% zelig_qi_to_df() %>% qi_slimmer())
response_est$grp <- c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other")
response_est$grp <- c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other")
response_est$measure <- "Response"

all_estimates <- rbind(reception_est, obj_comprehension_est, subj_comprehension_est, response_est)
all_estimates$measure <- factor(all_estimates$measure, levels = c("Reception", "Comprehension (Objective)", "Comprehension (Subjective)", "Response"))
all_estimates$grp <- factor(all_estimates$grp, levels = c("Male", "Female", "Non-Hisp", "Hispanic", "White", "Black", "Other"))

warn_est$measure <- factor(warn_est$measure, levels = c("Reception", "Comprehension (Objective)", "Comprehension (Subjective)", "Response"))
age_est$measure <- factor(age_est$measure, levels = c("Reception", "Comprehension (Objective)", "Comprehension (Subjective)", "Response"))

p1 <- ggplot(warn_est, aes(y = qi_ci_median, x = WARNING_AVG)) +
  # geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.5) +
  ylim(-0.5, 0.5) +
  labs(x = "Average Number of Tornado Warnings (Year)", y = expression("Scale Score ("*sigma*")")) +
  theme_bw() +
  ggtitle("(a) The Effect of Tornado Warning Exposure on Scale Scores") +
  facet_wrap(~ measure, nrow = 1)
p2 <- ggplot(age_est, aes(y = qi_ci_median, x = AGE)) +
  # geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  geom_ribbon(aes(ymin = qi_ci_min, ymax = qi_ci_max), alpha = 0.5) +
  ylim(-0.5, 0.5) +
  labs(x = "Age", y = expression("Scale Score ("*sigma*")")) +
  theme_bw() +
  ggtitle("(b) The Effect of Age on Scale Scores") +
  facet_wrap(~ measure, nrow = 1)
p3 <- ggplot(all_estimates, aes(y = qi_ci_median, x = grp)) +
  # geom_hline(yintercept = 0, linetype = 2) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = c(3 - 0.5, 5 - 0.5)) +
  geom_errorbar(aes(ymin = qi_ci_min, ymax = qi_ci_max), width = 0, size = 1, alpha = 0.5) +
  ylim(-0.5, 0.5) +
  labs(x = "Demographic Group", y = expression("Scale Score ("*sigma*")")) +
  guides(color = FALSE) +
  theme_bw() +
  ggtitle("(c) The Effect of Gender, Ethnicity, and Race on Scale Scores") +
  theme(axis.text.x = element_text(size = 8, angle = 70, hjust = 1)) +
  facet_wrap(~ measure, nrow = 1)
fig_3 <- gridExtra::grid.arrange(p1, p2, p3, ncol = 1)
ggsave(file = "~/Dropbox/WX Summer Fun/paper_1_figures/fig_3.png", fig_3, width = 10, height = 8, dpi = 500)

# Appendix, Table A1 -----------------------------
# RECEPTION TEST
rec_fit <- grm(recep_data)
information(rec_fit, c(-10, 0), items = 1) # replace "items = X" with 1-12, for each item information
information(rec_fit, c(0, 10), items = 1)

information(rec_fit, c(-10, 0))
information(rec_fit, c(0, 10))

# OBJECTIVE COMPREHENSION TEST
ocomp_fit <- ltm(obj_comp_data ~ z1)
information(ocomp_fit, c(-10, 0), items = 1) # replace "items = X" with 1-4, for each item information
information(ocomp_fit, c(0, 10), items = 1)

information(ocomp_fit, c(-10, 0))
information(ocomp_fit, c(0, 10))

# SUBJECTIVE COMPREHENSION TEST
scomp_fit <- grm(subj_comp_data)
information(scomp_fit, c(-10, 0), items = 1) # replace "items = X" with 1-8, for each item information
information(scomp_fit, c(0, 10), items = 1)

information(scomp_fit, c(-10, 0))
information(scomp_fit, c(0, 10))

# RESPONSE TEST
resp_fit <- grm(resp_data)
information(resp_fit, c(-10, 0), items = 1) # replace "items = X" with 1-10, for each item information
information(resp_fit, c(0, 10), items = 1)

information(resp_fit, c(-10, 0))
information(resp_fit, c(0, 10))

# Cronbachs Aplha
psych::alpha(recep_data) #0.90
obj_comp_cor_matrix <- as.matrix(data.frame(
  A = c(1.00,	0.28,	0.17,	0.22),
  B = c(0.28,	1.00,	0.31,	0.19),
  C = c(0.17,	0.31,	1.00,	0.06),
  D = c(0.22,	0.19,	0.06,	1.00)))
rownames(obj_comp_cor_matrix) <- colnames(obj_comp_cor_matrix)
psych::alpha(obj_comp_cor_matrix) #0.51
psych::alpha(subj_comp_data) #0.9
psych::alpha(resp_data) #0.91