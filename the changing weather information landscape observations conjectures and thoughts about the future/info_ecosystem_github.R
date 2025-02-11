library(tidyverse)
library(srvyr)
library(data.table)

#import survey data and calculate variables-----------------------------
data<-read_csv('filepath for info_landscape_data.csv here')
names(data)

#create generation variable
data <- data %>% 
  mutate(survey_year = as.numeric(survey_year)) %>% 
  mutate(year_born = survey_year - age) %>% 
  mutate(generation = case_when(
    year_born %in% 1928:1945 ~ "Silent",
    year_born %in% 1946:1964 ~ "Boomers",
    year_born %in% 1965:1980 ~ "Gen X",
    year_born %in% 1981:1996 ~ "Millennials",
    year_born %in% 1997:2012 ~ "Gen Z"
  )) %>% 
  mutate(generation = factor(generation, levels = c("Silent", "Boomers", "Gen X", "Millennials", "Gen Z")))

#calculate mean reliance by survey year
estimates_by_year <- data %>% 
  dplyr::select(survey_hazard, survey_year, weightfactor, `Broadcast\nradio`:`Sirens`) %>% 
  drop_na(weightfactor) %>% 
  pivot_longer(`Broadcast\nradio`:`Sirens`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(name, survey_hazard, survey_year) %>% 
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>% 
  filter(mean != 0)
estimates_by_year %>% print(n = Inf)

#calculate mean reliance by generation
estimates_by_generation <- data %>% 
  filter(survey_year %in% 2021:2023) %>% 
  dplyr::select(survey_hazard, generation, weightfactor, `Broadcast\nradio`:`Sirens`) %>% 
  drop_na(weightfactor, generation) %>% 
  pivot_longer(`Broadcast\nradio`:`Sirens`) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(name, generation) %>% 
  summarize(mean = survey_mean(value, vartype = "ci", na.rm = TRUE)) %>% 
  filter(mean != 0)
estimates_by_generation %>% print(n = Inf)

#create figure 1 -----------------------------
fig_1 <- ggplot(estimates_by_year %>% filter(survey_hazard == "WX"), aes(x = survey_year, y = mean, group = name)) +
  geom_line(size = 1, alpha = 0.5, color = "#585ea8") +
  geom_pointrange(aes(ymin = mean_low, ymax = mean_upp), color = "#585ea8") +
  geom_smooth(se = FALSE, method = lm, color = "#292e63") +
  facet_wrap(~fct_reorder(name, -mean), nrow = 1) +
  labs(x = "Year", y = "Reliance (Mean)", color = "",
       title = "Warnings and information about severe weather are available from multiple sources. How\nmuch do you, personally, rely on each of the following sources of information about\nextreme weather?",
       caption = "Source: Severe weather and society survey (2017-2023)") +
  scale_y_continuous(expand = c(0, 0), limits = c(2.2, 4.0)) +
  scale_x_continuous(breaks = 2017:2023, labels = 17:23) +
  theme_classic(base_size = 16) +
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        strip.background = element_blank(), 
        axis.text.x = element_text(size = 9),
        title = element_text(size = 14))
fig_1

#create figure 2 -----------------------------
fig_2a <- ggplot(estimates_by_year %>% filter(survey_hazard == "TC"), aes(x = survey_year, y = mean, group = name)) +
  geom_line(size = 0.5, alpha = 0.5, color = "#585ea8") +
  geom_pointrange(aes(ymin = mean_low, ymax = mean_upp), color = "#585ea8", size = 0.3) +
  geom_smooth(se = FALSE, method = lm, color = "#292e63") +
  facet_wrap(~fct_reorder(name, -mean), nrow = 1) +
  labs(x = "Year", y = "Reliance (Mean)", color = "",
       title = "Information about hurricanes is available from multiple sources. How much do you, personally,\nrely on each of the following sources of information?",
       caption = "Source: Tropical cyclone and society survey (2020-2023)") +
  scale_y_continuous(expand = c(0, 0), limits = c(2.2, 4.0)) +
  scale_x_continuous(breaks = 2017:2023, labels = 17:23) +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"),
        strip.background = element_blank(), 
        axis.text = element_text(size = 11),
        title = element_text(size = 16),
        strip.text = element_text(size = 14))
fig_2b <- ggplot(estimates_by_year %>% filter(survey_hazard == "WW"), aes(x = survey_year, y = mean, group = name)) +
  geom_line(size = 0.5, alpha = 0.5, color = "#585ea8") +
  geom_pointrange(aes(ymin = mean_low, ymax = mean_upp), color = "#585ea8", size = 0.3) +
  geom_smooth(se = FALSE, method = lm, color = "#292e63") +
  facet_wrap(~fct_reorder(name, -mean), nrow = 1) +
  labs(x = "Year", y = "Reliance (Mean)", color = "",
       title = "When winter weather threatens your area, how much do you rely on the following channels\nof information?",
       caption = "Source: Winter weather and society survey (2021-2023)") +
  scale_y_continuous(expand = c(0, 0), limits = c(2.2, 4.0)) +
  scale_x_continuous(breaks = 2017:2023, labels = 17:23) +
  theme_classic(base_size = 12) +
  theme(plot.margin = margin(0.5, 0.25, 0.25, 0.25, "cm"),
        strip.background = element_blank(), 
        axis.text = element_text(size = 11),
        title = element_text(size = 16),
        strip.text = element_text(size = 14))
fig_2 <- cowplot::plot_grid(fig_2a, fig_2b, align = "h",ncol = 1)
fig_2

#create figure 3 -----------------------------
fig_3 <- ggplot(estimates_by_generation, aes(x = generation, y = mean, group = name)) +
  geom_line(size = 1, alpha = 0.5, color = "#585ea8") +
  geom_pointrange(aes(ymin = mean_low, ymax = mean_upp), color = "#585ea8") +
  geom_smooth(se = FALSE, method = lm, color = "#292e63") +
  facet_wrap(~fct_reorder(name, -mean), nrow = 1) +
  labs(x = "Generation", y = "Reliance (Mean)", color = "",
       title = "Warnings and information about severe weather are available from multiple sources. How\nmuch do you, personally, rely on each of the following sources of information about\nextreme weather?",
       caption = "Source: Severe, tropical, and winter weather and society survey (2021-2023)") +
  scale_y_continuous(expand = c(0, 0), limits = c(1.5, 4.3)) +
  theme_classic(base_size = 16) +
  theme(plot.margin = margin(0.25, 0.25, 0.25, 0.25, "cm"),
        strip.background = element_blank(), 
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        title = element_text(size = 14))
fig_3
