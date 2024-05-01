library(tidyverse)
library(data.table)
library(sf)
library(survey)
library(srvyr)


# Warning comprehension (fig. 3) -----------------------------
#Import both Spanish and English data
survey_data<- read_csv("https://www.dropbox.com/scl/fi/hiwyib00nsqif0hrdu0vj/survey_data.csv?rlkey=yb6uls25gmnmw20ccah2777u4&dl=1") 

#Transform data
warning_comp <- survey_data %>% 
     select(weightfactor, lang, 
            `(a) Tornado Watch Understanding` = torwatch, 
            `(b) Tornado Warning Understanding` = torwarn) %>% 
     pivot_longer(`(a) Tornado Watch Understanding`:`(b) Tornado Warning Understanding`) %>% 
     drop_na() %>% 
     as_survey_design(ids = 1, weights = weightfactor) %>% 
     group_by(lang, name, value) %>%
     summarize(p = survey_mean(vartype = "ci")) %>% 
     mutate(labels = case_when(
       name == "(a) Tornado Watch Understanding" & value == 1 ~ "(1)\nTornado\nWatch",
       name == "(a) Tornado Watch Understanding" & value == 2 ~ "(2)\nTornado\nWarning",
       name == "(a) Tornado Watch Understanding" & value == 3 ~ "(3)\nDon't\nknow",
       name == "(b) Tornado Warning Understanding" & value == 1 ~ "(1)\nTornado\nWatch",
       name == "(b) Tornado Warning Understanding" & value == 2 ~ "(2)\nTornado\nWarning",
       name == "(b) Tornado Warning Understanding" & value == 3 ~ "(3)\nDon't\nknow"))

#Create plot
p <- ggplot(warning_comp, aes(x = labels, y = p, fill = lang)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = p_low, ymax = p_upp), 
                position = position_dodge(width = 0.9), 
                size = 1, 
                width = 0.2) + 
  facet_wrap(~name, scales = "free_x") +
  labs(x = "Response", y = "Respondents", fill = "") +
  theme_classic(base_size = 12) +
     theme(legend.position = c(0.05, 0.9)) +
     scale_fill_grey(start = 0.3, end = 0.7) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15, face = "bold"),
        legend.background = element_blank())
p

# Translations (fig. 4) -----------------------------
#Import just Spanish data
sp_data<- read_csv("https://www.dropbox.com/scl/fi/zjj7663gjltgoh40rimdn/sp_data.csv?rlkey=htbhakx9234kl595g96l9ximy&dl=1") 

#Transform data and calculate means
sp_data <- sp_data %>% 
  mutate(HISP = case_when(
    hisp == 0 ~ "(f) (Heritage)\nNot Hispanic or Latinx",
    hisp == 1 ~ "(g) (Heritage)\nMexican or Chicano",
    hisp == 2 ~ "(h) (Heritage)\nPuerto Rican",
    hisp == 3 ~ "(i) (Heritage)\nCuban",
    hisp == 4 ~ "(j) (Heritage)\nOther Hispanic")) %>% 
  mutate(ENGLISH = case_when(
    english == 0 ~ "(b) (Speak English)\nNo, not at all",
    english == 1 ~ "(c) (Speak English)\nYes, but not very well",
    english == 2 ~ "(d) (Speak English)\nYes, well",
    english == 3 ~ "(e) (Speak English)\nYes, very well"))

all_means <- sp_data %>% 
  select(weightfactor, urg_adv:urg_eme) %>% 
  pivot_longer(urg_adv:urg_eme) %>% 
  mutate(name = factor(name, levels = c("urg_eme", "urg_ame", "urg_ale", "urg_adv", "urg_avi", "urg_vig"),
                       labels = c("Emergencia", "Amenaza", "Alerta", "Advertencia", "Aviso**", "Vigilancia*"))) %>%
  drop_na() %>% 
  as_survey_design(ids = 1, weights = weightfactor) %>% 
  group_by(name) %>%
  summarize(mean = survey_mean(value, vartype = "ci")) %>% 
  mutate(group = "(a) Full sample")

english_means <- sp_data %>% 
  select(weightfactor, ENGLISH, urg_adv:urg_eme) %>% 
  pivot_longer(urg_adv:urg_eme) %>% 
  mutate(name = factor(name, levels = c("urg_eme", "urg_ame", "urg_ale", "urg_adv", "urg_avi", "urg_vig"),
                       labels = c("Emergencia", "Amenaza", "Alerta", "Advertencia", "Aviso**", "Vigilancia*"))) %>%
  drop_na() %>% 
  as_survey_design(ids = 1, weights = weightfactor) %>% 
  group_by(ENGLISH, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci")) %>% 
  rename("group" = "ENGLISH")

hisp_means <- sp_data %>% 
  select(weightfactor, HISP, urg_adv:urg_eme) %>% 
  pivot_longer(urg_adv:urg_eme) %>% 
  mutate(name = factor(name, levels = c("urg_eme", "urg_ame", "urg_ale", "urg_adv", "urg_avi", "urg_vig"),
                       labels = c("Emergencia", "Amenaza", "Alerta", "Advertencia", "Aviso**", "Vigilancia*"))) %>%
  drop_na() %>% 
  as_survey_design(ids = 1, weights = weightfactor) %>% 
  group_by(HISP, name) %>%
  summarize(mean = survey_mean(value, vartype = "ci")) %>% 
  rename("group" = "HISP")

all_means <- bind_rows(all_means, english_means, hisp_means)

#Create plots
p <- ggplot(all_means, aes(x = 1, y = mean, label = name, group = name)) +
  geom_point(size = 2) + 
  xlim(1, 1.2) +
  labs(x = "", y = "Urgency (mean)") +
  theme_classic(base_size = 12) +
  facet_wrap(~group, nrow = 2) +
  ggrepel::geom_text_repel(force = 0.1, nudge_x = 0.1, direction = "y", hjust = 0, segment.size = 0.1, size = 3) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 12, face = "bold"),
        legend.background = element_blank())
p

