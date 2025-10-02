library(tidyverse)
options(scipen=999)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(srvyr)

colors = c('#000000','#dc267f','#fe6100','#0072b2')
Diverge = c('#1B519C','#3680B6','#F5F5F5','#FDB863','#E66101')

#Data Analysis ================================================================================================================

#SPCdata<-read.csv("Read in WX23_SPC.csv here")

#Figure 4 ====================
Scale_con_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_word_con,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_con_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Concern", x = "", color = "Scale", title = "Concern by Scale Only") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm'))  
#ggsave(filename = "Concern_by_Scale_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")

#Figure 5 ====================
Scale_resp_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_word_resp,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_resp_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Likeihood of Response", x = "", color = "Scale", title = "Likelihood of Response by Scale Only") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm'))  
#ggsave(filename = "Resp_like_by_Scale_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")

#Figure 6 ====================
Scale_eff_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_word_eff,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_eff_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Perceived Response Efficacy", x = "", color = "Scale", title = "Perceived Response Efficacy by Scale Only") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm')) 
#ggsave(filename = "Efficacy_by_Scale_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")


#Mean values with maps=================================================================
#Figure 7 ====================
Scale_con_map_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_map_con,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_con_map_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Concern", x = "", color = "Scale", title = "Concern with Map and Words") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm'))  
#ggsave(filename = "Concern_by_Scale_and_map_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")

#Figure 8 ====================
Scale_resp_map_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_map_resp,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_resp_map_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Likelihood of Response", x = "", color = "Scale", title = "Likelihood of Response with Map and Words") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm'))  
#ggsave(filename = "Resp_like_by_Scale_and_map_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")

#Figure 9 ====================
Scale_eff_map_means <- SPCdata %>% 
  select(Version = spc_scale_rand,
         Level = spc_level_rand,
         var = spc_map_eff,
         WF = weightfactor) %>%
  pivot_longer(var) %>%
  as_survey_design(ids = 1, weights = WF) %>%
  group_by(Version, Level, name) %>% 
  summarise(mean = survey_mean(value, na.rm = TRUE), 
            sd = sqrt(survey_var(value, na.rm = TRUE)),
            n = n()) %>% 
  mutate(se = sd / sqrt(n))
ggplot(Scale_eff_map_means, aes(x = Level, y = mean, color = Version, Level = Version, shape = Version)) +
  geom_line(linewidth = 1.2, position = position_dodge(width = 0.35)) +
  geom_point(size = 3.5, position = position_dodge(width = 0.35)) +
  geom_errorbar(size = 1.2, aes(ymin = mean - (1.645 * se), ymax = mean + (1.645 * se)), width = 0.3, position = position_dodge(width = 0.35)) +
  scale_shape_manual(name = "Scale", values = c(15, 16, 17, 18), limits = c("current_scale","likert_scale","spanish_scale","levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  scale_x_discrete(limits = c("1","2","3","4","5"), labels = c('','','','','')) +
  scale_color_manual(limits = c("current_scale","likert_scale",'spanish_scale',"levels_scale"), 
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale'), values = colors) +
  labs(y = "Mean Perceived Response Efficacy", x = "", color = "Scale", title = "Perceived Response Efficacy with Map and Words") +
  annotate("text", x = 1:5, y = 13, label = c("MARGINAL", "SLIGHT", "ENHANCED", "MODERATE", "HIGH"), size = 5, color = colors[1]) +
  annotate("text", x = 1:5, y = 10, label = c("VERY LOW", "LOW", "MEDIUM", "HIGH", "VERY HIGH"), size = 5, color = colors[2]) +
  annotate("text", x = 1:5, y = 7, label = c("MINIMAL", "LOW", "MODERATE", "HIGH", "EXTREME"), size = 5, color = colors[3]) +
  annotate("text", x = 1:5, y = 4, label = c("LEVEL 1 OF 5", "LEVEL 2 OF 5", "LEVEL 3 OF 5", "LEVEL 4 OF 5", "LEVEL 5 OF 5"), size = 5, color = colors[4]) +
  coord_cartesian(ylim = c(20, 100), clip = "off") + theme_bw(base_size = 22) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + 
  theme(plot.margin = unit(c(2, 2, 15, 2), "mm"), legend.spacing.y = unit(0.3, 'cm')) 
#ggsave(filename = "Efficacy_by_Scale_and_map_plot_WF.png", height = 10, width = 13, path = "Directory of Choice")

#Post-questionnaire data analysis =======================================================

#Postdata<-read.csv("Post-questionnaire dataset")

#Figure 10 ====================
PrefMeans <- Postdata %>%
  select(prefer_scale_1,prefer_scale_2,prefer_scale_3,prefer_scale_4) %>%
  na.omit() %>%
  mutate(Current_Scale = ifelse(prefer_scale_1 == "1", 1,
                                ifelse(prefer_scale_2 == "1", 2,
                                       ifelse(prefer_scale_3 == "1", 3,
                                              ifelse(prefer_scale_4 == "1", 4, NA
                                              )))),
         Likert_Scale = ifelse(prefer_scale_1 == "2", 1,
                               ifelse(prefer_scale_2 == "2", 2,
                                      ifelse(prefer_scale_3 == "2", 3,
                                             ifelse(prefer_scale_4 == "2", 4, NA
                                             )))),
         Spanish_Scale = ifelse(prefer_scale_1 == "3", 1,
                                ifelse(prefer_scale_2 == "3", 2,
                                       ifelse(prefer_scale_3 == "3", 3,
                                              ifelse(prefer_scale_4 == "3", 4, NA
                                              )))),
         Numbered_Levels = ifelse(prefer_scale_1 == "4", 1,
                                  ifelse(prefer_scale_2 == "4", 2,
                                         ifelse(prefer_scale_3 == "4", 3,
                                                ifelse(prefer_scale_4 == "4", 4, NA
                                                ))))
  ) %>%
  select(Current_Scale:Numbered_Levels) %>%
  pivot_longer(Current_Scale:Numbered_Levels) %>%
  group_by(name) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n))

PrefMeansGroup <- Postdata %>%
  select(prefer_scale_1,prefer_scale_2,prefer_scale_3,prefer_scale_4,Career) %>%
  na.omit() %>%
  mutate(Current_Scale = ifelse(prefer_scale_1 == "1", 1,
                                ifelse(prefer_scale_2 == "1", 2,
                                       ifelse(prefer_scale_3 == "1", 3,
                                              ifelse(prefer_scale_4 == "1", 4, NA
                                              )))),
         Likert_Scale = ifelse(prefer_scale_1 == "2", 1,
                               ifelse(prefer_scale_2 == "2", 2,
                                      ifelse(prefer_scale_3 == "2", 3,
                                             ifelse(prefer_scale_4 == "2", 4, NA
                                             )))),
         Spanish_Scale = ifelse(prefer_scale_1 == "3", 1,
                                ifelse(prefer_scale_2 == "3", 2,
                                       ifelse(prefer_scale_3 == "3", 3,
                                              ifelse(prefer_scale_4 == "3", 4, NA
                                              )))),
         Numbered_Levels = ifelse(prefer_scale_1 == "4", 1,
                                  ifelse(prefer_scale_2 == "4", 2,
                                         ifelse(prefer_scale_3 == "4", 3,
                                                ifelse(prefer_scale_4 == "4", 4, NA
                                                ))))
  ) %>%
  select(Current_Scale:Numbered_Levels,Career) %>%
  pivot_longer(Current_Scale:Numbered_Levels) %>%
  group_by(name,Career) %>%
  summarize(mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            n = n()) %>%
  mutate(se = sd / sqrt(n))

a = ggplot(PrefMeans, aes(x = 1, y = mean, color = name, group = name, level = name, shape = name)) +
  geom_point(size = 4, position=position_dodge(width = 0.15)) +
  geom_errorbar(size = 1.5, aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), position=position_dodge(width = 0.15), width = 0.2) +
  xlim(0.75,1.25) +
  scale_y_reverse(limits = c(4,1)) +
  scale_color_manual(name = "Scale\nName", values = colors, limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"), labels = c("Current\nScale","Likert\nScale",'"Spanish"\nScale',"Numerical\nScale")) +
  scale_shape_manual(name = "Scale\nName", values = c(15, 16, 17, 18), limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"),
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  theme_bw(base_size = 22) +
  labs(x=' ',y='Mean ranking',color=' ')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.spacing.y = unit(0.3,'cm'), plot.title = element_text(size=20, face="bold")) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + ggtitle("a.) All Participants (n = 39)")
b = ggplot(subset(PrefMeansGroup, Career == 'BCST'), aes(x = 1, y = mean, color = name, group = name, level = name, shape = name)) +
  geom_point(size = 4, position=position_dodge(width = 0.15)) +
  geom_errorbar(size = 1.5, aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), position=position_dodge(width = 0.15), width = 0.2) +
  xlim(0.75,1.25) +
  scale_y_reverse(limits = c(4,1)) +
  scale_color_manual(name = "Scale\nName", values = colors, limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"), labels = c("Current\nScale","Likert\nScale",'"Spanish"\nScale',"Numerical\nScale")) +
  scale_shape_manual(name = "Scale\nName", values = c(15, 16, 17, 18), limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"),
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  theme_bw(base_size = 22) +
  labs(x=' ',y='',color=' ')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.spacing.y = unit(0.3,'cm'), plot.title = element_text(size=20, face="bold")) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + ggtitle("b.) All Broadcasters (n = 19)")
c = ggplot(subset(PrefMeansGroup, Career == 'EM'), aes(x = 1, y = mean, color = name, group = name, level = name, shape = name)) +
  geom_point(size = 4, position=position_dodge(width = 0.15)) +
  geom_errorbar(size = 1.5, aes(ymin = mean - (1.96 * se), ymax = mean + (1.96 * se)), position=position_dodge(width = 0.15), width = 0.2) +
  xlim(0.75,1.25) +
  scale_y_reverse(limits = c(4,1)) +
  scale_color_manual(name = "Scale\nName", values = colors, limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"), labels = c("Current\nScale","Likert\nScale",'"Spanish"\nScale',"Numerical\nScale")) +
  scale_shape_manual(name = "Scale\nName", values = c(15, 16, 17, 18), limits = c("Current_Scale","Likert_Scale","Spanish_Scale","Numbered_Levels"),
                     labels=c('Current\nScale', 'Likert\nScale', '"Spanish"\nScale', 'Numerical\nScale')) +
  theme_bw(base_size = 22) +
  labs(x=' ',y='',color=' ')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  theme(legend.spacing.y = unit(0.3,'cm'), plot.title = element_text(size=20, face="bold")) + guides(color = guide_legend(byrow = TRUE), shape = guide_legend(byrow = TRUE)) + ggtitle("c.) All Emergency Managers (n = 20)")

grid.arrange(a + theme(legend.position="none"), b + theme(legend.position="none"), c, ncol = 7, layout_matrix = cbind(c(1),c(1),c(2),c(2),c(3),c(3),c(3)))
g <- arrangeGrob(a + theme(legend.position="none"), b + theme(legend.position="none"), c, ncol = 7, layout_matrix = cbind(c(1),c(1),c(2),c(2),c(3),c(3),c(3)))
#ggsave(filename = "Breakdown_Pref.png", height = 8, width = 16, path = "Directory of Choice", g)

#Figure 11 ====================
Postdata %>%
  select(impor_change) %>%
  na.omit() %>%
  gather(x, value, impor_change) %>%
  group_by(x) %>%
  count(value) %>%
  mutate(prop = n/sum(n) * 100) %>%
  data.frame() %>%
  add_row(x = "impor_change", value = "Not at all important", n = 0, prop = 0) %>%
  add_row(x = "impor_change", value = "Not very important", n = 0, prop = 0) %>%
  ggplot(aes(value, y = prop, fill = factor(value))) +
  geom_bar(stat = "identity", position=position_dodge(), color = "black") +
  #scale_fill_manual(name = "Scale", values = c('#d55e00','#cc79a7','#0072b2','#009e73'),labels = c("Current\nScale","Likert\nScale",'"Spanish"\nScale',"Numbered\nLevels")) +
  scale_x_discrete(limits= c("Not at all important","Not very important","Somewhat important","Very important","Extremely important"),
                   labels= c("Not at all\nimportant","Not very\nimportant","Somewhat\nimportant","Very\nimportant","Extremely\nimportant")) + 
  scale_y_continuous(limits = c(0,50)) +
  scale_fill_manual(values = Diverge, limits = factor(c("Not at all important","Not very important","Somewhat important","Very important","Extremely important"))) +
  labs(title = "How important do you think it is\nthat SPC change the outlook scale?",x = "", y = "Proportion of Responses (%)") +
  theme_bw(base_size = 22) + theme(legend.position = "none")
#ggsave(filename = "Importance.png", height = 10, width = 10, path = "Directory of Choice", b)

