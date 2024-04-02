library(tidyverse)

em_data<-read_csv('https://www.dropbox.com/scl/fi/ee1ivhx6dsozipjpg33rh/wxem_wave2_info.csv?rlkey=yuelolstftjzlzctoo2ltvht6&dl=1') #Read in data

## Figure 2: Ranking of Information Attributes ---------------------------
em<-em_data%>%
  select(forcast_loc:forcast_safe) %>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Severe") #Organize and simplify ranking data
em_overall_ratings <- em %>% 
  mutate(name = factor(name, levels = c("forcast_loc", 
                                        "forcast_sev", 
                                        "forcast_time",
                                        "forcast_prob",
                                        "forcast_impact",
                                        "forcast_safe"),
                       labels = c("Location",
                                  "Severity", 
                                  "Timing",
                                  "Chance",
                                  "Impacts",
                                  "Protective\nactions"))) %>% 
  group_by(name,haz) %>%
  summarise(n = n(), x = mean(value,na.rm = T), s = sd(value,na.rm = T)) %>% 
  mutate(lb_x =  x - (1.96 * (s / sqrt(n - 1))), 
         ub_x =  x + (1.96 * (s / sqrt(n - 1))),
         lb_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = FALSE))), 
         ub_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = TRUE))))%>%
  arrange(x) %>% 
  ggplot(., aes(x = 1, y = x, color=name)) +
  geom_point(size=5,position = position_dodge(width = 0.15)) +
  geom_errorbar(aes(ymin = lb_x, ymax = ub_x),width = 0.8, size=0.8,position = position_dodge(width = 0.15)) +
  ylim(5.8,1)+
  xlim(0,2)+
  theme_minimal(base_size = 22)+
  labs(x=' ',y='Mean ranking',color=' ')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
em_overall_ratings #Print figure

#Save figure
ggsave("filepath", em_overall_ratings, 
       height = 8, width = 11)


#Figure 3: Attribute timeline -----------------------------------------------------------------------------------------------
em_severe_timeline <- em_data %>% 
  select(three_days:fifteen_min) %>% 
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(name, value) %>% 
  summarise(n = n())%>%
  mutate(p = n / sum(n),
         se = sqrt(p * (1 - p) / n),
         lower = (p - 1.96 * se)*100,
         upper = (p + 1.96 * se)*100) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes"))) %>% 
  mutate(value = factor(value, labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                          "Protective\nActions"))) %>%
  ggplot(., aes(x = name, y = p*100, color = value, group = value)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  theme_minimal(base_size = 22) +
  ylim(-3,43)+
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "") +
  scale_shape_manual(values=c(15,16,17,0,1,2))+
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))+
  scale_fill_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
em_severe_timeline #Print figure

#Save figure
ggsave("filepath", em_severe_timeline, 
       height = 7, width = 12, bg='white')
