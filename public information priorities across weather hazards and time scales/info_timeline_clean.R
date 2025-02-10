library(tidyverse)
library(zipcodeR)
library(maptools)
library(sp)
library(rgdal)

#load survey data---------------
#change the file paths to your computer!
wxdata<-read_csv('your filepath for WX21 data here')
tcdata<-read_csv('your filepath for TC21 data here')
wwdata<-read_csv('your filepath for WW22 data here')

#Figure 1: overall ratings----------------------
wx<-wxdata%>%
  select(forcast_loc:forcast_safe) %>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Severe")
tc<-tcdata%>%
  select(forcast_loc:forcast_safe) %>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Tropical")
ww<-wwdata%>%
  select(forcast_loc:forcast_safe) %>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Winter")
comb_data<-rbind(wx,tc,ww)
fig1 <- comb_data %>% 
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
                                  "Protective actions"))) %>% 
  group_by(name,haz) %>%
  summarise(n = n(), x = mean(value,na.rm = T), s = sd(value,na.rm = T)) %>% 
  mutate(lb_x =  x - (1.96 * (s / sqrt(n - 1))), 
         ub_x =  x + (1.96 * (s / sqrt(n - 1))),
         lb_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = FALSE))), 
         ub_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = TRUE))))%>%
  arrange(x) %>% 
  ggplot(., aes(x = 1, y = x, color=name)) +
  geom_point(size=5,position = position_dodge(width = 0.15)) +
  geom_errorbar(aes(ymin = lb_x, ymax = ub_x),width = 0.8, linewidth=0.8,position = position_dodge(width = 0.15)) +
  facet_wrap(~haz)+
  ylim(5.1,2)+
  xlim(0,2)+
  theme_minimal(base_size = 22)+
  labs(x=' ',y='Mean ranking',color=' ')+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
fig1

#Figure 2: severe timeline---------------
fig2 <- wxdata %>% 
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
fig2

#difference of proportion tests for increase in impacts/actions-----------------
severe_ttest<-wxdata %>% 
  select(three_days:fifteen_min) %>% 
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n), 
         se = sqrt(p * (1 - p) / n),
         lower = p - 1.96 * se,
         upper = p + 1.96 * se) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes"))) %>% 
  mutate(value = factor(value, labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                          "Protective\nActions")))

hr4_paction <- severe_ttest %>% filter(name == '4 hours', value == 'Protective\nActions')
min60_paction <- severe_ttest %>% filter(name == '60 minutes', value == 'Protective\nActions')

point_estimate <- min60_paction$p - hr4_paction$p
point_estimate

standard_error <- sqrt((min60_paction$se ^ 2) + (hr4_paction$se ^ 2))
standard_error

confidence_interval <- c(point_estimate - 1.96 * standard_error, point_estimate + 1.96 * standard_error)
confidence_interval

z_stat <- (point_estimate - 0) / standard_error
z_stat

p_value <- 1 - pnorm(z_stat)
p_value

hr4_impacts <- severe_ttest %>% filter(name == '4 hours', value == 'Impacts')
min60_impacts <- severe_ttest %>% filter(name == '60 minutes', value == 'Impacts')

point_estimate <- min60_impacts$p - hr4_impacts$p
point_estimate

standard_error <- sqrt((min60_impacts$se ^ 2) + (hr4_impacts$se ^ 2))
standard_error

confidence_interval <- c(point_estimate - 1.96 * standard_error, 
                         point_estimate + 1.96 * standard_error)
confidence_interval

z_stat <- (point_estimate - 0) / standard_error
z_stat

p_value <- 1 - pnorm(z_stat)
p_value

#shannon entropy calculations----------------
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

#severe
ent_sev<-wxdata %>% 
  select(three_days:fifteen_min) %>% 
  pivot_longer(three_days:fifteen_min)

ent_three<-ent_sev%>%filter(name=='three_days')
ent_15<-ent_sev%>%filter(name=='fifteen_min')

entropy(ent_three$value)
entropy(ent_15$value)

#tropical
ent_tc<-tcdata %>% 
  select(five_days:one_day) %>% 
  pivot_longer(five_days:one_day)

ent_trop_far<-ent_tc%>%filter(name=='five_days')
ent_trop_close<-ent_tc%>%filter(name=='one_day')

entropy(ent_trop_far$value)
entropy(ent_trop_close$value)

#winter
ent_ww<-wwdata %>% 
  select(five_days:zero_day) %>% 
  pivot_longer(five_days:zero_day)

ent_ww_far<-ent_ww%>%filter(name=='five_days')
ent_ww_close<-ent_ww%>%filter(name=='zero_day')

entropy(ent_ww_far$value)
entropy(ent_ww_close$value)

#Figure 3: tropical timeline-------------------
fig3 <- tcdata %>% 
  select(five_days:one_day) %>% 
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n),
         se = sqrt(p * (1 - p) / n),
         lower = (p - 1.96 * se)*100,
         upper = (p + 1.96 * se)*100) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day"))) %>% 
  mutate(value = factor(value, labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                          "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = value, group = value)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  theme_minimal(base_size = 22) +
  ylim(0,40)+
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "") +
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
fig3

#Figure 4: winter timeline------------------
fig4 <- wwdata %>% 
  select(five_days:zero_day) %>% 
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = (n / sum(n)),
         se = sqrt(p * (1 - p) / n),
         lower = (p - 1.96 * se)*100,
         upper = (p + 1.96 * se)*100) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of'))) %>% 
  mutate(value = factor(value, labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                          "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = value, group = value)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  theme_minimal(base_size = 22) +
  ylim(0,40)+
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "") +
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
fig4

#Figure 5: protective actions by avg number of warnings--------------------
#severe by average number of warnings
severe_by_warn <- wxdata %>% 
  select(three_days:fifteen_min,WX) %>% 
  drop_na()%>%
  mutate(wx_warns=case_when(WX>=quantile(WX,probs=0.9) ~ 'High',
                            WX<=quantile(WX,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n), 
         se = sqrt(p * (1 - p) / n),
         lower = p - 1.96 * se,
         upper = p + 1.96 * se)%>%
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  filter(value == 'Protective\nActions')%>%
  mutate(haz='Severe')

#winter timeline stratified by average number of warnings
winter_by_warn <- wwdata %>% 
  select(five_days:zero_day,WW) %>% 
  drop_na()%>%
  mutate(wx_warns=case_when(WW>=quantile(WW,probs=0.9) ~ 'High',
                            WW<=quantile(WW,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n), 
         se = sqrt(p * (1 - p) / n),
         lower = p - 1.96 * se,
         upper = p + 1.96 * se)%>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  filter(value == 'Protective\nActions')%>%
  mutate(haz='Winter')

#tropical timeline stratified by average number of warnings
tropical_by_warn <- tcdata %>% 
  select(five_days:one_day,TC) %>% 
  drop_na()%>%
  drop_na(TC)%>%
  mutate(wx_warns=case_when(TC>1 ~ 'High',
                            TC==0 ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n), 
         se = sqrt(p * (1 - p) / n),
         lower = p - 1.96 * se,
         upper = p + 1.96 * se)%>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  filter(value == 'Protective\nActions')%>%
  mutate(haz='Tropical')

#total plot of protective actions by warning counts
fig5<-rbind(tropical_by_warn,winter_by_warn,severe_by_warn)%>%
  mutate(wx_warns=factor(wx_warns, levels = c("Low",'High')))%>%
  ggplot(., aes(x = name, y = p*100, color = wx_warns,group=wx_warns)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~haz,scales = 'free_x')+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Event", y = "% who chose protective actions", color = "Avg. Annual\nWarnings") +
  scale_color_manual(values=c("#BBDFFA","#1727AE"))+
  theme(axis.text.x = element_text(size = 14,angle = 45),
        axis.title.y = element_text(size=22),
        panel.background = element_rect(fill = 'white',color=NA))
fig5

#supplemental material: overall ratings by avg number of warnings------------------------
#severe
wx_warns_data<-wxdata%>%
  select(forcast_loc:forcast_safe,WX) %>% 
  drop_na(WX)%>%
  mutate(wx_warns=case_when(WX>=quantile(WX,probs=0.9) ~ 'High',
                            WX<=quantile(WX,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Severe")%>%
  select(wx_warns, name, value, haz)

#tropical
tc_warns_data<-tcdata%>%
  select(forcast_loc:forcast_safe,TC) %>% 
  drop_na(TC)%>%
  mutate(wx_warns=case_when(TC>1 ~ 'High',
                            TC==0 ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Tropical")%>%
  select(wx_warns, name, value, haz)

#winter
ww_warns_data<-wwdata%>%
  select(forcast_loc:forcast_safe,WW) %>% 
  drop_na(WW)%>%
  mutate(wx_warns=case_when(WW>=quantile(WW,probs=0.9) ~ 'High',
                            WW<=quantile(WW,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>% 
  pivot_longer(forcast_loc:forcast_safe)%>%
  mutate(haz="Winter")%>%
  select(wx_warns, name, value, haz)

#full plot, figure 1
comb_warns_data<-rbind(wx_warns_data,tc_warns_data,ww_warns_data)

sm_fig1 <- comb_warns_data %>% 
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
                                  "Protective actions")),
         wx_warns=factor(wx_warns, levels=c('Low','High'))) %>% 
  group_by(name,haz,wx_warns) %>%
  summarise(n = n(), x = mean(value,na.rm = T), s = sd(value,na.rm = T)) %>% 
  mutate(lb_x =  x - (1.96 * (s / sqrt(n - 1))), 
         ub_x =  x + (1.96 * (s / sqrt(n - 1))),
         lb_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = FALSE))), 
         ub_s = s * (sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = TRUE))))%>%
  arrange(x) %>% 
  ggplot(., aes(x = wx_warns, y = x, color=name,group=name)) +
  geom_point(size=5, position = position_dodge(width = 0.15)) +
  geom_errorbar(aes(ymin = lb_x, ymax = ub_x),width = 0.7, size=0.8,alpha=0.6, position = position_dodge(width = 0.15)) +
  geom_line(size=1, position = position_dodge(width = 0.15))+
  facet_wrap(~haz)+
  ylim(5.2,1.5)+
  theme_minimal(base_size = 22)+
  labs(x='Avg. Annual Warnings',y='Mean ranking',color=' ')+
  scale_color_manual(values=c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00","#fccf03"))
sm_fig1

#supplemental material: severe rankings by avg warnings-------------------
sm_fig2 <- wxdata %>% 
  select(three_days:fifteen_min,WX) %>% 
  drop_na(WX)%>%
  mutate(wx_warns=case_when(WX>=quantile(WX,probs=0.9) ~ 'High',
                            WX<=quantile(WX,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = wx_warns,group=wx_warns)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "Avg. Annual \nWarnings") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig2

#supplemental material: tropical rankings by avg warnings-------------------
sm_fig3 <- tcdata %>% 
  select(five_days:one_day,TC) %>% 
  drop_na(TC)%>%
  mutate(wx_warns=case_when(TC>1 ~ 'High',
                            TC==0 ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = wx_warns,group=wx_warns)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "Avg. Annual\nWarnings") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig3

#supplemental material: winter rankings by avg warnings-------------------
sm_fig4 <- wwdata %>% 
  select(five_days:zero_day,WW) %>% 
  drop_na(WW)%>%
  mutate(wx_warns=case_when(WW>=quantile(WW,probs=0.9) ~ 'High',
                            WW<=quantile(WW,probs=0.1) ~ 'Low',
                            TRUE ~ 'Other'))%>%
  filter(wx_warns %in% c('High','Low'))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(wx_warns,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = wx_warns,group=wx_warns)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "Avg. Annual\nWarnings") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig4

#supplemental material: severe rankings by education level-------------------
sm_fig5 <- wxdata %>% 
  select(three_days:fifteen_min,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(edu_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = edu_r,group=edu_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "Edu. Level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig5

#supplemental material: tropical rankings by education level-------------------
sm_fig6 <- tcdata %>% 
  select(five_days:one_day,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(edu_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = edu_r,group=edu_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "Edu. level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig6

#supplemental material: winter rankings by education level-------------------
sm_fig7 <- wwdata %>% 
  select(five_days:zero_day,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(edu_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = edu_r,group=edu_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "Edu. level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig7

#supplemental material: severe rankings by income-------------------
sm_fig8 <- wxdata %>% 
  select(three_days:fifteen_min,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"),
         inc_r=factor(inc_r,levels = c('More than 50k','Less than 50k')))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(inc_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = inc_r,group=inc_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "Income Level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig8

#supplemental material: tropical rankings by income-------------------
sm_fig9 <- tcdata %>% 
  select(five_days:one_day,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"),
         inc_r=factor(inc_r,levels = c('More than 50k','Less than 50k')))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(inc_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = inc_r,group=inc_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "Income level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig9

#supplemental material: winter rankings by income-------------------
sm_fig10 <- wwdata %>% 
  select(five_days:zero_day,edu,income) %>% 
  mutate(edu_r=case_when(edu<=5 ~ 'Less than 4-yr degree',
                         edu>5 ~ '4-yr degree or more'),
         inc_r=case_when(income==1 ~ "Less than 50k",
                         income>1 ~ "More than 50k"),
         inc_r=factor(inc_r,levels = c('More than 50k','Less than 50k')))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(inc_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = inc_r,group=inc_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "Income level") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig10

#supplemental material: severe rankings by race/ethnicity-------------------
sm_fig11 <- wxdata %>% 
  select(three_days:fifteen_min,race,hisp) %>% 
  mutate(race_r=case_when(race==1 & hisp==0 ~ 'White, Not Hispanic',
                          race>=2 | hisp==1 ~ 'Non-White/Hispanic'),
         race_r=factor(race_r,levels = c('Non-White/Hispanic','White, Not Hispanic')))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(race_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = race_r,group=race_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "Race & Ethnicity") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig11

#supplemental material: tropical rankings by race/ethnicity-------------------
sm_fig12 <- tcdata %>% 
  select(five_days:one_day,race,hisp) %>% 
  mutate(race_r=case_when(race==1 & hisp==0 ~ 'White, Not Hispanic',
                          race>=2 | hisp==1 ~ 'Non-White/Hispanic'),
         race_r=factor(race_r,levels = c('Non-White/Hispanic','White, Not Hispanic')))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(race_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = race_r,group=race_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "Race & Ethnicity") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig12

#supplemental material: winter rankings by race/ethnicity-------------------
sm_fig13 <- wwdata %>% 
  select(five_days:zero_day,race,hisp) %>% 
  mutate(race_r=case_when(race==1 & hisp==0 ~ 'White, Not Hispanic',
                          race>=2 | hisp==1 ~ 'Non-White/Hispanic'),
         race_r=factor(race_r,levels = c('Non-White/Hispanic','White, Not Hispanic')))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(race_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = race_r,group=race_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "Race & Ethnicity") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig13

#supplemental material: severe rankings by gender-------------------
sm_fig14 <- wxdata %>% 
  select(three_days:fifteen_min,gend) %>% 
  mutate(gend_r=case_when(gend==0 ~ 'Female',
                         gend==1 ~ 'Male'))%>%
  pivot_longer(three_days:fifteen_min) %>% 
  drop_na() %>% 
  group_by(gend_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("three_days", "one_day", "four_hours", "sixty_min", "fifteen_min"), 
                       labels = c("3 days", "1 day", "4 hours", "60 minutes", "15 minutes")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = gend_r,group=gend_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Severe Weather Event", y = "Respondents (percent)", color = "Gender") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig14

#supplemental material: tropical rankings by gender-------------------
sm_fig15 <- tcdata %>% 
  select(five_days:one_day,gend) %>% 
  mutate(gend_r=case_when(gend==0 ~ 'Female',
                          gend==1 ~ 'Male'))%>%
  pivot_longer(five_days:one_day) %>% 
  drop_na() %>% 
  group_by(gend_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day"), 
                       labels = c("5 days", "3 days", "2 days", "1 day")))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>% 
  ggplot(., aes(x = name, y = p*100, color = gend_r,group=gend_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Tropical Weather Event", y = "Respondents (percent)", color = "Gender") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig15

#supplemental material: winter rankings by gender-------------------
sm_fig16 <- wwdata %>% 
  select(five_days:zero_day,gend) %>% 
  mutate(gend_r=case_when(gend==0 ~ 'Female',
                          gend==1 ~ 'Male'))%>%
  pivot_longer(five_days:zero_day) %>% 
  drop_na() %>% 
  group_by(gend_r,name, value) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(name = factor(name, levels = c("five_days","three_days", "two_days","one_day",'zero_day'), 
                       labels = c("5 days", "3 days", "2 days", "1 day",'Day of')))%>% 
  mutate(value = factor(value, levels=c(1,2,3,4,5,6), labels = c("Location", "Timing", "Chance", "Severity", "Impacts",
                                                                 "Protective\nActions")))%>%
  ggplot(., aes(x = name, y = p*100, color = gend_r,group=gend_r)) +
  geom_line(size = 1.25) +
  geom_point(size = 3) +
  facet_wrap(~value)+
  theme_minimal(base_size = 22) +
  labs(x = "Time Before Forecast Winter Weather Event", y = "Respondents (percent)", color = "Gender") +
  scale_color_manual(values=c("#1727AE","#BBDFFA"))+
  theme(axis.text.x = element_text(size = 14,angle = 45))
sm_fig16

