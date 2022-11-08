library(tidyverse)
library(ggridges)
library(Hmisc)

#Read Data
data <- read_csv("https://www.dropbox.com/s/b919itcm7y03x22/ConOutInterpretation.csv?dl=1")

# Categorize which treatment the participant received (category only, level only, category + level, etc)-----------------------------------
data <- data %>% 
  mutate(treatment = str_remove(spc_rand, "A "),
         treatment = str_remove(treatment, "AN "), 
         treatment = factor(treatment, levels = rev(c("SLIGHT RISK", "LEVEL 2 of 5 RISK", "5% CHANCE", "SLIGHT RISK (LEVEL 2 of 5)", "SLIGHT RISK (5% CHANCE)",
                                                     "ENHANCED RISK", "LEVEL 3 of 5 RISK", "15% CHANCE", "ENHANCED RISK (LEVEL 3 of 5)", "ENHANCED RISK (15% CHANCE)",
                                                     "MODERATE RISK", "LEVEL 4 of 5 RISK", "30% CHANCE", "MODERATE RISK (LEVEL 4 of 5)", "MODERATE RISK (30% CHANCE)"))),  
         category = case_when(treatment %in% c("SLIGHT RISK", "ENHANCED RISK", "MODERATE RISK") ~ "Category Only",
                              treatment %in% c("LEVEL 2 of 5 RISK", "LEVEL 3 of 5 RISK", "LEVEL 4 of 5 RISK") ~ "Level Only",
                              treatment %in% c("5% CHANCE", "15% CHANCE", "30% CHANCE") ~ "Percent Only",
                              treatment %in% c("SLIGHT RISK (LEVEL 2 of 5)", "ENHANCED RISK (LEVEL 3 of 5)", "MODERATE RISK (LEVEL 4 of 5)") ~ "Category + Level",
                              treatment %in% c("SLIGHT RISK (5% CHANCE)", "ENHANCED RISK (15% CHANCE)", "MODERATE RISK (30% CHANCE)") ~ "Category + Percent"), 
         category = factor(category, levels = c("Category Only", "Level Only", "Percent Only", "Category + Level", "Category + Percent")),
         spc_risk_perc = as.numeric(spc_risk_perc),
         spc_resp_perc = as.numeric(spc_resp_perc))

#FIG 2: Plot levels of concern by category-------------------------------------------------------------------------

icr_concern<-data %>% 
  group_by(category, treatment) %>% 
  mutate(n=n(),
         lb = quantile(spc_risk_perc, p = 0.25, na.rm = TRUE), 
         median = quantile(spc_risk_perc, p = 0.5, na.rm = TRUE), 
         ub = quantile(spc_risk_perc, p = 0.75, na.rm = TRUE))%>%
  ungroup()%>% 
  group_by(category, treatment)%>%
  mutate(race_med=quantile(spc_risk_perc, p = 0.5, na.rm = TRUE))%>%
  ggplot(., aes(x = median, y = treatment, color = category)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lb, xmax = ub), size = 1.5) +
  facet_wrap(~category, scales = "free_y", ncol = 1) +
  guides(color = "none") +
  labs(x = "Concern Scale", y = "", title = " ") +
  scale_color_manual( values = c('#f74aff','#377EB8', '#6AA84F','#f7c428', '#E41A1C'))+
  geom_label(aes(label = paste("n =",n), x = 104, y = treatment),color='gray46',size=4) +
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),
        panel.grid.major = element_line(color='gray'),
        panel.grid.minor = element_line(color='#FFFFFF'),
        plot.title = element_text(size=15,color='#4d4d4d'),
        legend.text=element_text(size=10,color='#4d4d4d'),
        legend.title=element_text(size=10,color='#4d4d4d'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(size=15))+
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), 
                     labels = c("0\nNot at all\nconcerned", "25", "50", "75", "100\nExtremely\nconcerned"), 
                     limits = c(0, 105)) +
  theme(axis.title = element_blank())
icr_concern

#FIG 3: Plot response likelihood by category-------------------------------------------------------------------------

icr_resp<-data %>% 
  group_by(category, treatment) %>% 
  mutate(lb = quantile(spc_resp_perc, p = 0.25, na.rm = TRUE), 
         median = quantile(spc_resp_perc, p = 0.5, na.rm = TRUE), 
         ub = quantile(spc_resp_perc, p = 0.75, na.rm = TRUE))%>%
  ungroup()%>% 
  group_by(category, treatment)%>%
  mutate(race_med=quantile(spc_resp_perc, p = 0.5, na.rm = TRUE))%>%
  ggplot(., aes(x = median, y = treatment, color = category)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lb, xmax = ub), size = 1.5) +
  facet_wrap(~category, scales = "free_y", ncol = 1) +
  guides(color = "none") +
  labs(x = "Likelihood Scale", y = "", title = " ") +
  scale_color_manual( values = c('#f74aff','#377EB8', '#6AA84F','#f7c428', '#E41A1C'))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),
        panel.grid.major = element_line(color='gray'),
        panel.grid.minor = element_line(color='#FFFFFF'),
        plot.title = element_text(size=15,color='#4d4d4d'),
        legend.text=element_text(size=10,color='#4d4d4d'),
        legend.title=element_text(size=10,color='#4d4d4d'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(size=15))+
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), 
                     labels = c("0\nNot at all\nlikely", "25", "50", "75", "100\nExtremely\nlikely"), 
                     limits = c(0, 105)) +
  theme(axis.title = element_blank())
icr_resp

#Fig. 4: Median Concern by demographic--------------------------------------

#all
data%>%
  group_by(category,treatment)%>%
  summarise(med=median(spc_risk_perc,na.rm = T))

#gender
data%>%
  group_by(category, treatment,gend)%>%
  summarise(med=median(spc_risk_perc,na.rm = T))%>%
  filter(gend==1)%>% #gend==1 for male, gend==0 for female
  print(n=40)

#age
data%>%
  mutate(age_rr=car::recode(data$age,"18:34=1;35:54=2;55:120=3"))%>%
  group_by(category, treatment,age_rr)%>%
  filter(age_rr==3)%>% #age 18-34: age_rr==1, 35-54: age_rr==2, 55+: age_rr==3
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#ethnicity
data%>%
  group_by(category, treatment,hisp)%>%
  filter(hisp==1)%>% #hisp==1 for hispanic, hisp==0 non-hispanic 
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#race
data%>%
  mutate(race_rr=car::recode(data$race,"1=1;2:7=2"))%>%
  group_by(category, treatment,race_rr)%>%
  filter(race_rr==1)%>% #Non-white: race_rr==2, white: race_rr==1
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#numeracy
##numeracy assessment:
cointoss_correct<-car::recode(as.numeric(as.character(data$cointoss)),"500=1;NA=NA;else=0")
bigbucks_correct<-car::recode(as.numeric(as.character(data$bigbucks)),"10=1;NA=NA;else=0")
acme_pub_correct<-car::recode(as.numeric(as.character(data$acme_pub)),"0.1=1;NA=NA;else=0")
choir_correct<-car::recode(as.numeric(as.character(data$choir)),"25=1;NA=NA;else=0")
fiveside_correct<-car::recode(as.numeric(as.character(data$fiveside)),"30=1;NA=NA;else=0")
sixside_correct<-car::recode(as.numeric(as.character(data$sixside)),"20=1;NA=NA;else=0")
mushroom_correct<-car::recode(as.numeric(as.character(data$mushroom)),"50=1;NA=NA;else=0")
part1_score<-cointoss_correct+bigbucks_correct+acme_pub_correct
part2_score<-NA
advance<-ifelse(part1_score<2,0,1)
part2_score<-ifelse(advance==0,0,part2_score)
part2_score<-ifelse(advance==1 & choir_correct==0 & fiveside_correct==0,1,part2_score)
part2_score<-ifelse(advance==1 & choir_correct==0 & fiveside_correct==1,2,part2_score)
part2_score<-ifelse(advance==1 & choir_correct==1 & sixside_correct==0 & mushroom_correct==0,3,part2_score)
part2_score<-ifelse(advance==1 & choir_correct==1 & sixside_correct==0 & mushroom_correct==1,4,part2_score)
part2_score<-ifelse(advance==1 & choir_correct==1 & sixside_correct==1,4,part2_score)
data$numeracy<-part1_score+part2_score
data$numeracy

data%>%
  mutate(Numeracy=car::recode(numeracy,"0:3='Low';4:10='High'"))%>%
  group_by(category, treatment,Numeracy)%>%
  filter(is.na(Numeracy)!=T)%>%
  filter(Numeracy=='High')%>% #Change numeracy here
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#region
data$nws_region
data%>%
  group_by(category, treatment,nws_region)%>%
  filter(nws_region=='Western Region')%>% #Change region here
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#Fig. 5: Median Response likelihood by demographic--------------------------------------
#all
data%>%
  group_by(category,treatment)%>%
  summarise(med=median(spc_resp_perc,na.rm = T))

#gender
data%>%
  group_by(category, treatment,gend)%>%
  summarise(med=median(spc_resp_perc,na.rm = T))%>%
  print(n=40)

#age
data%>%
  mutate(age_rr=car::recode(data$age,"18:34=1;35:54=2;55:120=3"))%>%
  group_by(category, treatment,age_rr)%>%
  filter(age_rr==1)%>% #age 18-34: age_rr==1, 35-54: age_rr==2, 55+: age_rr==3
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#ethnicity
data%>%
  group_by(category, treatment,hisp)%>%
  filter(hisp==1)%>% #hisp==1 for hispanic, hisp==0 non-hispanic 
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#race
data%>%
  mutate(race_rr=car::recode(data$race,"1=1;2:7=2"))%>%
  group_by(category, treatment,race_rr)%>%
  filter(race_rr==1)%>% #Non-white: race_rr==2, white: race_rr==1
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#numeracy
data%>%
  mutate(Numeracy=car::recode(numeracy,"0:3='Low';4:10='High'"))%>%
  group_by(category, treatment,Numeracy)%>%
  filter(is.na(Numeracy)!=T)%>%
  filter(Numeracy=='Low')%>% #Change numeracy here
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)

#region
region_risk<-data%>%
  group_by(category, treatment,nws_region)%>%
  filter(nws_region=='Southern Region')%>% #Change region here
  summarise(med=median(spc_risk_perc,na.rm = T),n=n())%>%
  print(n=50)
