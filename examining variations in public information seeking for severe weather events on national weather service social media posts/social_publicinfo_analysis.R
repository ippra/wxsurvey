library(tidyverse)
library(sf)
library(ggthemes)

#Load NWS post/comment data
data <- read.csv("File Path Here")

#Add warning counts to NWS post/comment file based on CWA-------------------------
#Load warning count data
warning_data<-read_csv("File Path Here")%>%
  mutate(warn_days=days)%>%
  dplyr::select(WFO, warn_days)
names(warning_data)

warning_data%>%arrange()

#Change "CWA" to "WFO" to join the two data sets
data<-data%>%
  mutate(WFO=CWA)

#Join
data<-left_join(data, warning_data, by = "WFO") # Add warning count data to survey data by WFO
names(data)

data_wfos<-data$WFO

#Find percentiles where the groups are ROUGHLY equal in number of comments--------------------
#Looks like 0-75%, 75-95%, and 95-100% are close
#.   Low     Medium   High 
#.   3152    3024     2933 

data_wx_warnings<-warning_data%>%
  filter(WFO %in% data_wfos)

low_warns<-quantile(data_wx_warnings$warn_days,0.75)
high_warns<-quantile(data_wx_warnings$warn_days,0.95)

#Create a new variable: high/low/medium for number of warnings
data<-data%>%
  mutate(warning_group=ifelse(warn_days<=low_warns,'Low',NA),
         warning_group=ifelse(warn_days>low_warns,'Medium',warning_group),
         warning_group=ifelse(warn_days>high_warns,'High',warning_group),
         warning_group=factor(warning_group,levels=c('Low','Medium','High')))

table(data$warning_group)

#Print CWAs in each group (low/med/high)-------------------------
#CWAs in "Low" group
data%>%
  filter(warning_group=='Low')%>%
  dplyr::select(CWA_full)%>%
  group_by(CWA_full)%>%
  summarise(n=n())%>%
  print(n=500)

#CWAs in "Medium" group
data%>%
  filter(warning_group=='Medium')%>%
  dplyr::select(CWA_full)%>%
  group_by(CWA_full)%>%
  summarise(n=n())%>%
  print(n=500)

#CWAs in "High" group
data%>%
  filter(warning_group=='High')%>%
  dplyr::select(CWA_full,warn_days)%>%
  group_by(CWA_full)%>%
  summarise(n=n())%>%
  print(n=500)

#Recode the time_class variable------------------------------------
data<-data%>%
  mutate(time_class_r=ifelse(time_class=='During the event','1 min - 15 mins',time_class),
         time_class_r=ifelse(time_class=='2 days','1 day',time_class_r),
         time_class_r=ifelse(time_class=='4 days','3 days',time_class_r),
         time_class_r=ifelse(time_class=='5 days','3 days',time_class_r),
         time_class_r=ifelse(time_class=='6 days','3 days',time_class_r),
         time_class_r=factor(time_class_r,levels=c('After event occurred','1 min - 15 mins','16 mins - 59 mins','1 hour-3:59 hours',
                                                   '4 hours-24 hours','1 day','3 days'),
                             labels=c('After event\noccurred','15 mins - \nDuring event','16 - 59 mins','1 - 3:59 hours',
                                      '4 - 24 hours','1 - 2 days','3 - 7 days')))

#Calculate number of comments with each location identifier type: ------------------------------------------
table(data$time_class_r)
missing_loc_info<-data%>% 
  filter(time_class_r=='After event\noccurred' | time_class_r=='15 mins - \nDuring event' | time_class_r=='16 - 59 mins' | time_class_r=='1 - 3:59 hours' | time_class_r=='4 - 24 hours')%>%
  filter(is.na(loc_info)==T)

round(prop.table(table(data$loc_info)),3)

#Plot Figures --------------------

#Figure 5a: The number of comments for all events categorized by time bin for all comments-----------------------
comment_bins<-data%>%
  dplyr::select(time_class_r)%>%
  group_by(time_class_r)%>%
  summarise(n=n())%>%
  mutate(p=n/sum(.$n))%>%
  ggplot()+
  geom_bar(aes(x = reorder(time_class_r, desc(time_class_r)),y=p*100),fill='#1C87B5',stat = 'identity')+
  geom_text(aes(x=time_class_r,y=(p*100)+1.2,label=paste0(round(p*100,0),"%")),size=6)+
  labs(x='Time before forecast severe weather event',y='Percent of comments')+
  theme_minimal(base_size = 22)
comment_bins

ggsave("File Path Here", comment_bins, height = 8, width = 13,bg='white')

#Figure 5b: The number of comments for all events categorized by time bin --------------------------
#           for comments binned down by average number of warning days.
comment_bins_exp<-data%>%
  dplyr::select(time_class_r,warning_group)%>%
  group_by(warning_group)%>%
  mutate(n_group=n())%>%
  group_by(time_class_r,.add=T)%>%
  mutate(n=n(),
         p=n/n_group)%>%
  ggplot()+
  geom_bar(aes(x = reorder(time_class_r, desc(time_class_r)),y=p*100,fill=warning_group),stat = 'identity',position = position_dodge())+
  geom_text(aes(x=time_class_r,y=(p*100)+1,label=paste0(round(p*100,0),"%"),group=warning_group),size=5,position = position_dodge(width=0.9))+
  scale_fill_manual(values = c('#72C5E9','#1C87B5','#104C65'))+
  labs(x='Time before forecast severe weather event',y='Percent of comments',
       fill='Avg.\nWarning\nDays')+
  theme_minimal(base_size = 23)

comment_bins_exp

ggsave("File Path Here", comment_bins_exp, height = 8, width = 15,bg='white')


#Figure 6a: The percent breakdown of questions based on mutually exclusive categories for all questions ----------------------------------------
reply_cat<-data%>%
  dplyr::select(mu_loc:mu_unc)%>%
  pivot_longer(mu_loc:mu_unc)%>%
  group_by(name)%>%
  summarise(p=round(mean(value,na.rm = T)*100,0))%>%
  mutate(name=factor(name,levels=c("mu_loc","mu_cha","mu_tim","mu_sev","mu_pro","mu_haz","mu_imp","mu_unc"),
                     labels=c('Location','Chance','Timing','Severity','Protective\nActions','Hazard','Impacts','Uncertain')))%>%
  ggplot()+
  geom_bar(aes(x = name,y=p,fill=name),stat = 'identity')+
  geom_text(aes(x=name,y=p+1,label=paste0(p,'%')),size=6)+
  scale_fill_manual(values=c("#E41A1C","#4DAF4A", "#377EB8",  "#984EA3", "#FF7F00","#FACAE8","#fccf03",'#A19FA0'))+
  labs(x=' ',y='Percent of comments')+
  theme_minimal(base_size = 23)+
  theme(legend.position = 'none')

reply_cat

ggsave("File Path Here", reply_cat, height = 8, width = 13,bg='white')

#Figure 6b: The percent breakdown of questions based on mutually exclusive ----------------------------------------
#           categories for questions binned down by average number of warning days.
reply_cat_exp<-data%>%
  dplyr::select(mu_loc:mu_unc,warning_group)%>%
  pivot_longer(mu_loc:mu_unc)%>%
  group_by(name,warning_group)%>%
  summarise(p=round(mean(value,na.rm = T)*100,0))%>%
  mutate(name=factor(name,levels=c("mu_loc","mu_cha","mu_tim","mu_sev","mu_pro","mu_haz","mu_imp","mu_unc"),
                     labels=c('Location','Chance','Timing','Severity','Protective\nActions','Hazard','Impacts','Uncertain')))%>%
  ggplot()+
  geom_bar(aes(x = name,y=p,fill=warning_group),stat = 'identity',position = position_dodge())+
  geom_text(aes(x=name,y=p+1.4,label=paste0(p,'%'),group=warning_group),size=5,position = position_dodge(width=0.9))+  
  scale_fill_manual(values = c('#72C5E9','#1C87B5','#104C65'))+
  labs(x=' ',y='Percent of comments',fill='Avg.\nWarning\nDays')+
  theme_minimal(base_size = 23)

reply_cat_exp

ggsave("File Path Here", reply_cat_exp, height = 8, width = 15,bg='white')


#Figure 7a: The change in the percentage makeup of questions by category over time for all questions----------------------------

overall_timeline<-data%>%
  dplyr::select(mu_loc:mu_imp,time_class_r)%>%
  pivot_longer(mu_loc:mu_imp)%>%
  mutate(name=factor(name,levels=c("mu_loc","mu_cha","mu_tim","mu_sev","mu_pro","mu_haz","mu_imp","mu_unc"),
                     labels=c('Location','Chance','Timing','Severity','Pro. Actions','Hazard','Impacts','Uncertain')))%>%
  group_by(time_class_r,name)%>%
  summarise(p=mean(value))%>%
  ggplot()+
  geom_line(aes(x=reorder(time_class_r, desc(time_class_r)),y=p*100,group=name,color=name),linewidth = 1.5)+
  geom_point(aes(x=reorder(time_class_r, desc(time_class_r)),y=p*100,group=name,color=name),size=3.5)+
  scale_color_manual(values=c("#E41A1C","#4DAF4A", "#377EB8",  "#984EA3", "#FF7F00","#FACAE8","#fccf03",'#A19FA0'))+
  labs(x='Time Before Forecast Severe Weather Event',y='Percent of comments', color=' ')+
  theme_minimal(base_size = 23)

overall_timeline

ggsave("File Path Here", overall_timeline, height = 8, width = 15,bg='white')


#Figure 7b: The change in the percentage makeup of questions by category over ----------------------------
#           time for questions binned by average number of warning days.

overall_timeline_exp<-data%>%
  dplyr::select(mu_loc:mu_imp,time_class_r,warning_group)%>%
  pivot_longer(mu_loc:mu_imp)%>%
  mutate(name=factor(name,levels=c("mu_loc","mu_cha","mu_tim","mu_sev","mu_pro","mu_haz","mu_imp","mu_unc"),
                     labels=c('Location','Chance','Timing','Severity','Protective Actions','Hazard','Impacts','Uncertain')))%>%
  mutate(time_class_r=factor(time_class_r,levels=c('After event\noccurred','15 mins - \nDuring event','16 - 59 mins','1 - 3:59 hours',
                                                   '4 - 24 hours','1 - 2 days','3 - 7 days'),
                             labels=c('After ','15m-\nDuring','16-59m','1-3:59h',
                                      '4-24h','1-2d','3-7d')))%>%
  group_by(time_class_r,warning_group,name)%>%
  summarise(p=mean(value))%>%
  ggplot()+
  geom_line(aes(x=reorder(time_class_r, desc(time_class_r)),y=p*100,group = warning_group,color=warning_group),linewidth = 1.5)+
  geom_point(aes(x=reorder(time_class_r, desc(time_class_r)),y=p*100,color=warning_group),size=3.5)+
  facet_wrap(~name,scales='free_y')+
  scale_color_manual(values = c('#72C5E9','#1C87B5','#104C65'))+
  labs(x='Time Before Forecast Severe Weather Event',y='Percent of comments', color='Avg. Warning Days')+
  theme_minimal(base_size = 23)+
  theme(legend.position = c(0.85, 0),
        axis.text.x = element_text(angle = 60, vjust = 0.5))

overall_timeline_exp

ggsave("File Path Here", overall_timeline_exp, height = 8, width = 15,bg='white')

