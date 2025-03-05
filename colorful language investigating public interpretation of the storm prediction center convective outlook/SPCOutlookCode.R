# SPC Outlook Ranking Analysis

library(survey)
library(car)
library(psych)
library(vcd)
library(reshape2)
library(tidyverse)
library(Rcpp)
options(scipen=999)
library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(broom)
library(stargazer)
library(DiagrammeR)
library(erer)


#Data Clean up=================================================
data<-read.csv("https://www.dropbox.com/s/9fmplyc61usizuy/ColorfulLanguage.csv?dl=1")

#Numeracy Assessment
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
numeracy<-part1_score+part2_score
data$numeracy<-car::recode(numeracy,"0=1;1=2;NA=NA")

data <- data %>%
  mutate(race=car::recode(data$race,"1=1;2=2;3:7=3")) %>% #white: race=1, black: race=2, other: race=3
  mutate(Salience = (follow)) %>%
  mutate(Responsive = ((6 - resp_ignore) + resp_prot + (6 - resp_busy) + (6 - resp_unsure)) / 4) %>%
  mutate(Prepared = ifelse(rq_4 == 1, 1,0) + ifelse(rq_5 == 1, 1,0) + ifelse(rq_6 == 1, 1,0)) %>%
  mutate(WatchWarnObj = ifelse(is.na(torwatch), ifelse(torwarn == 2, 1, 0),ifelse(torwatch == 1, 1, 0))) %>%
  mutate(nws_region = car::recode(data$nws_region, "'Central Region'=1; 'Eastern Region'=2; 'Southern Region'=3; 'Western Region'=4")) %>%
  #Sort how they ranked each word
  mutate(Word1 = ifelse(spc_mar == 1, "Marginal", 
                        ifelse(spc_sli == 1, "Slight",
                               ifelse(spc_enh == 1, "Enhanced", 
                                      ifelse(spc_mod == 1, "Moderate",
                                             ifelse(spc_hig == 1, "High", NA)))))) %>%
  mutate(Word2 = ifelse(spc_mar == 2, "Marginal", 
                          ifelse(spc_sli == 2, "Slight",
                                 ifelse(spc_enh == 2, "Enhanced", 
                                        ifelse(spc_mod == 2, "Moderate",
                                               ifelse(spc_hig == 2, "High", NA)))))) %>%
  mutate(Word3 = ifelse(spc_mar == 3, "Marginal", 
                          ifelse(spc_sli == 3, "Slight",
                                 ifelse(spc_enh == 3, "Enhanced", 
                                        ifelse(spc_mod == 3, "Moderate",
                                               ifelse(spc_hig == 3, "High", NA)))))) %>%
  mutate(Word4 = ifelse(spc_mar == 4, "Marginal", 
                          ifelse(spc_sli == 4, "Slight",
                                 ifelse(spc_enh == 4, "Enhanced", 
                                        ifelse(spc_mod == 4, "Moderate",
                                               ifelse(spc_hig == 4, "High", NA)))))) %>%
  mutate(Word5 = ifelse(spc_mar == 5, "Marginal", 
                          ifelse(spc_sli == 5, "Slight",
                                 ifelse(spc_enh == 5, "Enhanced", 
                                        ifelse(spc_mod == 5, "Moderate",
                                               ifelse(spc_hig == 5, "High", NA)))))) %>%
  #Sort how they ranked each color
  mutate(Col1 = ifelse(spc_mar_col == 1, "Green", 
                       ifelse(spc_sli_col == 1, "Yellow",
                              ifelse(spc_enh_col == 1, "Orange", 
                                     ifelse(spc_mod_col == 1, "Red",
                                            ifelse(spc_hig_col == 1, "Magenta", NA)))))) %>%
  mutate(Col2 = ifelse(spc_mar_col == 2, "Green", 
                       ifelse(spc_sli_col == 2, "Yellow",
                              ifelse(spc_enh_col == 2, "Orange", 
                                     ifelse(spc_mod_col == 2, "Red",
                                            ifelse(spc_hig_col == 2, "Magenta", NA)))))) %>%
  mutate(Col3 = ifelse(spc_mar_col == 3, "Green", 
                       ifelse(spc_sli_col == 3, "Yellow",
                              ifelse(spc_enh_col == 3, "Orange", 
                                     ifelse(spc_mod_col == 3, "Red",
                                            ifelse(spc_hig_col == 3, "Magenta", NA)))))) %>%
  mutate(Col4 = ifelse(spc_mar_col == 4, "Green", 
                       ifelse(spc_sli_col == 4, "Yellow",
                              ifelse(spc_enh_col == 4, "Orange", 
                                     ifelse(spc_mod_col == 4, "Red",
                                            ifelse(spc_hig_col == 4, "Magenta", NA)))))) %>%
  mutate(Col5 = ifelse(spc_mar_col == 5, "Green", 
                        ifelse(spc_sli_col == 5, "Yellow",
                               ifelse(spc_enh_col == 5, "Orange", 
                                      ifelse(spc_mod_col == 5, "Red",
                                             ifelse(spc_hig_col == 5, "Magenta", NA)))))) %>%
  mutate(WGrp = ifelse(spc_mar == 2 &
                         spc_sli == 1 &
                         spc_enh == 4 &
                         spc_mod == 3 &
                         spc_hig == 5, 1,
                       ifelse(spc_mar == 1 &
                                spc_sli == 2 &
                                spc_enh == 4 &
                                spc_mod == 3 &
                                spc_hig == 5, 2,
                              ifelse(spc_mar == 2 &
                                       spc_sli == 1 &
                                       spc_enh == 3 &
                                       spc_mod == 4 &
                                       spc_hig == 5, 3,
                                     ifelse(spc_mar == 1 &
                                              spc_sli == 2 &
                                              spc_enh == 3 &
                                              spc_mod == 4 &
                                              spc_hig == 5, 0,
                                            ifelse(spc_mar == 2 &
                                                     spc_sli == 1 &
                                                     spc_enh == 5 &
                                                     spc_mod == 3 &
                                                     spc_hig == 4, 4,
                                                   ifelse(spc_mar == 3 &
                                                            spc_sli == 1 &
                                                            spc_enh == 4 &
                                                            spc_mod == 2 &
                                                            spc_hig == 5, 5,
                                                          ifelse(spc_mar == 3 &
                                                                   spc_sli == 1 &
                                                                   spc_enh == 2 &
                                                                   spc_mod == 4 &
                                                                   spc_hig == 5, 6, 7)))))))) %>%
  mutate(CGrp = ifelse(spc_mar_col == 1 &
                         spc_sli_col == 2 &
                         spc_enh_col == 3 &
                         spc_mod_col == 5 &
                         spc_hig_col == 4, 1,
                       ifelse(spc_mar_col == 1 &
                                spc_sli_col == 2 &
                                spc_enh_col == 3 &
                                spc_mod_col == 4 &
                                spc_hig_col == 5, 0,
                              ifelse(spc_mar_col == 1 &
                                       spc_sli_col == 3 &
                                       spc_enh_col == 4 &
                                       spc_mod_col == 5 &
                                       spc_hig_col == 2, 2,
                                     ifelse(spc_mar_col == 1 &
                                              spc_sli_col == 2 &
                                              spc_enh_col == 4 &
                                              spc_mod_col == 5 &
                                              spc_hig_col == 3, 3,
                                            ifelse(spc_mar_col == 1 &
                                                     spc_sli_col == 3 &
                                                     spc_enh_col == 2 &
                                                     spc_mod_col == 5 &
                                                     spc_hig_col == 4, 4,
                                                   ifelse(spc_mar_col == 1 &
                                                            spc_sli_col == 4 &
                                                            spc_enh_col == 3 &
                                                            spc_mod_col == 5 &
                                                            spc_hig_col == 2, 5, 6))))))) %>%
  mutate(SPCscore = 20 - (((1 - spc_mar)**2 + (2 - spc_sli)**2 + (3 - spc_enh)**2 + (4 - spc_mod)**2 + (5 - spc_hig)**2))/2) %>%
  mutate(SPCscoreC = 20 - (((1 - spc_mar_col)**2 + (2 - spc_sli_col)**2 + (3 - spc_enh_col)**2 + (4 - spc_mod_col)**2 + (5 - spc_hig_col)**2))/2) %>%
  mutate(SPCscoreT = (SPCscore + SPCscoreC) / 2) %>%
  mutate(agegroup = car::recode(data$age,"18:29=1;30:44=2;45:59=3;60:74=4;75:120=5"))%>%
  mutate(CorrectW = ifelse(SPCscore == 20, 1, 0)) %>%
  mutate(CorrectC = ifelse(SPCscoreC == 20, 1, 0)) %>%
  mutate(CorrectT = ifelse(CorrectW == 1 & CorrectC == 1, 1, 0)) %>%
  mutate(TimeSpent = ifelse(page_time < 29.00, 0, 
                            ifelse(page_time >= 29.00 & page_time < 44.00, 1, 
                                   ifelse(page_time >= 44.00 & page_time < 61.25, 2, 3)))) %>%
  mutate(is_bluedot = ifelse(is.na(is_bluedot) == T, 0, 1)) %>%
  mutate(totaltime = ifelse(time_taken < 16.00, 0, 
                            ifelse(page_time >= 16.00 & page_time < 23.00, 1, 
                                   ifelse(page_time >= 23.00 & page_time < 32.00, 2, 3))))


#Summaries of data ===================================================

#Fig4: Plot of word order
AllWordPlot = data %>%
  select(Word1,Word2,Word3,Word4,Word5) %>%
  na.omit() %>%
  gather(x, value, Word1:Word5) %>%
  group_by(x) %>%
  count(value) %>%
  mutate(prop = n/3006) %>%
  ggplot(aes(x, y = prop, fill = factor(value, levels=c("Marginal","Slight","Enhanced","Moderate","High")))) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  scale_fill_manual(name = "Risk Words", values = c("#00B050","#FFFF00","#FFA329","#FF0000","#FF00FF")) +
  scale_x_discrete(limits= c("Word1","Word2","Word3","Word4","Word5"), labels = c("First\nWord","Second\nWord","Third\nWord","Fourth\nWord","Fifth\nWord")) + theme_bw() +
  scale_y_continuous(limits = c(0,0.7)) +
  labs(title = "Participants' Preferred Risk Word by Order",x = "", y = "Proportion of Responses")

#Counts and proportions of how participants ordered words
data %>%
  group_by(Word1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Word2) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Word3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Word4) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Word5) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Table 3: How participants grouped the words
wordgroup <- data %>% 
  count(Word1,Word2,Word3,Word4,Word5) %>%
  mutate(Perc = (n / sum(n))*100) %>%
  mutate(Perc=round(Perc, digits=2)) %>%
  arrange(-n)
colnames(wordgroup)<-c("First Word", "Second Word", "Third Word", "Fourth Word", 
                      "Fifth Word", "n", "Percent of\n Participants\n (%)")
grid.newpage()
grid.table(head(wordgroup, n = 10L),  rows=NULL)


#Fig5: Plot of color order
AllColorPlot = data %>%
  select(Col1,Col2,Col3,Col4,Col5) %>%
  na.omit() %>%
  gather(x, value, Col1:Col5) %>%
  group_by(x) %>%
  count(value) %>%
  mutate(prop = n/3006) %>%
  ggplot(aes(x, y = prop, fill = factor(value, levels=c("Green","Yellow","Orange","Red","Magenta")))) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  scale_fill_manual(name = "Risk Colors", values = c("#00B050","#FFFF00","#FFA329","#FF0000","#FF00FF")) +
  scale_x_discrete(limits= c("Col1","Col2","Col3","Col4","Col5"), labels = c("First\nColor","Second\nColor","Third\nColor","Fourth\nColor","Fifth\nColor")) + theme_bw() +
  scale_y_continuous(limits = c(0,0.7)) +
  labs(title = "Participants' Preferred Risk Color by Order",x = "", y = "Proportion of Responses")

#Counts and proportions of how participants ordered colors
data %>%
  group_by(Col1) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Col2) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Col3) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Col4) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
data %>%
  group_by(Col5) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

#Table 4: How participants grouped the colors
colgroup <- data %>%
  count(Col1,Col2,Col3,Col4,Col5) %>%
  mutate(Perc = (n / sum(n))*100) %>%
  mutate(Perc=round(Perc, digits=2)) %>%
  arrange(-n)
colnames(colgroup)<-c("First Color", "Second Color", "Third Color", "Fourth Color", 
                      "Fifth Color", "n", "Percent of\n Participants\n (%)")
grid.newpage()
grid.table(head(colgroup, n = 10L),  rows=NULL)

#Data Analysis =============================================================
#Logistic Models =================
#Prep for augmented model output
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mean_df <- data %>%
  select(agegroup,gend,race,edu,nws_region,WatchWarnObj,numeracy,Salience,TimeSpent) %>%
  mutate(gend = getmode(gend),race = getmode(race),nws_region = getmode(nws_region),WatchWarnObj = getmode(WatchWarnObj)) %>%
  summarize_all(mean,na.rm=TRUE) 

#Logistic Models and summary
modW <- glm(data = data, CorrectW ~ agegroup + as.factor(gend) + as.factor(race) + edu + numeracy + as.factor(nws_region) + Salience + as.factor(WatchWarnObj) + TimeSpent, family = binomial(link = logit), x = TRUE)
modC <- glm(data = data, CorrectC ~ agegroup + as.factor(gend) + as.factor(race) + edu + numeracy + as.factor(nws_region) + Salience + as.factor(WatchWarnObj) + TimeSpent, family = binomial(link = logit), x = TRUE)

summary(modW)
summary(modC)

maBina(modW)
maBina(modC)

#Summaries of correctness by independent variables =================
#Age
modW %>%
  augment(newdata = data.frame(agegroup = 1:5, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> AgeW
modC %>%
  augment(newdata = data.frame(agegroup = 1:5, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) -> AgeC
AC = ggplot(AgeC, aes(agegroup, .fitted)) +
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
  labs(title = "A.) SPC Color Correctness Vs Age Group", x = "Age", y = "Predicted Pr(Correct)") +
  scale_x_discrete(limits = c(1:5),labels=c("18-29", "30-44","45-59","60-74",">75")) +
  scale_y_continuous(limits = c(0,0.4)) +
  theme_bw() + theme(legend.position = "none")

#Gender
MaleW = modW %>%
  augment(newdata = data.frame(gend = 1, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
FemW = modW %>%
  augment(newdata = data.frame(gend = 0, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
GenW = rbind(MaleW,FemW)
GW = ggplot(GenW, aes(gend, .fitted, color = as.factor(gend))) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, size = 1, position = position_dodge(width = 0.5)) +
  labs(title = "C.) SPC Word Correctness Vs Gender", x = "Gender", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.3)) +
  scale_x_discrete(limits = c(0:1),
                   labels = c("Female", "Male")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(limits = c(0,1), 
                                  values = c("red","blue"))

#Race
WhiC = modC %>%
  augment(newdata = data.frame(race = 1, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
BlaC = modC %>%
  augment(newdata = data.frame(race = 2, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
OthC = modC %>%
  augment(newdata = data.frame(race = 3, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
RacC = rbind(WhiC,BlaC,OthC)
RC = ggplot(RacC, aes(race, .fitted, color = as.factor(race))) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5, size = 1, position = position_dodge(width = 0.5)) +
  labs(title = "E.) SPC Color Correctness Vs Race", x = "Race", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.4)) +
  scale_x_discrete(limits = c(1:3),
                   labels = c("White","Black","Other")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(limits = c(1:3),
                                  values = c("blue","black","red"))

#Education
EduW = modW %>%
  augment(newdata = data.frame(edu = 1:8, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
EW = ggplot(EduW, aes(edu, .fitted)) +
  geom_point(size = 2, color = "orange") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "orange") +
  labs(title = "B.) SPC Word Correctness Vs Education", x = "Education", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.3)) +
  scale_x_discrete(limits = c(1:8), labels=c("<High\nSchool","HS/\nGED","Voc./\nTech.", 
                                             "Some\nCol.", "2 year/\nAssoc.", "B.A.", "M.A.", "PhD/\nJD/MD")) +
  theme_bw() + theme(legend.position = "none")

EduC = modC %>%
  augment(newdata = data.frame(edu = 1:8, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)

#Numeracy
NumW = modW %>%
  augment(newdata = data.frame(numeracy = 1:7, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
NW = ggplot(NumW, aes(numeracy, .fitted)) +
  geom_point(size = 2, color = "dark green") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "dark green") +
  labs(title = "A.) SPC Word Correctness Vs Numeracy", x = "Numeracy", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.3)) +
  scale_x_discrete(limits = c(1:7)) +
  theme_bw() + theme(legend.position = "none")


NumC = modC %>%
  augment(newdata = data.frame(numeracy = 1:7, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
NC = ggplot(NumC, aes(numeracy, .fitted)) +
  geom_point(size = 2, color = "dark green") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "dark green") +
  labs(title = "C.) SPC Color Correctness Vs Numeracy", x = "Numeracy", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.4)) +
  scale_x_discrete(limits = c(1:7)) +
  theme_bw() + theme(legend.position = "none")


#Salience (Follow)
SalW = modW %>%
  augment(newdata = data.frame(Salience = 1:5, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)

SalC = modC %>%
  augment(newdata = data.frame(Salience = 1:5, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
SC = ggplot(SalC, aes(Salience, .fitted)) +
  geom_point(size = 2, color = "blue") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "blue") +
  labs(title = "B.) SPC Color Correctness Vs Salience", x = "Salience", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.4)) +
  scale_x_discrete(limits = c(1:5)) + theme_bw() + theme(legend.position = "none")

#Objective Watch/Warning Comprehension
ObjnC = modC %>%
  augment(newdata = data.frame(WatchWarnObj = 0, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
ObjyC = modC %>%
  augment(newdata = data.frame(WatchWarnObj = 1, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
ObjC= rbind(ObjnC,ObjyC)
OC = ggplot(ObjC, aes(WatchWarnObj, .fitted, color = as.factor(WatchWarnObj))) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3, size = 1, position = position_dodge(width = 0.5)) +
  labs(title = "F.) SPC Color Correctness Vs\nWatch/Warning Understanding", x = "Watch/Warning Understanding", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.4)) +
  scale_x_discrete(limits = c(0:1), labels = c("Incorrect", "Correct")) +
  theme_bw() + theme(legend.position = "none") + scale_color_manual(limits = c(0,1),
                                  values = c("red","blue")) 

#Time Spent
TimW = modW %>%
  augment(newdata = data.frame(TimeSpent = 0:3, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)

TimC = modC %>%
  augment(newdata = data.frame(TimeSpent = 0:3, mean_df), type.predict = "response", se_fit=TRUE) %>%
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)
TC = ggplot(TimC, aes(TimeSpent, .fitted)) +
  geom_point(size = 2, color = "purple") + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1, color = "purple") +
  labs(title = "D.) SPC Color Correctness Vs Work Time", x = "Time Spent on Problem (seconds)", y = "Predicted Pr(Correct)") +
  scale_y_continuous(limits = c(0,0.4)) +
  scale_x_discrete(limits = c(0:4), labels = c("<29", "29-44", "44-61.25", ">61.25")) +
  theme_bw() + theme(legend.position = "none")

#Fig. 6 SPC Word Correctness
#windows(8,6)
grid.arrange(NW,EW,GW, nrow = 2)
#Fig. 7 SPC Color Correctness
#windows(8,9)
grid.arrange(AC,NC,TC,RC,OC, nrow = 3)



