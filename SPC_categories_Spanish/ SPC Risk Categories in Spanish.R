#This is the code for the figures and data analysis in "Assessing public
#interpretation of current and linguist-suggested SPC Risk Categories in Spanish"

##Run libraries
library(tidyverse)
library(dplyr)
library(data.table)
library(sf)
library(survey)
library(srvyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(broom)
library(stargazer)
library(DiagrammeR)
library(erer)

##Load the data
sp_data <- fread("https://www.dropbox.com/scl/fi/lszpwf6hw93y8osf8znho/SPC_Spanish.csv?rlkey=trxry5zug7dafkxw13h08m87r&dl=1") %>% 
  rename("weightfactor" = "weight")

##Clean the data
sp_data <- sp_data %>%
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
                                            ifelse(spc_hig_col == 5, "Magenta", NA))))))

data <- sp_data %>%
  mutate(English = ifelse(english == 0, "1. Not at all", 
                          ifelse(english == 1, "2. Not very well",
                                 ifelse(english == 2, "3. Well", 
                                        ifelse(english == 3, "4. Very well", NA))))) %>%
  mutate(Hisp = ifelse(hisp == 0, "1. Not Hispanic", 
                       ifelse(hisp == 1, "2. Mexican",
                              ifelse(hisp == 2, "3. Puerto Rican", 
                                     ifelse(hisp == 3, "4. Cuban",
                                            ifelse(hisp == 4, "5. Other origin", NA))))))

##Filter for a data set that is just the SPC words
dataA <- sp_data %>%
  filter(spc_rand == "A") %>%
  mutate(Word1 = ifelse(spc_mar == 1, "Minimo", 
                        ifelse(spc_sli == 1, "Leve",
                               ifelse(spc_enh == 1, "Elevado", 
                                      ifelse(spc_mod == 1, "Moderado",
                                             ifelse(spc_hig == 1, "Alto", NA)))))) %>%
  mutate(Word2 = ifelse(spc_mar == 2, "Minimo", 
                        ifelse(spc_sli == 2, "Leve",
                               ifelse(spc_enh == 2, "Elevado", 
                                      ifelse(spc_mod == 2, "Moderado",
                                             ifelse(spc_hig == 2, "Alto", NA)))))) %>%
  mutate(Word3 = ifelse(spc_mar == 3, "Minimo", 
                        ifelse(spc_sli == 3, "Leve",
                               ifelse(spc_enh == 3, "Elevado", 
                                      ifelse(spc_mod == 3, "Moderado",
                                             ifelse(spc_hig == 3, "Alto", NA)))))) %>%
  mutate(Word4 = ifelse(spc_mar == 4, "Minimo", 
                        ifelse(spc_sli == 4, "Leve",
                               ifelse(spc_enh == 4, "Elevado", 
                                      ifelse(spc_mod == 4, "Moderado",
                                             ifelse(spc_hig == 4, "Alto", NA)))))) %>%
  mutate(Word5 = ifelse(spc_mar == 5, "Minimo", 
                        ifelse(spc_sli == 5, "Leve",
                               ifelse(spc_enh == 5, "Elevado", 
                                      ifelse(spc_mod == 5, "Moderado",
                                             ifelse(spc_hig == 5, "Alto", NA))))))

##Filter for data set that is just linguist words
dataB <- sp_data %>%
  filter(spc_rand == "B") %>%
  mutate(Word1 = ifelse(spc_mar == 1, "Minimo", 
                        ifelse(spc_sli == 1, "Bajo",
                               ifelse(spc_enh == 1, "Moderado", 
                                      ifelse(spc_mod == 1, "Alto",
                                             ifelse(spc_hig == 1, "Extremo", NA)))))) %>%
  mutate(Word2 = ifelse(spc_mar == 2, "Minimo", 
                        ifelse(spc_sli == 2, "Bajo",
                               ifelse(spc_enh == 2, "Moderado", 
                                      ifelse(spc_mod == 2, "Alto",
                                             ifelse(spc_hig == 2, "Extremo", NA)))))) %>%
  mutate(Word3 = ifelse(spc_mar == 3, "Minimo", 
                        ifelse(spc_sli == 3, "Bajo",
                               ifelse(spc_enh == 3, "Moderado", 
                                      ifelse(spc_mod == 3, "Alto",
                                             ifelse(spc_hig == 3, "Extremo", NA)))))) %>%
  mutate(Word4 = ifelse(spc_mar == 4, "Minimo", 
                        ifelse(spc_sli == 4, "Bajo",
                               ifelse(spc_enh == 4, "Moderado", 
                                      ifelse(spc_mod == 4, "Alto",
                                             ifelse(spc_hig == 4, "Extremo", NA)))))) %>%
  mutate(Word5 = ifelse(spc_mar == 5, "Minimo", 
                        ifelse(spc_sli == 5, "Bajo",
                               ifelse(spc_enh == 5, "Moderado", 
                                      ifelse(spc_mod == 5, "Alto",
                                             ifelse(spc_hig == 5, "Extremo", NA)))))) 


##SPC original words bar graph (Fig. 4)
AllWordPlotA = dataA %>%
  select(weightfactor,Word1,Word2,Word3,Word4,Word5) %>%
  na.omit() %>%
  gather(x, value, Word1:Word5) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(x, value) %>%
  summarize(p = survey_mean(vartype = "ci")) %>%
  ggplot(aes(x, y = p, fill = factor(value, levels=c("Minimo","Leve","Elevado","Moderado","Alto")))) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  scale_fill_manual(name = "Risk Words", breaks = c("Alto", "Moderado", "Elevado", "Leve", "Minimo"), values = c("#FF00FF","#FF0000","#FFA329","#FFFF00","#00B050")) +
  scale_x_discrete(limits= c("Word1","Word2","Word3","Word4","Word5"), labels = c("First\nWord","Second\nWord","Third\nWord","Fourth\nWord","Fifth\nWord")) + theme_bw() +
  scale_y_continuous(limits = c(0,0.7)) +
  labs(title = "Participants' Preferred Risk Word by Order: NWS SPC Original Translations",x = "", y = "Proportion of Responses")

AllWordPlotA

##Common complete answers (Fig. 5)
sp_data %>%
  filter(spc_rand == 'A')%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(ordering = paste(spc_mar, spc_sli, spc_enh, spc_mod, spc_hig)) %>% 
  group_by(ordering) %>%
  summarize(p = survey_mean()) %>% 
  mutate(p = round(p * 100, 2)) %>% 
  arrange(-p) %>% 
  print(n = 30)

sp_data %>%
  filter(spc_rand == 'B')%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(ordering = paste(spc_mar, spc_sli, spc_enh, spc_mod, spc_hig)) %>% 
  group_by(ordering) %>%
  summarize(p = survey_mean()) %>% 
  mutate(p = round(p * 100, 2)) %>% 
  arrange(-p) %>% 
  print(n = 30)

#Outputs are recorded in a table created in the document


##Linguist words bar graph (Fig. 6)
AllWordPlotB = dataB %>%
  select(weightfactor,Word1,Word2,Word3,Word4,Word5) %>%
  na.omit() %>%
  gather(x, value, Word1:Word5) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(x, value) %>%
  summarize(p = survey_mean(vartype = "ci")) %>%
  ggplot(aes(x, y = p, fill = factor(value, levels=c("Minimo","Bajo","Moderado","Alto","Extremo")))) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  scale_fill_manual(name = "Risk Words", breaks = c("Extremo", "Alto", "Moderado", "Bajo", "Minimo"), values = c("#FF00FF","#FF0000","#FFA329","#FFFF00","#00B050")) +
  scale_x_discrete(limits= c("Word1","Word2","Word3","Word4","Word5"), labels = c("First\nWord","Second\nWord","Third\nWord","Fourth\nWord","Fifth\nWord")) + theme_bw() +
  scale_y_continuous(limits = c(0,0.7)) +
  labs(title = "Preferred Risk Word by Order: Linguist Translations",x = "", y = "Proportion of Responses")

AllWordPlotB

##Mean rank order (Fig. 7) 
plot_direct<-sp_data%>%
  select(weightfactor,spc_rand,spc_mar:spc_hig)%>%
  filter(spc_rand=='A')%>%
  pivot_longer(spc_mar:spc_hig)%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(name = factor(name, levels = c("spc_mar","spc_sli",'spc_enh','spc_mod','spc_hig'),
                       labels = c("1.Minimo",
                                  '2.Leve',
                                  '3.Elevado',
                                  '4.Moderado',
                                  '5.Alto')))%>%
  group_by(name) %>%
  summarize(p = survey_mean(value, na.rm = TRUE, vartype = "ci")) %>%
  arrange(p) %>%
  ggplot(., aes(x = 1, y = p, label = name)) +
  geom_point(aes(color = name), size=8) +
  xlim(0.5,1.5) +
  labs(title = "Original SPC Translations",
       y = " ") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:5, labels = c("1 Lower\nRisk","2", "3","4", "5 Higher\nRisk"), limits = c(1,5)) +
  scale_color_manual(values = c('#00B050','#FFFF00','#FFA329','#FF0000','#FF00FF'))+
  ggrepel::geom_text_repel(force = 0.5, nudge_x = 0.15, direction = "y", hjust = 0, segment.size = 0.2) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
plot_direct

plot_linguist<-sp_data%>%
  select(weightfactor,spc_rand,spc_mar:spc_hig)%>%
  filter(spc_rand=='B')%>%
  pivot_longer(spc_mar:spc_hig)%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(name = factor(name, levels = c("spc_mar","spc_sli",'spc_enh','spc_mod','spc_hig'),
                       labels = c("1.Minimo",
                                  '2.Bajo',
                                  '3.Moderado',
                                  '4.Alto',
                                  '5.Extremo')))%>%
  group_by(name) %>%
  summarize(p = survey_mean(value, na.rm = TRUE, vartype = "ci")) %>%
  arrange(p) %>%
  ggplot(., aes(x = 1, y = p, label = name)) +
  geom_point(aes(color = name), size=8) +
  xlim(0.5,1.5) +
  labs(title = "Linguist Translations",
       y = " ") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:5, labels = c("1 Lower\nRisk","2", "3", "4", "5 Higher\nRisk"), limits = c(1,5)) +
  scale_color_manual(values = c('#00B050','#FFFF00','#FFA329','#FF0000','#FF00FF'))+
  ggrepel::geom_text_repel(force = 0.5, nudge_x = 0.15, direction = "y", hjust = 0, segment.size = 0.2) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
plot_linguist

average <- grid.arrange(plot_direct, plot_linguist, nrow = 1, top = "Mean Rankings")

##Mean rank order for the linguist words by English proficiency (Fig. 8)
plot_LingEnglish<-data%>%
  select(weightfactor,English, spc_rand, spc_mar:spc_hig)%>%
  filter(spc_rand=='B')%>%
  pivot_longer(spc_mar:spc_hig)%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(name = factor(name, levels = c("spc_mar","spc_sli",'spc_enh','spc_mod','spc_hig'),
                       labels = c("1.Minimo",
                                  '2.Bajo',
                                  '3.Moderado',
                                  '4.Alto',
                                  '5.Extremo')))%>%
  group_by(name, English) %>%
  summarize(p = survey_mean(value, na.rm = TRUE, vartype = "ci")) %>%
  arrange(p) %>%
  ggplot(., aes(x = 1, y = p, label = name)) +
  geom_point(aes(color = name), size=8) +
  xlim(0.5,1.5) +
  facet_wrap(~ English, nrow = 1) +
  labs(title = "Mean Rankings by English Proficiency: Linguist Translations",
       y = " ") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:5, labels = c("1 Lower\nRisk","2", "3", "4", "5 Higher\nRisk"), limits = c(1,5)) +
  scale_color_manual(values = c('#00B050','#FFFF00','#FFA329','#FF0000','#FF00FF'))+
  ggrepel::geom_text_repel(force = 0.5, nudge_x = 0.15, direction = "y", hjust = 0, segment.size = 0.2) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

plot_LingEnglish

##Mean rank order for the linguist words by Hispanic heritage (Fig. 9)
plot_Linghisp<-data%>%
  select(weightfactor, Hisp, spc_rand, spc_mar:spc_hig)%>%
  filter(spc_rand=='B')%>%
  pivot_longer(spc_mar:spc_hig)%>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  mutate(name = factor(name, levels = c("spc_mar","spc_sli",'spc_enh','spc_mod','spc_hig'),
                       labels = c("1.Minimo",
                                  '2.Bajo',
                                  '3.Moderado',
                                  '4.Alto',
                                  '5.Extremo')))%>%
  group_by(name, Hisp) %>%
  summarize(p = survey_mean(value, na.rm = TRUE, vartype = "ci")) %>%
  arrange(p) %>%
  ggplot(., aes(x = 1, y = p, label = name)) +
  geom_point(aes(color = name), size=8) +
  xlim(0.5,1.5) +
  facet_wrap(~ Hisp, nrow = 1) +
  labs(title = "Mean Rankings by Hispanic Heritage: Linguist Translations",
       y = " ") +
  theme_minimal() +
  scale_y_continuous(breaks = 1:5, labels = c("1 Lower\nRisk","2", "3", "4", "5 Higher\nRisk"), limits = c(1,5)) +
  scale_color_manual(values = c('#00B050','#FFFF00','#FFA329','#FF0000','#FF00FF'))+
  ggrepel::geom_text_repel(force = 0.5, nudge_x = 0.15, direction = "y", hjust = 0, segment.size = 0.2) +
  theme(axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

plot_Linghisp

##SPC colors bar graph (Fig. 10)
AllColorPlot = sp_data %>%
  select(weightfactor,Col1,Col2,Col3,Col4,Col5) %>%
  na.omit() %>%
  gather(x, value, Col1:Col5) %>%
  as_survey_design(ids = 1, weights = weightfactor) %>%
  group_by(x, value) %>%
  summarize(p = survey_mean(vartype = "ci")) %>%
  ggplot(aes(x, y = p, fill = factor(value, levels=c("Green","Yellow","Orange","Red","Magenta")))) +
  geom_bar(stat="identity", position=position_dodge(), color = "black") +
  scale_fill_manual(name = "Risk Colors", breaks = c("Magenta", "Red", "Orange", "Yellow", "Green"), values = c("#FF00FF","#FF0000","#FFA329","#FFFF00","#00B050")) +
  scale_x_discrete(limits= c("Col1","Col2","Col3","Col4","Col5"), labels = c("First\nColor","Second\nColor","Third\nColor","Fourth\nColor","Fifth\nColor")) + theme_bw() +
  scale_y_continuous(limits = c(0,0.7)) +
  labs(title = "Participants' Preferred Risk Color by Order: Spanish Survey",x = "", y = "Proportion of Responses")

AllColorPlot  #color bar graph
