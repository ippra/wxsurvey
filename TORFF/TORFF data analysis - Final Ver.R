# Libraries and options ---------------------
library(tidyverse)
library(tidytext)
library(data.table)
library(directlabels)
library(ggrepel)
library(ggpubr)

# Data ---------------------

#This data contains the proportions based on ONLY the count of the mentions of tornado, rain, and flood.
stats <- readRDS("/Users/annawanless/Dropbox (Univ. of Oklahoma)/IPPRA/WX_Surveys/Public Links DO NOT TOUCH/GIThubdata/Sean/TORFF/TORFF_Coverage_Deidentified.Rds") 
                    #You may need to download file to your local computer and paste pathname here

# Event Sums (by proportion) ==========================================================
CaseA = 'Case A (29 July 2021)'
CaseB = 'Case B (17 March 2021)'
CaseC = 'Case C (27 Mar. 2021)'
CaseD = 'Case D (18 May 2021)'
CaseE = 'Case E (17 Aug. 2021)'
CaseF = 'Case F (01 Sept. 2021)'


EV1 <- stats %>% filter(event == "Case A (29 July 2021)") %>%
  mutate(labelh = 0.1 * p)
EV1$labelh <- with(EV1, ave(labelh, source, FUN = function(f) max(f, na.rm=T))) 

EV2 <- stats %>% filter(event == "Case B (17 March 2021)") %>%
  mutate(labelh = 0.1 * p)
EV2$labelh <- with(EV2, ave(labelh, source, FUN = function(f) max(f, na.rm=T)))

EV3 <- stats %>% filter(event == "Case C (27 March 2021)")%>%
  mutate(labelh = 0.025 * p)
EV3$labelh <- with(EV3, ave(labelh, source, FUN = function(f) max(f, na.rm=T)))

EV4 <- stats %>% filter(event == "Case D (18 May 2021)")%>%
  mutate(labelh = 0.1 * p)
EV4$labelh <- with(EV4, ave(labelh, source, FUN = function(f) max(f, na.rm=T)))

EV5 <- stats %>% filter(event == "Case E (17 August 2021)")%>%
  mutate(labelh = 0.1 * p)
EV5$labelh <- with(EV5, ave(labelh, source, FUN = function(f) max(f, na.rm=T)))

EV6 <- stats %>% filter(event == "Case F (1 September 2021)")%>%
  mutate(labelh = 0.1 * p)
EV6$labelh <- with(EV6, ave(labelh, source, FUN = function(f) max(f, na.rm=T)))


#Event averages

EV1avg <- EV1 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV1avg$labelh <- with(EV1avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EV2avg <- EV2 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV2avg$labelh <- with(EV2avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EV3avg <- EV3 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV3avg$labelh <- with(EV3avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EV4avg <- EV4 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV4avg$labelh <- with(EV4avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EV5avg <- EV5 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV5avg$labelh <- with(EV5avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EV6avg <- EV6 %>%
  group_by(event, name) %>%
  summarize(meanp = mean(p)) %>%
  mutate(labelh = 0.1 * meanp)
EV6avg$labelh <- with(EV6avg, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EVavg = rbind(rbind(rbind(rbind(rbind(EV1avg,EV2avg), EV3avg), EV4avg), EV5avg), EV6avg)

## Figure 3: Proportions 
ggplot(EVavg, aes(x = fct_reorder(name, -meanp), y = meanp, fill = name)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 20) +
  facet_wrap(~event, scales = "free", ncol = 2) +
  guides(fill = "none") +
  theme(strip.background = element_blank()) +
  labs(x = "", y = "Proportion of Sentences") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_x_discrete(limits = c("tornado","flood","rain")) +
  scale_fill_manual(name = "name", values = c("#07474f","#1e90ff","#b31b1b")) +
  geom_text(aes(label = (signif(meanp, digits = 3)*100)), nudge_y = EV1avg$labelh, size = 5) + 
  theme(strip.text.x = element_text(size = 20))
  


#Figure 4: Difference plots ===========================================================================

TorFF <- readRDS("/Users/annawanless/Dropbox (Univ. of Oklahoma)/IPPRA/WX_Surveys/Public Links DO NOT TOUCH/GIThubdata/Sean/TORFF/TORvsFF_Deidentified.Rds")
                    #You may need to download file to your local computer and paste pathname here


ggplot(TorFF, aes(x = sentence_num, y = TorVsFF, color = source)) +
  geom_line(size = 1.25) +
  geom_hline(yintercept = 0, linetype = 2, size = 1) +
  facet_wrap(~event, scales = "free", ncol = 2) +
  labs(x = "Sentence", y = "Tornado - Flood Mentions") +
  theme_classic(base_size = 20) +
  scale_color_brewer(palette="Paired") +
  theme(strip.background = element_blank()) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(legend.position="none") 


#Case Study plots =======================================================================

#Figure 5: Proportion plots
CaseG = 'Case G (31 May 2013)'
CaseH = 'Case H (26 August 2017)'

Casedat <- readRDS("/Users/annawanless/Dropbox (Univ. of Oklahoma)/IPPRA/WX_Surveys/Public Links DO NOT TOUCH/GIThubdata/Sean/TORFF/Casedat_Deidentified.Rds")
                    #You may need to download file to your local computer and paste pathname here

EVE <- Casedat %>% filter(event == CaseG) %>%
  mutate(labelh = 0.1 * p)
EVE$labelh <- with(EVE, ave(labelh, event, FUN = function(f) max(f, na.rm=T))) 

EVH <- Casedat %>% filter(event == CaseH) %>%
  mutate(labelh = 0.1 * p)
EVH$labelh <- with(EVH, ave(labelh, event, FUN = function(f) max(f, na.rm=T)))

EVcase = rbind(EVE,EVH)

ggplot(EVcase, aes(x = fct_reorder(name, -p), y = p, fill = name)) +
  geom_bar(stat = "identity") +
  theme_classic(base_size = 25) +
  facet_wrap(~event, scales = "free") +
  guides(fill = "none") +
  theme(strip.background = element_blank()) +
  labs(x = "", y = "Sentences") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_x_discrete(limits = c("tornado","flood","rain")) +
  scale_fill_manual(name = "name", values = c("#07474f","#1e90ff","#b31b1b")) +
  geom_text(aes(label = (signif(p, digits = 3)*100)), nudge_y = EVcase$labelh, size = 5) + 
  theme(strip.text.x = element_text(size = 20))

#Figure 6: Difference plot
TorFFCase <- readRDS("/Users/annawanless/Dropbox (Univ. of Oklahoma)/IPPRA/WX_Surveys/Public Links DO NOT TOUCH/GIThubdata/Sean/TORFF/TorFFCase_Deidentified.Rds")

ggplot(TorFFCase, aes(x = sentence_num, y = TorVsFF)) +
  geom_line(size = 1.25) +
  geom_hline(yintercept = 0, linetype = 2, size = 1) +
  facet_wrap(~event, scales = "free") +
  labs(x = "Sentence", y = "Tornado - Flood Mentions") +
  theme_classic(base_size = 25) +
  theme(strip.background = element_blank()) +
  theme(strip.text.x = element_text(size = 20)) +
  theme(legend.position="none") 
