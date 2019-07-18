library(data.table)
library(tidyverse)
library(rgdal)
library(rmapshaper)
library(scales)

setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Storm Data/Storm Data") # https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/
storm_data <- list.files(pattern = "StormEvents_details")
storm_data <- lapply(storm_data, fread, sep = ",")
storm_data <- rbindlist(storm_data)
storm_data$BEGIN_DATE <- substr(storm_data$BEGIN_DATE_TIME, 1, 9)
table(storm_data$EVENT_TYPE)

storm_data$EVENT_TYPE_REC <- NA
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Heat", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HEAT", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Cold", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "COLD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Snow", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Blizzard", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Winter", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Ice", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "SNOW", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Tornado", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "TORN", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Flood", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FLOOD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Surge", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FLOOD", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Hurricane", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Depression", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Tropical Storm", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "HURR", storm_data$EVENT_TYPE_REC)
storm_data$EVENT_TYPE_REC <- ifelse(storm_data$EVENT_TYPE %in% storm_data$EVENT_TYPE[grep("Wildfire", storm_data$EVENT_TYPE, ignore.case = TRUE)], 
                                    "FIRE", storm_data$EVENT_TYPE_REC)

table(storm_data$EVENT_TYPE, storm_data$EVENT_TYPE_REC)

storm_data_counts <- storm_data %>% 
  drop_na(EVENT_TYPE_REC) %>% 
  group_by(WFO, EVENT_TYPE_REC, BEGIN_DATE) %>% 
  count() %>%
  group_by(WFO, EVENT_TYPE_REC) %>% 
  count() %>%
  mutate(m = n/20) %>% # divide by 20 to get yearly averages 
  dplyr::select(WFO, m) %>% 
  spread(EVENT_TYPE_REC, m) %>% 
  as.data.frame() %>% 
  filter(!WFO %in% c("AFC", "AFG", "AJK", "GUM", "HFO", "ASO", "GUA"))

setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Storm Data/Drought Data") # https://droughtmonitor.unl.edu/Data/DataDownload/ComprehensiveStatistics.aspx
drought_data <- list.files(pattern = "dm_export")
drought_data <- lapply(drought_data, fread, sep = ",")
drought_data <- rbindlist(drought_data)
drought_data$drought <- ifelse(drought_data$D1 > 0 | drought_data$D2 > 0 | drought_data$D3 > 0 | drought_data$D4 > 0, 1, 0)

drought_data_counts <- drought_data %>% 
  filter(drought == 1) %>% 
  rename("WFO" = "WeatherForecastOffice") %>% 
  group_by(WFO) %>%
  count() %>% 
  mutate(m = n/19) %>% # divide by 19* to get yearly averages
  dplyr::select(WFO, m) %>% 
  rename("DROUGHT" = "m") %>% 
  filter(!WFO %in% c("AFC", "AFG", "AJK", "GUM", "HFO", "ASO", "GUA"))
  
data <- left_join(storm_data_counts, drought_data_counts, by = "WFO")
data[is.na(data)] <- 0 # Change all NAs to 0

data <- data %>% 
  mutate_at(vars(-WFO), list(~scale(.) %>% as.vector))

write_csv(data, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Storm Data/Storm_Data_by_CWA.csv")
