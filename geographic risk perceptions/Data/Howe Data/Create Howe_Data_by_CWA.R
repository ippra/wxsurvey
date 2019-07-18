# Libraries and Options -------------------------
library(tidyverse)
library(rgdal)

# Read Data -------------------------
howe_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Howe Data/riskp_county_table.csv")

# CWA Data -----------------------------
census_data <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Census Data/County_Census_Data_AgeSexRaceHisp.csv")
census_data$FIPS <- as.numeric(as.character(census_data$FIPS))
census_data <- census_data %>% 
  filter(MALE == 1, AGE_GROUP == 1, HISP == 1, RACE_GROUP == 1) %>% 
  dplyr::select(FIPS, CWA, TOT_POP)

howe_data <- left_join(howe_data, census_data, by = c("CountyFP" = "FIPS"))
howe_data <- howe_data %>% drop_na(CWA)

# Aggregate -----------------------------
howe_data_by_cwa <- howe_data %>% 
  group_by(CWA) %>% 
  summarise(CWA_TOT_POP = sum(TOT_POP)) %>% 
  right_join(., howe_data, by = "CWA") %>% 
  mutate(COUNTY_PROP = TOT_POP / CWA_TOT_POP) %>% 
  mutate(riskp_est = riskp_est * COUNTY_PROP) %>%
  group_by(CWA) %>%
  summarise(riskp_est = sum(riskp_est)) %>% 
  arrange(-riskp_est)

# Write Data -------------------------
write_csv(howe_data_by_cwa, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Howe Data/Howe_Data_by_CWA.csv")
