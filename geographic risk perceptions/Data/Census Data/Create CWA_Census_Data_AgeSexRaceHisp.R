# Libraries and Options -------------------------
library(tidyverse)
library(rgdal)

# Read Data -------------------------
d <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Census Data/County_Census_Data_AgeSexRaceHisp.csv")
d <- d %>% 
  group_by(CWA, MALE, AGE_GROUP, HISP, RACE_GROUP) %>% 
  summarise(DEMGRP_POP = sum(DEMGRP_POP), TOT_POP = sum(TOT_POP)) %>% 
  mutate(DEMGRP_PROP = DEMGRP_POP/TOT_POP)

# CWA Data -----------------------------
setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile")
cwa_shape <- readOGR('.', 'w_11au16')

setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile (Counties)")
cwa_cnty_shp <- readOGR('.','c_11au16')
cwa_cnty_shp$CWA <- substr(cwa_cnty_shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa_cnty_shp <- cwa_cnty_shp[!duplicated(cwa_cnty_shp$FIPS), ] # Remove duplicate counties

# Write Data -------------------------
write_csv(d, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Census Data/CWA_Census_Data_AgeSexRaceHisp.csv")
