# Libraries and Options -------------------------
library(rgdal)
library(tidyverse)
library(data.table)
options(scipen = 999)
options(max.print = 99999)
"%ni%" <- Negate("%in%")

# County Data -------------------------
setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/County Shapefile")
cnty.shp <- readOGR('.','cb_2016_us_county_500k')
cnty.shp$FIPS <- paste0(cnty.shp$STATEFP, cnty.shp$COUNTYFP) # create FIPS variable
cnty.shp <- subset(cnty.shp, STATEFP %ni% c("02","60","66","15","78","72","69")) # limit to CONUS states

# CWA Data -------------------------
setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile")
list.files()
cwa.shp <- readOGR('.','w_11au16')
setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile (Counties)")
cwa.cnty.shp <- readOGR('.','c_11au16')
cwa.cnty.shp$CWA <- substr(cwa.cnty.shp$CWA, start = 1, stop = 3) # keep first CWA in counties that span multiple CWAs
cwa.cnty.shp <- cwa.cnty.shp[!duplicated(cwa.cnty.shp$FIPS), ] # remove duplicate counties

# Census Data -------------------------
census.dat <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Census Data/CWA_Census_Data_AgeSexRaceHisp.csv")
census.dat <- merge(census.dat, with(cwa.shp@data, data.frame(CWA, Region)), by = "CWA", all.x = TRUE) # add nws region id to census data
census.dat <- plyr::rename(census.dat, c("Region" = "CWA_REGION"))

# Survey Data -------------------------
survey.dat <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/base_survey_data.csv")

# Storm Data -------------------------
storm.dat <- read_csv("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Storm Data/Storm_Data_by_CWA.csv")

# Merge Data -------------------------
survey.dat <- merge(survey.dat, storm.dat, by.x = "CWA", by.y = "WFO", all.x = TRUE)
census.dat <- merge(census.dat, storm.dat, by.x = "CWA", by.y = "WFO", all.x = TRUE)

# Write Data -------------------------
write_csv(survey.dat, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/survey_data.csv")
write_csv(census.dat, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/census_data.csv")
