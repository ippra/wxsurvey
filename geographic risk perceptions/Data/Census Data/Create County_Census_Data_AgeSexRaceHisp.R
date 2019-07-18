# Libraries and Options -------------------------
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
options(scipen = 999)
options(max.print = 99999)
"%ni%" <- Negate("%in%")

# Census Data ------------------------- 
# Data: https://www.census.gov/data/datasets/2016/demo/popest/counties-detail.html
# Codebook: https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2016/cc-est2016-alldata.pdf
# data <- fread("https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/counties/asrh/cc-est2016-alldata-40.csv") # Only Oklahoma
p <- paste0("https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/counties/asrh/cc-est2016-alldata-", 
            formatC(1:56, width = 2, format = "d", flag = "0")[-c(3, 7, 14, 43, 52)], ".csv")
l <- lapply(p, fread)
data <- do.call("rbind", l)
data <- subset(data, YEAR == 9) # 7/1/2016 population estimate
data <- subset(data, AGEGRP %in% 4:18) # Age 15 to 19 years - Age 85 years or older
data <- subset(data, STNAME %ni% c("Alaska", "Hawaii"))

data$AGEGRP <- car::recode(data$AGEGRP, "4:7 = 1; 8:12 = 2; 13:18 = 3")
data <- data[,c("STATE", "COUNTY", "AGEGRP", 
                "NHWA_MALE", "NHBA_MALE", "NH_MALE", "HWA_MALE", "HBA_MALE", "H_MALE",
                "NHWA_FEMALE", "NHBA_FEMALE", "NH_FEMALE", "HWA_FEMALE", "HBA_FEMALE", "H_FEMALE"), ] %>%
  group_by(STATE, COUNTY, AGEGRP) %>% 
  summarise_all(funs(sum))

data <- data.frame(
  subset(data, AGEGRP == 1)[, c("STATE", "COUNTY")],
  subset(data, AGEGRP == 1)$NHWA_MALE,
  subset(data, AGEGRP == 1)$NHBA_MALE,
  subset(data, AGEGRP == 1)$NH_MALE - (subset(data, AGEGRP == 1)$NHWA_MALE + subset(data, AGEGRP == 1)$NHBA_MALE),
  subset(data, AGEGRP == 1)$HWA_MALE,
  subset(data, AGEGRP == 1)$HBA_MALE,
  subset(data, AGEGRP == 1)$H_MALE - (subset(data, AGEGRP == 1)$HWA_MALE + subset(data, AGEGRP == 1)$HBA_MALE),
  subset(data, AGEGRP == 1)$NHWA_FEMALE,
  subset(data, AGEGRP == 1)$NHBA_FEMALE,
  subset(data, AGEGRP == 1)$NH_FEMALE - (subset(data, AGEGRP == 1)$NHWA_FEMALE + subset(data, AGEGRP == 1)$NHBA_FEMALE),
  subset(data, AGEGRP == 1)$HWA_FEMALE,
  subset(data, AGEGRP == 1)$HBA_FEMALE,
  subset(data, AGEGRP == 1)$H_FEMALE - (subset(data, AGEGRP == 1)$HWA_FEMALE + subset(data, AGEGRP == 1)$HBA_FEMALE),
  subset(data, AGEGRP == 2)$NHWA_MALE,
  subset(data, AGEGRP == 2)$NHBA_MALE,
  subset(data, AGEGRP == 2)$NH_MALE - (subset(data, AGEGRP == 2)$NHWA_MALE + subset(data, AGEGRP == 2)$NHBA_MALE),
  subset(data, AGEGRP == 2)$HWA_MALE,
  subset(data, AGEGRP == 2)$HBA_MALE,
  subset(data, AGEGRP == 2)$H_MALE - (subset(data, AGEGRP == 2)$HWA_MALE + subset(data, AGEGRP == 2)$HBA_MALE),
  subset(data, AGEGRP == 2)$NHWA_FEMALE,
  subset(data, AGEGRP == 2)$NHBA_FEMALE,
  subset(data, AGEGRP == 2)$NH_FEMALE - (subset(data, AGEGRP == 2)$NHWA_FEMALE + subset(data, AGEGRP == 2)$NHBA_FEMALE),
  subset(data, AGEGRP == 2)$HWA_FEMALE,
  subset(data, AGEGRP == 2)$HBA_FEMALE,
  subset(data, AGEGRP == 2)$H_FEMALE - (subset(data, AGEGRP == 2)$HWA_FEMALE + subset(data, AGEGRP == 2)$HBA_FEMALE),
  subset(data, AGEGRP == 3)$NHWA_MALE,
  subset(data, AGEGRP == 3)$NHBA_MALE,
  subset(data, AGEGRP == 3)$NH_MALE - (subset(data, AGEGRP == 3)$NHWA_MALE + subset(data, AGEGRP == 3)$NHBA_MALE),
  subset(data, AGEGRP == 3)$HWA_MALE,
  subset(data, AGEGRP == 3)$HBA_MALE,
  subset(data, AGEGRP == 3)$H_MALE - (subset(data, AGEGRP == 3)$HWA_MALE + subset(data, AGEGRP == 3)$HBA_MALE),
  subset(data, AGEGRP == 3)$NHWA_FEMALE,
  subset(data, AGEGRP == 3)$NHBA_FEMALE,
  subset(data, AGEGRP == 3)$NH_FEMALE - (subset(data, AGEGRP == 3)$NHWA_FEMALE + subset(data, AGEGRP == 3)$NHBA_FEMALE),
  subset(data, AGEGRP == 3)$HWA_FEMALE,
  subset(data, AGEGRP == 3)$HBA_FEMALE,
  subset(data, AGEGRP == 3)$H_FEMALE - (subset(data, AGEGRP == 3)$HWA_FEMALE + subset(data, AGEGRP == 3)$HBA_FEMALE)
  )
names(data) <- c("STATE", "COUNTY", paste0("DEMGRP_",formatC(1:36, width = 2, format = "d", flag = "0")))
data$FIPS <- paste0(formatC(data$STATE, width = 2, format = "d", flag = "0"), 
                    formatC(data$COUNTY, width = 3, format = "d", flag = "0"))
data <- with(data, data.frame(FIPS, data[, 3:38]))
sort(table(data$FIPS))

data.long <- gather(data, DEMGRP, DEMGRP_POP, -FIPS)
data.long <- arrange(data.long, FIPS, DEMGRP)
data.long$MALE <- rep(rep(c(rep(1, 6), rep(0, 6)), 3), nrow(data)) # 0 = Male; 1 = Female
data.long$AGE_GROUP <- rep(c(rep(1, 12), rep(2, 12), rep(3, 12)), nrow(data)) # 1 = 15-34; 2 = 35-59; 3 = 60+ 
data.long$HISP <- rep(rep(c(rep(0, 3), rep(1, 3)), 6), nrow(data)) # 0 = Hisp; 1 = Not Hisp
data.long$RACE_GROUP <- rep(rep(1:3, 12), nrow(data)) # 1 = White alone; 2 = Black alone; 3 = Other
sort(table(data.long$FIPS)/36)

tot.data <- aggregate(DEMGRP_POP ~ FIPS, data.long, sum)
names(tot.data) <- c("FIPS", "TOT_POP")
data.long <- merge(data.long, tot.data, by = "FIPS", all.x = TRUE)
sort(table(data.long$FIPS)/36)

org.data <- do.call("rbind", l)
org.data$FIPS <- paste0(formatC(org.data$STATE, width = 2, format = "d", flag = "0"), 
                    formatC(org.data$COUNTY, width = 3, format = "d", flag = "0"))
org.data <- subset(org.data, AGEGRP == 0 & YEAR == 9 & STNAME %ni% c("Alaska", "Hawaii"))
data.long <- merge(data.long, with(org.data, data.frame(FIPS, STATE, COUNTY, STNAME, CTYNAME)), by = "FIPS", all.x = TRUE)
data.long <- data.long[,c("STATE","COUNTY","STNAME","CTYNAME","FIPS","MALE","AGE_GROUP","HISP","RACE_GROUP","DEMGRP_POP","TOT_POP")]
data.long$DEMGRP_PROP <- data.long$DEMGRP_POP / data.long$TOT_POP
sort(table(data.long$FIPS)/36)

# CWA Data -------------------------
setwd("~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/CWA Shapefile (Counties)")

cwa.cnty.shp <- readOGR('.','c_11au16')
cwa.cnty.shp$CWA <- substr(cwa.cnty.shp$CWA, start = 1, stop = 3) # Only keep first CWA in counties that span multiple CWAs
cwa.cnty.shp <- cwa.cnty.shp[!duplicated(cwa.cnty.shp$FIPS), ] # Remove duplicate counties
data.long <- merge(data.long, with(cwa.cnty.shp@data, data.frame(FIPS, CWA)), by = "FIPS", all.x = TRUE)

# Write Data -------------------------
write.csv(data.long, "~/Desktop/Geographic Distributions of Extreme Weather Risk Perceptions/Data/Census Data/County_Census_Data_AgeSexRaceHisp.csv", row.names = FALSE)

