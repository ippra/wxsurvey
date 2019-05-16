# Libraries and Options ---------------------------------------------------------------
install.county("osx")

library(data.table)
library(rgdal)
library(rgeos)
library(maps)
library(UScensus2010)
library(UScensus2010county)
library(ggmap)
library(geosphere)
library(effects)
library(car)
library(erer)
library(stargazer)
library(tidyverse)
data(oklahoma.county10)
'%ni%'<-Negate('%in%')
options(scipen=999)

# Survey Data ---------------------------------------------------------------
recruitment.data <- read_csv("/Users/makenzie.krocak/Downloads/survey.data1.csv")
recruitment.data <- filter(recruitment.data, is.na(recruitment.data$long) == FALSE)

# Watch/Warning Data ---------------------------------------------------------------
setwd("/Users/makenzie.krocak/Desktop/PhD Stuff/CRCM PhD Stuff/False positives work/wwa_201601010000_201612310000")
iem.torn.shp <- readOGR('.', 'wwa_201601010000_201612310000') #IEM Watch/Warning Archive
iem.torn.shp$UTCDATE <- paste(substr(iem.torn.shp$ISSUED, 5, 6), substr(iem.torn.shp$ISSUED, 7, 8), substr(iem.torn.shp$ISSUED, 3, 4), sep = "/")
iem.torn.shp$UTCTIME <- paste(substr(iem.torn.shp$ISSUED, 9, 10), substr(iem.torn.shp$ISSUED, 11, 12), sep = ":")
iem.torn.shp$UTCDATETIME <- paste(iem.torn.shp$UTCDATE, iem.torn.shp$UTCTIME)
iem.torn.shp$UTCDATETIME <- as.POSIXct(strptime(iem.torn.shp$UTCDATETIME, "%m/%d/%y %H:%M", tz = "UTC"))
iem.torn.shp$CSTDATETIME <- format(iem.torn.shp$UTCDATETIME, tz = "CST6CDT", usetz = TRUE)
iem.torn.shp$CSTDATE <- substr(iem.torn.shp$CSTDATETIME, 1, 10)
iem.torn.shp <- subset(iem.torn.shp,WFO %in% c("OUN", "TSA", "AMA", "LUB", "FWD", "SHV", "LZK", "SGF", "ICT", "DDC", "PUB"))
table(subset(iem.torn.shp, PHENOM %in% c("TO", "FF", "SV") & SIG == "W" & GTYPE == "P")$SIG)

# Projections ---------------------------------------------------------------
albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
NAD83 <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
oklahoma.county10 <- spTransform(oklahoma.county10, albers)
iem.torn.shp <- spTransform(iem.torn.shp, albers)
coordinates(recruitment.data) <-~ long + lat
proj4string(recruitment.data) <- wgs84
recruitment.data <- spTransform(recruitment.data, albers)

# Geographic Overlays
system1.data <- subset(recruitment.data, storm_system == 1 & storm_rec_warn %in% 0:1)
system2.data <- subset(recruitment.data, storm_system == 2 & storm_rec_warn %in% 0:1)

system1.data$per_warn_rec <- system1.data$storm_rec_warn
system1.data$act_warn_rec <- data.frame(system1.data, over(system1.data, subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), logical = TRUE))$CSTDATE
system1.data$act_warn_rec <- ifelse(is.na(system1.data$act_warn_rec), 0, 1)

system2.data$per_warn_rec <- system2.data$storm_rec_warn
system2.data$act_warn_rec <- data.frame(system2.data, over(system2.data, subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), logical = TRUE))$CSTDATE
system2.data$act_warn_rec <- ifelse(is.na(system2.data$act_warn_rec), 0, 1)

# Reception Matrices ---------------------------------------------------------------
system1.data$warn_rec_matrix <- NA
system1.data$warn_rec_matrix <- ifelse(system1.data$act_warn_rec == 1 & system1.data$per_warn_rec == 1, "True Positive", system1.data$warn_rec_matrix)
system1.data$warn_rec_matrix <- ifelse(system1.data$act_warn_rec == 1 & system1.data$per_warn_rec == 0, "False Negative", system1.data$warn_rec_matrix)
system1.data$warn_rec_matrix <- ifelse(system1.data$act_warn_rec == 0 & system1.data$per_warn_rec == 1, "False Positive", system1.data$warn_rec_matrix)
system1.data$warn_rec_matrix <- ifelse(system1.data$act_warn_rec == 0 & system1.data$per_warn_rec == 0, "True Negative", system1.data$warn_rec_matrix)
apr_TP<-round(sum(ifelse(system1.data$warn_rec_matrix=='True Positive',1,0))/sum(table(system1.data$warn_rec_matrix))*100,2)
apr_FP<-round(sum(ifelse(system1.data$warn_rec_matrix=='False Positive',1,0))/sum(table(system1.data$warn_rec_matrix))*100,2)
apr_TN<-round(sum(ifelse(system1.data$warn_rec_matrix=='True Negative',1,0))/sum(table(system1.data$warn_rec_matrix))*100,2)
apr_FN<-round(sum(ifelse(system1.data$warn_rec_matrix=='False Negative',1,0))/sum(table(system1.data$warn_rec_matrix))*100,2)

system2.data$warn_rec_matrix <- NA
system2.data$warn_rec_matrix <- ifelse(system2.data$act_warn_rec == 1 & system2.data$per_warn_rec == 1, "True Positive", system2.data$warn_rec_matrix)
system2.data$warn_rec_matrix <- ifelse(system2.data$act_warn_rec == 1 & system2.data$per_warn_rec == 0, "False Negative", system2.data$warn_rec_matrix)
system2.data$warn_rec_matrix <- ifelse(system2.data$act_warn_rec == 0 & system2.data$per_warn_rec == 1, "False Positive", system2.data$warn_rec_matrix)
system2.data$warn_rec_matrix <- ifelse(system2.data$act_warn_rec == 0 & system2.data$per_warn_rec == 0, "True Negative", system2.data$warn_rec_matrix)
may_TP<-round(sum(ifelse(system2.data$warn_rec_matrix=='True Positive',1,0))/sum(table(system2.data$warn_rec_matrix))*100,2)
may_FP<-round(sum(ifelse(system2.data$warn_rec_matrix=='False Positive',1,0))/sum(table(system2.data$warn_rec_matrix))*100,2)
may_TN<-round(sum(ifelse(system2.data$warn_rec_matrix=='True Negative',1,0))/sum(table(system2.data$warn_rec_matrix))*100,2)
may_FN<-round(sum(ifelse(system2.data$warn_rec_matrix=='False Negative',1,0))/sum(table(system2.data$warn_rec_matrix))*100,2)

# Map Reception Matrices ---------------------------------------------------------------
system1.data$color <- car::recode(system1.data$warn_rec_matrix, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system1.data$color)

system2.data$color <- car::recode(system2.data$warn_rec_matrix, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system2.data$color)

#Figure 1
plot(oklahoma.county10, lwd = 1, border = "black")
plot(subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), add = TRUE, col = "#FF000040", border = "#FF0000", lwd = 2)
text(-500000, -160000, paste("True Positive = ",apr_TP,'%',sep=''), cex = 1.7, col = '#40e54b')
text(-500000, -200000,paste("False Negative = ",apr_FN,'%',sep=''), cex = 1.7, col = '#6948ff')
text(-500000, -240000,paste("False Positive = ",apr_FP,'%',sep=''), cex = 1.7, col = '#e54040')
text(-500000, -280000,paste("True Negative = ",apr_TN,'%',sep=''), cex = 1.7, col = '#5e5e5e')
points(system1.data, col = system1.data$color, pch = 19, cex = 1.25)

#Figure 2
plot(oklahoma.county10, lwd = 1, border = "black")
plot(subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), add = TRUE, col = "#FF000040", border = "#FF0000", lwd = 2)
text(-500000, -160000, paste("True Positive = ",may_TP,'%',sep=''), cex = 1.7, col = '#40e54b')
text(-500000, -200000,paste("False Negative = ",may_FN,'%',sep=''), cex = 1.7, col = '#6948ff')
text(-500000, -240000,paste("False Positive = ",may_FP,'%',sep=''), cex = 1.7, col = '#e54040')
text(-500000, -280000,paste("True Negative = ",may_TN,'%',sep=''), cex = 1.7, col = '#5e5e5e')
points(system2.data, col = system2.data$color, pch = 19, cex = 1.25)

# ---------------------------------------------------------Geographic Spillover (Buffering) ---------------------------------------------------------------
warnings<-subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P")
warning_to_buffer<-spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), CRS( "+proj=lcc +lon_0=-100 +lat_0=39.8")) 
poly_buff<-gBuffer(warning_to_buffer, width = 16093.4, quadsegs = 10)
poly_buff <- spTransform(poly_buff, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83"))

warnings2<-subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P")
warning_to_buffer2<-spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), CRS( "+proj=lcc +lon_0=-100 +lat_0=39.8")) 
poly_buff2<-gBuffer(warning_to_buffer2, width = 16093.4, quadsegs = 10)
poly_buff2 <- spTransform(poly_buff2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83"))

system1.data_geo <- cbind(system1.data, dist2Line(spTransform(system1.data, NAD83), spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), NAD83)))
system1.data_geo$distance <- system1.data_geo$distance * 0.000621371
system2.data_geo <- cbind(system2.data, dist2Line(spTransform(system2.data, NAD83), spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), NAD83)))
system2.data_geo$distance <- system2.data_geo$distance * 0.000621371
system1.data_geo$distance <- ifelse(system1.data$act_warn_rec == 1, 0, system1.data_geo$distance)
system2.data_geo$distance <- ifelse(system2.data$act_warn_rec == 1, 0, system2.data_geo$distance)

system1.data$warn_rec_matrix2 <- NA
system1.data$warn_rec_matrix2 <- ifelse(system1.data_geo$distance < 10 & system1.data$per_warn_rec == 1, "True Positive", system1.data$warn_rec_matrix2)
system1.data$warn_rec_matrix2 <- ifelse(system1.data_geo$distance < 10 & system1.data$per_warn_rec == 0, "False Negative", system1.data$warn_rec_matrix2)
system1.data$warn_rec_matrix2 <- ifelse(system1.data_geo$distance > 10 & system1.data$per_warn_rec == 1, "False Positive", system1.data$warn_rec_matrix2)
system1.data$warn_rec_matrix2 <- ifelse(system1.data_geo$distance > 10 & system1.data$per_warn_rec == 0, "True Negative", system1.data$warn_rec_matrix2)
apr_TP_geo<-round(sum(ifelse(system1.data$warn_rec_matrix2=='True Positive',1,0))/sum(table(system1.data$warn_rec_matrix2))*100,2)
apr_FP_geo<-round(sum(ifelse(system1.data$warn_rec_matrix2=='False Positive',1,0))/sum(table(system1.data$warn_rec_matrix2))*100,2)
apr_TN_geo<-round(sum(ifelse(system1.data$warn_rec_matrix2=='True Negative',1,0))/sum(table(system1.data$warn_rec_matrix2))*100,2)
apr_FN_geo<-round(sum(ifelse(system1.data$warn_rec_matrix2=='False Negative',1,0))/sum(table(system1.data$warn_rec_matrix2))*100,2)

system2.data$warn_rec_matrix2 <- NA
system2.data$warn_rec_matrix2 <- ifelse(system2.data_geo$distance < 10 & system2.data$per_warn_rec == 1, "True Positive", system2.data$warn_rec_matrix2)
system2.data$warn_rec_matrix2 <- ifelse(system2.data_geo$distance < 10 & system2.data$per_warn_rec == 0, "False Negative", system2.data$warn_rec_matrix2)
system2.data$warn_rec_matrix2 <- ifelse(system2.data_geo$distance > 10 & system2.data$per_warn_rec == 1, "False Positive", system2.data$warn_rec_matrix2)
system2.data$warn_rec_matrix2 <- ifelse(system2.data_geo$distance > 10 & system2.data$per_warn_rec == 0, "True Negative", system2.data$warn_rec_matrix2)
may_TP_geo<-round(sum(ifelse(system2.data$warn_rec_matrix2=='True Positive',1,0))/sum(table(system2.data$warn_rec_matrix2))*100,2)
may_FP_geo<-round(sum(ifelse(system2.data$warn_rec_matrix2=='False Positive',1,0))/sum(table(system2.data$warn_rec_matrix2))*100,2)
may_TN_geo<-round(sum(ifelse(system2.data$warn_rec_matrix2=='True Negative',1,0))/sum(table(system2.data$warn_rec_matrix2))*100,2)
may_FN_geo<-round(sum(ifelse(system2.data$warn_rec_matrix2=='False Negative',1,0))/sum(table(system2.data$warn_rec_matrix2))*100,2)

#Map the Geospill (Buffer) Figures
system1.data$color <- car::recode(system1.data$warn_rec_matrix2, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system1.data$color)

system2.data$color <- car::recode(system2.data$warn_rec_matrix2, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system2.data$color)

#Figure 3
plot(oklahoma.county10, lwd = 1, border = "black")
plot(warnings,add = T, col = "#FF000050", border = "#FF0000", lwd = 2)
plot(poly_buff,add = T, col = "#FF000040", border='#FF000010')
points(system1.data, col = system1.data$color, pch = 19, cex = 1.25)
text(-500000, -160000, paste("True Positive = ",apr_TP_geo,'%',sep=''), cex = 1.7, col = '#40e54b')
text(-500000, -200000,paste("False Negative = ",apr_FN_geo,'%',sep=''), cex = 1.7, col = '#6948ff')
text(-500000, -240000,paste("False Positive = ",apr_FP_geo,'%',sep=''), cex = 1.7, col = '#e54040')
text(-500000, -280000,paste("True Negative = ",apr_TN_geo,'%',sep=''), cex = 1.7, col = '#5e5e5e')


#Figure 4
plot(oklahoma.county10, lwd = 1, border = "black")
plot(warnings2,add = T, col = "#FF000050", border = "#FF0000", lwd = 2)
plot(poly_buff2,add = T, col = "#FF000040", border='#FF000010')
points(system2.data, col = system2.data$color, pch = 19, cex = 1.25)
text(-500000, -160000, paste("True Positive = ",may_TP_geo,'%',sep=''), cex = 1.7, col = '#40e54b')
text(-500000, -200000,paste("False Negative = ",may_FN_geo,'%',sep=''), cex = 1.7, col = '#6948ff')
text(-500000, -240000,paste("False Positive = ",may_FP_geo,'%',sep=''), cex = 1.7, col = '#e54040')
text(-500000, -280000,paste("True Negative = ",may_TN_geo,'%',sep=''), cex = 1.7, col = '#5e5e5e')

#---------------------------------------------------------Watch vs. Warning Confusion ---------------------------------------------------------------
system1.data$color <- car::recode(system1.data$warn_rec_matrix, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system1.data$color)

system2.data$color <- car::recode(system2.data$warn_rec_matrix, "'True Positive' = '#40e54b80'; 'False Negative' = '#6948ff80'; 'False Positive' = '#e5404080'; 'True Negative' = '#5e5e5e80'")
table(system2.data$color)

#Figure 5
plot(oklahoma.county10,lwd=1,border="black")
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="TO" & SIG=="A" & GTYPE=="C" & CSTDATETIME!="2016-04-29 15:30:00 CDT"),add=T,border="#FFFF00",col="#FFFF0030",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="TO" & SIG=="W" & GTYPE=="P"),add=T,col="#FF000050",border="#FF0000",lwd=2)
points(system1.data,col=system1.data$color,pch=19,cex=1.25)

#Figure 6
plot(oklahoma.county10,lwd=1,border="black")
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="TO" & SIG=="A" & GTYPE=="C" & CSTDATETIME < as.POSIXct("2016-05-09 15:00:00 CDT")),add=T,border="#FFFF00",col="#FFFF0030",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="TO" & SIG=="W" & GTYPE=="P"),add=T,col="#FF000050",border="#FF0000",lwd=2)
points(system2.data,col=system2.data$color,pch=19,cex=1.25)

#--------------------------------------------------------------------Warning Inundation ---------------------------------------------------------------
#Figure 7
plot(oklahoma.county10,lwd=1,border="black")
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="FF" & SIG=="W" & GTYPE=="P"),add=T,border="#13a925",col="#13a92530",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="SV" & SIG=="W" & GTYPE=="P"),add=T,border="#FFFF00",col="#FFFF0030",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="TO" & SIG=="W" & GTYPE=="P"),add=T,col="#FF000050",border="#FF0000",lwd=2)
points(system1.data,col=system1.data$color,pch=19,cex=1.25)

#Figure 8
plot(oklahoma.county10,lwd=1,border="black")
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="FF" & SIG=="W" & GTYPE=="P"),add=T,border="#13a925",col="#13a92530",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="SV" & SIG=="W" & GTYPE=="P"),add=T,border="#FFFF00",col="#FFFF0030",lwd=2)
plot(subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="TO" & SIG=="W" & GTYPE=="P"),add=T,col="#FF000050",border="#FF0000",lwd=2)
points(system2.data,col=system2.data$color,pch=19,cex=1.25)

# ----------------------------------------------------------Generate regression Variables ---------------------------------------------------------------
#in a watch
system1.data$in_watch<-data.frame(system1.data,over(system1.data,subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM=="TO" & SIG=="A" & GTYPE=="C"),logical=T))$CSTDATE
system1.data$in_watch<-ifelse(is.na(system1.data$in_watch),0,1)
table(system1.data$in_watch)

system2.data$in_watch<-data.frame(system2.data,over(system2.data,subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM=="TO" & SIG=="A" & GTYPE=="C"),logical=T))$CSTDATE
system2.data$in_watch<-ifelse(is.na(system2.data$in_watch),0,1)
table(system2.data$in_watch)

#in another warning
system1.data$in_oth_warn<-data.frame(system1.data,over(system1.data,subset(iem.torn.shp,CSTDATE %in% c("2016-04-29") & PHENOM %in% c("SV","FF") & SIG=="W" & GTYPE=="P"),logical=T))$CSTDATE
system1.data$in_oth_warn<-ifelse(is.na(system1.data$in_oth_warn),0,1)
table(system1.data$in_oth_warn)

system2.data$in_oth_warn<-data.frame(system2.data,over(system2.data,subset(iem.torn.shp,CSTDATE %in% c("2016-05-09") & PHENOM %in% c("SV","FF") & SIG=="W" & GTYPE=="P"),logical=T))$CSTDATE
system2.data$in_oth_warn<-ifelse(is.na(system2.data$in_oth_warn),0,1)
table(system2.data$in_oth_warn)

#number of tornado warnings received on the event day
system1.data$num_tor_warnings<-sapply(over(system1.data,subset(iem.torn.shp,PHENOM=="TO" & SIG=="W" & GTYPE=="P" & CSTDATETIME < as.POSIXct("2016-08-01 00:00:00 CDT")),returnList=TRUE),nrow)
system2.data$num_tor_warnings<-sapply(over(system2.data,subset(iem.torn.shp,PHENOM=="TO" & SIG=="W" & GTYPE=="P" & CSTDATETIME < as.POSIXct("2016-08-01 00:00:00 CDT")),returnList=TRUE),nrow)

#distance from nearest tornado warning
system1.data <- cbind(system1.data, dist2Line(spTransform(system1.data, NAD83), spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-04-29") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), NAD83)))
max(system1.data$distance)
system1.data$distance <- system1.data$distance * 0.000621371
max(system1.data$distance)
system2.data <- cbind(system2.data, dist2Line(spTransform(system2.data, NAD83), spTransform(subset(iem.torn.shp, CSTDATE %in% c("2016-05-09") & PHENOM == "TO" & SIG == "W" & GTYPE == "P"), NAD83)))
system2.data$distance <- system2.data$distance * 0.000621371
system1.data$distance <- ifelse(system1.data$act_warn_rec == 1, 0, system1.data$distance)
system2.data$distance <- ifelse(system2.data$act_warn_rec == 1, 0, system2.data$distance)

#merge system.data with recruitment.data
system.data<-rbind(with(system1.data@data,data.frame(p_id,warn_rec_matrix,warn_rec_matrix2,distance,in_watch,in_oth_warn,num_tor_warnings)),with(system2.data@data,data.frame(p_id,warn_rec_matrix,warn_rec_matrix2,distance,in_watch,in_oth_warn,num_tor_warnings)))
names(system.data)
recruitment.data<-merge(recruitment.data,system.data,by="p_id",all.x=T)
recruitment.data$storm_system<-ifelse(recruitment.data$storm_system==0,NA,recruitment.data$storm_system)
dim(recruitment.data)

#number of days after event
recruitment.data$end_date<-as.Date(format(as.POSIXct(recruitment.data$end_datetime,origin="1970-01-01",tz="CST6CDT"),'%Y-%m-%d'))
recruitment.data$days_after_warn<-NA
recruitment.data$days_after_warn<-ifelse(recruitment.data$storm_system==1,recruitment.data$end_date-as.Date("2016-04-29"),recruitment.data$days_after_warn)
recruitment.data$days_after_warn<-ifelse(recruitment.data$storm_system==2,recruitment.data$end_date-as.Date("2016-05-09"),recruitment.data$days_after_warn)

#subjective reception variables
recruitment.data$rec_subscribe<-car::recode(recruitment.data$rec_subscribe,"'Strongly disagree'=1;'Disagree'=2;'Somewhat disagree'=3;'Neither agree nor disagree'=4;'Somewhat agree'=5;'Agree'=6;'Strongly agree'=7")

#weather info sources
recruitment.data$wthr_info_web1<-car::recode(recruitment.data$wthr_info_web,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_govweb1<-car::recode(recruitment.data$wthr_info_govweb,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_loctv1<-car::recode(recruitment.data$wthr_info_loctv,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_cabtv1<-car::recode(recruitment.data$wthr_info_cabtv,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_radio1<-car::recode(recruitment.data$wthr_info_radio,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_fam1<-car::recode(recruitment.data$wthr_info_fam,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_soc1<-car::recode(recruitment.data$wthr_info_soc,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_phone1<-car::recode(recruitment.data$wthr_info_phone,"'Never'=1;'Less than once per week'=2;'About once per week'=3;'Several times per week'=4;'About once a day'=5;'Several times a day'=6; NA=1")
recruitment.data$wthr_info_TV1<-rowMeans(with(recruitment.data@data,data.frame(wthr_info_cabtv1,wthr_info_loctv1)))
recruitment.data$wthr_info_int1<-rowMeans(with(recruitment.data@data,data.frame(wthr_info_govweb1,wthr_info_web1)))
recruitment.data$wthr_info_soc_network1<-rowMeans(with(recruitment.data@data,data.frame(wthr_info_soc1,wthr_info_fam1)))

#code race
recruitment.data$race1<-car::recode(recruitment.data$race,"'1'=0;'2'=1;'3'=2;'4'=3;'5'=3;'6'=3;'7'=3")

#recruitment.data$distance<-ifelse(recruitment.data$distance >150, NA, recruitment.data$distance)
#hist(recruitment.data$distance)


# Fit Model, Table 4 ---------------------------------------------------------------
outside.data<-subset(recruitment.data@data,warn_rec_matrix %in% c("False Positive","True Negative"))
false_positive<-glm(ifelse(warn_rec_matrix=="False Positive",1,0)~log(distance)+in_watch+in_oth_warn+days_after_warn+
                      num_tor_warnings+age+gender+race1+education+I(income/10000)+
                      wthr_info_TV1+wthr_info_int1+wthr_info_soc_network1+wthr_info_radio1+wthr_info_phone1+
                      rec_subscribe,
                      data=outside.data,x=T,family='binomial')
maBina(false_positive)
false_positive
1 - (994.1/1074) #--> R2 = 0.074
#Significant: distance,  in_oth_warn, num_tor_warnings, age, wthr_info_soc_network, rec_subscribe.
#107 missing from 954

#plot distance effect
means<-as.data.frame.list(colMeans(with(outside.data,data.frame(distance,in_watch,in_oth_warn,days_after_warn,num_tor_warnings, age,gender,race1,
                                                                wthr_info_TV1,wthr_info_int1,wthr_info_soc_network1,wthr_info_radio1,wthr_info_phone1,
                                                                  rec_subscribe,education,income)),na.rm=T))
p.distance<-predict(false_positive,newdata=data.frame(distance=1:100,means),type="response",se.fit=T)

plot(p.distance$fit,type="l",lty=1,ylab="Prop",cex.lab=1.25,xlab='Miles',ylim=c(0,1),main="Distance",cex.axis=1.5,yaxs="i",xaxt='n',cex.main=1.5,font.main=1,lwd=4)
lines(p.distance$fit+(1.96*p.distance$se.fit),type="l",lty=1,col="steelblue",lwd=2)
lines(p.distance$fit-(1.96*p.distance$se.fit),type="l",lty=1,col="steelblue",lwd=2)
axis(1,seq(1,100,25),seq(1,100,25),cex.axis=1.5)



