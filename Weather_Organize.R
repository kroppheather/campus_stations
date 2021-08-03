########################################################
########################################################
###### Script to organize data downloaded from    ######
###### METER weather station.                     ######
########################################################
########################################################
###### Original author: R. Pike                   ###### 
###### Updated by: H. Kropp                       ###### 
########################################################
########################################################

#libraries
library(lubridate)
library(dplyr)
library(ggplot2)


########ggplot2############################
########## User inputs -----------

#### Setting up directories 

# Creating user numbers for each person
UsersAll <- data.frame(userID = c(1,2), userName=c("Student lab","Professor Kropp"))


#most recent tomst download
#assumes downloading all data
TomstD <- "07_28_2021"

# File path for meter data
DirMeter <- c("c:/Google Drive/research/projects/Data/campus_weather/METER/CSV",
             "E:/Google Drive/research/projects/Data/campus_weather/METER/CSV")

DirMeterQ <- c("c:/Google Drive/research/projects/Data/campus_weather/METER/field_qa",
              "E:/Google Drive/research/projects/Data/campus_weather/METER/field_qa")

DirTOMST <- c(paste0("c:/Google Drive/research/projects/Data/campus_weather/TOMST/",TomstD),
              paste0("E:/Google Drive/research/projects/Data/campus_weather/TOMST/",TomstD)) 
# File path to save final data
DirFinal <- c("Insert lab file path here",
              "E:/Google Drive/research/projects/Data/campus_weather/OUT")


# Secondary File path to save final data
DirFinal2 <- c("Insert lab file path here",
              "K:\\Environmental_Studies\\hkropp\\Data\\weather\\Meter")

# Select user - change if needed
user <- 2

############################
########## Meter -----------

#### Set up metadata and initial tables ####
#read in first file

#get all files
meterFiles <- list.files(paste0(DirMeter[user]))

meterTable <- read.csv(paste0(DirMeter[user],"/",meterFiles[1]), skip=3,header=FALSE)


for(i in 2:length(meterFiles)){
  meterTable <- rbind(meterTable,  read.csv(paste0(DirMeter[user],"/",meterFiles[i]), skip=3,header=FALSE))
  
}

colnames(meterTable) <- c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                          "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                          "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp")


#set up day of year
dateForm <-  mdy_hm(meterTable$Date)

meterTable$year <- year(dateForm) 
meterTable$doy <- yday(dateForm)
meterTable$hour <- hour(dateForm)
meterTable$minute <- minute(dateForm)
meterTable$time <- hour(dateForm)+(minute(dateForm)/60)
meterTable$DD <- meterTable$doy + (meterTable$time/24) 
meterTable$DY <- round(meterTable$year+((meterTable$DD-1)/ifelse(leap_year(meterTable$year),366,365)),6)



MeterMeta <- data.frame(name = c("Date","SolRad","Precip","LightningAct","LightningDist","WindDir","WindSpeed",
                                 "GustSpeed","AirTemp","VaporPr","AtmosPr","XLevel","YLevel","MaxPrecip",
                                 "SensorTemp","VPD","BatPct","BatVolt","RefPr","LogTemp"),
                        units = c("MM/DD/YYYY HH:MM",
                                  "W/m^2","mm","NA","km","degree","m/s","m/s","C",
                                  "kPa","kPa","degree","degree","mm/h","C","kPa","%","mV","kPa","C"))


# QAQC report

meterQA <- read.csv(paste0(DirMeterQ[user],"/Notes.csv"))
#parse date
QAdate <- mdy(meterQA$Date)

meterQAout <- data.frame(doy=yday(QAdate),
                         year=year(QAdate),
                         time=meterQA$Time,
                         Comment=meterQA$Comment)

MeterTableO1 <- left_join(meterTable, meterQAout, by=c("doy","year","time"))

############################
########## QA/QC -----------
#add in data flags and QAQC here


############################
########## plots-----------
plot(MeterTableO1$DY, MeterTableO1$AirTemp,type="l")
plot(MeterTableO1$DY, MeterTableO1$Precip, pch=19)
plot(MeterTableO1$DY, MeterTableO1$WindSpeed,type="l")
plot(MeterTableO1$DY, MeterTableO1$GustSpeed,type="l")
plot(MeterTableO1$DY, MeterTableO1$VaporPr,type="l")
#range +- 2 degrees
plot(MeterTableO1$DY, MeterTableO1$XLevel,type="l")
plot(MeterTableO1$DY, MeterTableO1$YLevel,type="l")


tail(MeterTableO1)

############################
########## TOMST-----------
#get files

tomstFiles <- list.files(DirTOMST)

TOMSTSensor <- data.frame(SN= c(91201802,
                                91200065,
                                94207592,
                                94214744,
                                94214743),
                          Height = c(0.25,0.5,0,0,0),
                          location=c("weather",
                                     "weather",
                                     "weather",
                                     "removal",
                                     "control" ))

#read in files
TMS1 <-  read.csv(paste0(DirTOMST[user], "/",tomstFiles[grep(paste0(TOMSTSensor$SN[3]),tomstFiles)]),
                       sep=";",header=FALSE)
TMS2 <-  read.csv(paste0(DirTOMST[user], "/",tomstFiles[grep(paste0(TOMSTSensor$SN[4]),tomstFiles)]),
                  sep=";",header=FALSE)
TMS3 <-  read.csv(paste0(DirTOMST[user], "/",tomstFiles[grep(paste0(TOMSTSensor$SN[5]),tomstFiles)]),
                  sep=";",header=FALSE)
#tms temps:  -6, +2 and +15cm
TMScols <- c("record","date","tz","Tm6","T2","T15","SM","shake","errFlag")

colnames(TMS1) <- TMScols
colnames(TMS2) <- TMScols
colnames(TMS3) <- TMScols

TMS1$dateF <- ymd_hm(TMS1$date)
TMS1$estD <- with_tz(TMS1$dateF,tzone="America/New_York" )

TMS2$dateF <- ymd_hm(TMS2$date)
TMS2$estD <- with_tz(TMS2$dateF,tzone="America/New_York" )

TMS3$dateF <- ymd_hm(TMS3$date)
TMS3$estD <- with_tz(TMS3$dateF,tzone="America/New_York" )


TMS1$location <- rep("weather",nrow(TMS1))
TMS2$location <- rep("removal",nrow(TMS2))
TMS3$location <- rep("control",nrow(TMS3))

TMSbind <- rbind(TMS1,TMS2,TMS3)

#omit error flag data
TMSAll <- TMSbind[TMSbind$errFlag != 16, ]

#correct soil moisture
#use loam until can confirm with geology
TMSAll$SM.cor <- (-0.00000005*(TMSAll$SM^2)) + (0.000398*TMSAll$SM) -0.291

TMSAll$SM.c <- ifelse(TMSAll$SM.cor <= 0.01,NA,TMSAll$SM.cor)

ggplot(TMSAll, aes(estD ,SM.c, col=location))+
  geom_point()+
  geom_line()


ggplot(TMSAll[TMSAll$location != "weather",], aes(estD ,SM.c, col=location))+
  geom_point()+
  geom_line()


ggplot(TMSAll[TMSAll$location != "weather",], aes(estD ,Tm6, col=location))+
  geom_point()+
  geom_line()
#removal
TMS2Q <-  TMS2[TMS2$errFlag != 16, ]
#control
TMS3Q <-  TMS3[TMS3$errFlag != 16, ]
TMScomp <- inner_join(TMS2Q, TMS3Q, by="estD")
TMScomps <- data.frame(estD= TMScomp$estD,
                       diffT= TMScomp$Tm6.x-TMScomp$Tm6.y)
#nned to fiter for installation
ggplot(TMScomps[TMScomps$diffT <3 & TMScomps$diffT >-2,], aes(estD ,diffT))+
  geom_point()+
  geom_line()

#### Save final tables ####


#write.table(MeterTableO1,paste0(DirFinal[user],"/meter/meter_weather_data.csv"),sep=",", row.names=FALSE)
#write.table(MeterMeta,paste0(DirFinal[user],"/meter/meter_weather_metadata.csv"),sep=",", row.names=FALSE)

#write.table(MeterTableO1,paste0(DirFinal2[user],"\\meter_weather_data.csv"),sep=",", row.names=FALSE)
#write.table(MeterMeta,paste0(DirFinal2[user],"\\meter_weather_metadata.csv"),sep=",", row.names=FALSE)


plot(MeterTableO1$DY, MeterTableO1$AirTemp,type="l")
plot(MeterTableO1$DY, MeterTableO1$Precip, pch=19)
plot(MeterTableO1$DY, MeterTableO1$WindSpeed,type="l")
plot(MeterTableO1$DY, MeterTableO1$GustSpeed,type="l")
plot(MeterTableO1$DY, MeterTableO1$VaporPr,type="l")
#range +- 2 degrees
plot(MeterTableO1$DY, MeterTableO1$XLevel,type="l")
plot(MeterTableO1$DY, MeterTableO1$YLevel,type="l")


tail(MeterTableO1)
