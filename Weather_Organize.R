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



############################
########## User inputs -----------

#### Setting up directories 

# Creating user numbers for each person
UsersAll <- data.frame(userID = c(1,2), userName=c("Student","Professor Kropp"))



# File path for meter data
DirMeter <- c("Insert lab file path here",
             "E:/Google Drive/research/projects/Data/campus_weather/METER/CSV")

DirMeterQ <- c("Insert lab file path here",
              "E:/Google Drive/research/projects/Data/campus_weather/METER/field_qa")
 
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
