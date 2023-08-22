# Toepeck Ubelix RUN

# required libraries #
library(data.table)
library(dplyr)
library(lubridate)
library("readxl")
library(tidyr)
library(hms)
library(zoo)
library(fst)
library(gdata)
library('stringr')

# helper functions #

# Merge a list of timelines into a single timeline
# Parameter: a list of timelines
# Output: a single timeline
mergeHenData <- function(henDataList) {
  combined <- rbindlist(henDataList)
  setkeyv(combined, c("Time", "Hen"))
  setindex(combined, Hen)
  return(combined)
}
# Remove non-transitions:
# Apply a function to a timeline hen-wise
# Parameters: a function to transform a single-hen timeline, a timeline
# Output: a single transformed timeline
applyPerHen <- function(f, data) {
  mergeHenData(Map(f, splitHenData(data)))
}

compactHenData <- function(henData) {
  # See https://www.rdocumentation.org/packages/data.table/versions/1.14.0/topics/rleid
  #   TRUE for first element in groups of repeated elements, FALSE otherwise
  # henData[, .I == .N] is a "is it the last row" vector
  return(henData[!duplicated(rleid(henData$Tierlevel)) | henData[, .I == .N],]) # corrected Zone to Tierlevel
}

# Convert a timeline into an list of single-hen timelines
# Parameter: a timeline
# Output: a list of single-hen timelines
splitHenData <- function(data) {
  if (nrow(data)) {
    hens <- unique(data[, Hen])
    splitData <- list()
    for (henID in hens) {
      splitData <- append(splitData, list(data[Hen == henID]))
    }
  } else { # Data is empty - return a list with one empty dataframe
    splitData <- list(data) 
  }
  return(splitData)
}

# Add a Duration column to a timeline
# Calculated for each row as the time difference to next row; 0 for the last row
# Parameter: a single-hen timeline
# Output: a timeline with durations
addDurations <- function(henData) {
  newData <- copy(henData)
  # Calculate duration of entry as difference to next entry
  newData[, Duration := (shift(Time, type="lead") - Time)]
  # Set duration for last entry as 0
  newData[nrow(newData), Duration := as.difftime(0, units="secs")]
  return(data.table(newData))
}

#Return the distance traveled between two zones
#Parameters: vector containing two zones to compare
defineDistance = function(zones,date,hen) {
  
  zone1 = zones[1]
  zone2 = zones[2]
  
  print(paste("Date:", date, "Hen:", hen, "Zone1:", zone1, "Zone2:", zone2))
  
  if((zone1 == zone2)|
     is.na(zone1) | is.na(zone2)){
    distance = 0
  } 
  else if ((is.na(zone1) & zone2 == "Ramp_Nestbox") |
     (is.na(zone2) & zone1 == "Ramp_Nestbox") |
     (is.na(zone1) & zone2 == "Tier_2") |
     (is.na(zone2) & zone1 == "Tier_2") |
     (is.na(zone1) & zone2 == "Litter") |
     (is.na(zone2) & zone1 == "Litter") |
     (is.na(zone1) & zone2 == "Tier_4") |
     (is.na(zone2) & zone1 == "Tier_4")){
    distance = NA
  }
  else if((zone1 == "Tier_4" & zone2 == "Ramp_Nestbox")|
     (zone2 == "Tier_4" & zone1 == "Ramp_Nestbox")|
     (zone1 == "Tier_2" & zone2 == "Ramp_Nestbox")|
     (zone2 == "Tier_2" & zone1 == "Ramp_Nestbox")|
     (zone1 == "Tier_2" & zone2 == "Litter")|
     (zone1 == "Litter" & zone2 == "Tier_2")){
    distance = 1
  }
  else if((zone1 == "Tier_4" & zone2 == "Tier_2")|
          (zone2 == "Tier_4" & zone1 == "Tier_2")|
          (zone1 == "Litter" & zone2 == "Ramp_Nestbox")|
          (zone2 == "Litter" & zone1 == "Ramp_Nestbox")
          ){
    distance = 2
  }
  else if((zone1 == "Tier_4" & zone2 == "Litter")|
          (zone2 == "Tier_4" & zone1 == "Litter")){
    distance = 3
  }
  else {
    stop("Error: the zones contained an unknown zone")
  }
  return(distance)
}

# get data of specific hen #
#setwd("//resstore.unibe.ch/vetsuisse_chicken_run/FATCAT/RFID/TEST")
#setwd("//resstore.unibe.ch/vetsuisse_chicken_run/FATCAT/RFID/newTEST")

# Loading the data #
# Get the list of .csv filenames under a given path
filenames <- list.files(pattern = "\\.csv$", ignore.case=TRUE)

#filename <-filenames[5]
# Load all the files into separate data.table tables
filesData <- lapply(filenames, function(filename) {
  # To have any warnings printed out immediately
  options(warn=1)
  # To correlate warnings with the file
  print(filename)
  
  # Read lines from file
  con <- file(file.path(filename))
  lines <- readLines(con)
  close(con)
  
  # Regular expression pattern
  # Example string: "28.04.2021 16:43:12.66 DA760104 16400088 11"
  pattern <- "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2}(\\.\\d{1,2})? [0-9A-F]{8} \\d{8} \\d{1,2}$"
  #pattern <- "^\\d{2}\\.\\d{2}\\.\\d{4} \\d{2}:\\d{2}:\\d{2} [0-9A-F]{8} \\d{8} \\d{2}$"
  
  # Drop lines that don't match the pattern (as a result of a broken write)
  lines <- lines[grepl(pattern, lines)]
  
  # Glue lines together back into a file + last line break
  text <- paste0(paste0(lines, collapse="\n"), "\n")
  
  # Parse as data.table
  fread(text=text,fill=T)
})


# Merge data in a single data.table
# It can still be unordered
memory.limit(size = 56000)

data <- rbindlist(filesData)


# Free up separate files' data
str(data)
head(data)
#View(data)
dim(data)
#write.csv(data,"rawdata", row.names = FALSE)
#df <- read.csv2("//resstore.unibe.ch/vetsuisse_chicken_run/FATCAT/RFID/rawdata.csv", sep=",")
df <- data
names(df)
#save.image(file="rawdata.Rdata")

#names(df)
#data <- df

glimpse(data)

# Data has the following shape at this point (auto-assigned column names:
# V1: "DD.MM.YYYY" date
# V2: "HH:MM:SS.SS" timestamp
# V3: Hexadecimal ID of the antenna that detected the tag
# V4: Decimal ID of the tag that was detected
# Note that it can be unordered
# Also note that the timestamps are not super-precise

#rename column names V3 to be HenID and V4 to be Antenna
head(data)
str(data)
names(data)[1] <- "Date"
names(data)[2] <- "Time"
names(data)[3] <- "Hen"
names(data)[4] <- "Antenna"
names(data)[5] <- "Coil"
sort(unique(data$Antenna))

############ ANTENNA long format###############
#rm(antenna.ID)
#antenna.ID <- read_excel("//nas-vetsuisse/VETSUISSE/Gruppen/VPHI/Welfare/2- Research Projects/Masha Marincek/Projects/Ca Timing/Antennas_Hobos/Antennas_Position.xlsx")
antenna.ID <- read_excel("/storage/research/vetsuisse_chicken_run/FATCAT/RFID/newTest/Antennas_Position.xlsx")
#antenna.ID <- read_excel("Antennas_Position.xlsx")

sort(unique(antenna.ID$Position))
head(antenna.ID)
str(antenna.ID)
antenna.ID$Pen <- as.factor(antenna.ID$Pen)
sort(unique(antenna.ID$Pen))
antenna.ID$side <- as.factor(antenna.ID$side)
antenna.ID$left_right <- as.factor(antenna.ID$left_right)
antenna.ID$Position <- as.factor(antenna.ID$Position)
str(antenna.ID)
str(data)
data$Antenna <-as.numeric(data$Antenna)
data.antenna <- merge(data,antenna.ID,by.x="Antenna",by.y ="Antenna_ID",all.x = T,allow.cartesian = T)
head(data.antenna)
sort(unique(data.antenna$side))
length(sort(unique(data.antenna$Hen)))
table(data.antenna$Pen,data.antenna$side)

# add Pen Info and link to BirdID
#rm(IDs)
#### TODO #### MERGE ID file with Health file of Amina and exclude all birds == X ####
#IDs <- read_excel("//nas-vetsuisse/VETSUISSE/Benutzer/yg18q990/Project_Toepecking/BirdID_Pen_ID.xlsx")
IDs <- read_excel("/storage/research/vetsuisse_chicken_run/FATCAT/RFID/newTest/BirdID_Pen_ID.xlsx")

head(IDs)
names(IDs)[2] <- "Pen_Bird"

toepeckID <- read_excel("finalHA_Toepecking.xlsx")
head(toepeckID)
names(toepeckID)[1] <- "Pen_Bird_found"
str(IDs)
str(toepeckID)
str(data.antenna)

head(data.antenna)
dim(IDs)
dim(data.antenna)
head(IDs)
data.IDs <- merge(toepeckID,IDs,by.x="Hen_ID",by.y ="Serial_N",all.x=T)
head(data.IDs)
head(data.antenna)
data.full <- merge(data.antenna,data.IDs,by.x="Hen",by.y ="Hen_ID",all.y=T)
str(data.full)
data.full$Pen <- as.numeric(as.character(data.full$Pen))
data.full <- subset(data.full, Pen == 14)
#data.full <- subset(data.full, Pen < 14)
rm(filesData)
rm(data)
rm(df)
rm(data.antenna)
rm(IDs)

write_fst(data.full, 'data.full_P14.fst')

str(data.full)
data.full.14 <- data.full
rm(data.full)

# allhens reload #
data.full.14$Time <- format(as.POSIXct(data.full.14$Time, format="%H:%M:%S"), format="%H:%M:%S")
setDT(data.full.14)
str(data.full.14)

# Parse datetime with lubridate - understands fractional seconds
data.full.14[, clockTime := dmy_hms(paste(Date, Time))]

data.full.14$Time <- as_hms(data.full.14$Time) #Time soll hms num werden
str(data.full.14)

#table(data.full.14$Hen,data.full.14$Pen)
data.full.14$Hen <- as.factor(data.full.14$Hen)
data.full.14$side <- as.factor(data.full.14$side)
data.full.14$Position <- as.factor(data.full.14$Position)
data.full.14$Pen <- as.factor(data.full.14$Pen)
data.full.14$Pen_Bird <- as.factor(data.full.14$Pen_Bird)
data.full.14$Pen_Bird_found <- as.factor(data.full.14$Pen_Bird_found)
data.full.14$HenID <- as.factor(data.full.14$Hen)
data.full.14$Tierlevel <- data.full.14$Position
levels(data.full.14$Tierlevel) <- list(Litter = "Corner piling",Litter  = "Litter_center", Litter = "Litter_peripheral",Litter ="Pophole_inside", Wintergarten="Pophole_outside",Tier_2="Tier_1",Ramp_Nestbox="Tier_3",Tier_4="Tier_4")
#Zone ergänzen
data.full.14$Zone <- as.factor(data.full.14$Tierlevel)
levels(data.full.14$Position)
levels(data.full.14$Zone)
data.full.14 <- setkeyv(data.full.13, c("Hen","Date","Time"))

sparseData <- applyPerHen(compactHenData, data.full.14)# for all dates use data.full.14
head(data.full.14)

# Order by timestamp then hen
sparseData.1 <- setkeyv(sparseData, c("Hen","Date","Time"))
head(sparseData)
allhens=data.frame(matrix(ncol=0, nrow=0))
hens <- unique(sparseData.1$Hen)
henID <- hens[1]
for (henID in hens) {
  henData <- sparseData[Hen == henID]
  henData$Time <- lubridate::hms(henData$Time)
  henData$Time <- period_to_seconds(henData$Time) 
  henData <- addDurations(henData)
  print(henID)
  
  allhens=rbind(henData,allhens)
}

# there are minus durations due to overlap to the next date
# therefore: deletion of minus durations
allhens <- allhens[allhens$Duration>=0]
#plot(allhens$Duration)
allhens_allpens <- allhens

rm(allhens)

########## Further processing #########
# calculate daily parameters per bird 
trackingData <- allhens_allpens
names(allhens_allpens)
head(allhens_allpens)
str(trackingData)
# sort by date and henID first

#Then arrange in descending order by Brand and SaleDate
rm(allhens_allpens)
rm(data.full.14)

trackingData.1 <- arrange(trackingData,Hen, clockTime)
str(trackingData.1)
head(trackingData.1)
rm(trackingData)


write_fst(trackingData.1,'trackingData.1.P14.fst')

###### Nestbox zone ########
#Nestbox entries per bird
#shift out only those in the morning, relevant for egg laying not resting (until 10)
#extract if hen was in nest zone on day or not
dailyNest = trackingData.1[hour(clockTime) < 10, .(NestZone = ifelse(any(Zone == "Ramp_Nestbox"), 1, 0)), by = .(Hen, Date)][order(Date, Hen)]
#extract how long each hen was in the box
dailyNest[NestZone == 1, DurationNest := trackingData.1[hour(clockTime) < 10 & Zone == "Ramp_Nestbox", sum(Duration), by = .(Hen, Date)][order(Date, Hen), V1]]
#dailyNest[ , NestSleep := trackingData.1[
#  if(trackingData.1[, .SD[1], by = .(Hen, Date), .SDcols = "distZone"]=="Ramp_Nestbox") {hour(Time) < 9, Zone == "Ramp_Nestbox", by = .(HenID, Date)][order(Date, HenID), V1]]}
#dailyNest[NestZone == 0 | NestSleep == TRUE, DurationNest := 0]

#extract when median duration in the nest is reached
dailyNest[, MedDurNest := ifelse(NestZone == 1,  round(DurationNest/2),NA) ]
## extract median timepoint as clocktime:
#1. Extract the first clockTime where conditions are met in trackingData.1
first_clockTimes <- trackingData.1[hour(clockTime) < 10 & Zone == "Ramp_Nestbox",
                                   .(Hen, Date, first_clockTime = min(clockTime)),
                                   by = .(Hen, Date)]
#2. Merge the result back into dailyNest
dailyNest <- dailyNest[first_clockTimes, EntryNest := i.first_clockTime, on = c("Hen", "Date")]
dailyNest[NestZone == 1 & DurationNest > 0, MedTimeNest := as.ITime(EntryNest + MedDurNest)]
head(dailyNest)
# maybe add something to correct for those not having left nest in between midnight and 10 ocklock - so far all 0!!!

#switches in and out of the nestbox zone
# helper = trackingData[Light == T & hour(Time) < 9 & Zone == "Ramp_Nestbox" & LightIndic != 1, .(SwitchesNest = .N),by = .(HenID, Date)]
# dailyNest = helper[dailyNest, on = c("HenID", "Date")]
# helper = dailyNest[, .(HenID, Date, MedTimeNest)][trackingData, on = c("HenID", "Date")][Time < MedTimeNest & Time > (MedTimeNest -(3600)),]
# helper[, nextZone := c(distZone[-1], NA), by = HenID]
# #calculate distance travelled by using function defineDistance
# helper[, distVertical := apply(X = cbind(distZone, nextZone), MARGIN = 1, FUN= defineDistance), by = HenID]
# 
# dailyNest = helper[,.(preNestDist = sum(distVertical)),by = .(HenID, Date)][dailyNest, on = c("HenID", "Date")]

head(dailyNest)

# ###### wintergarden use #####
# #extract if hen goes out on day or not
dailyGarten = trackingData.1[, .(Out = ifelse(any(Zone == "Wintergarten"), 1, 0)), by = .(Hen, Date)][order(Date, Hen)]
#extract how long each hen went out per day
dailyGarten[Out == 1, DurationGarten := trackingData.1[Zone == "Wintergarten", .(sum(Duration)), by = .(Hen, Date)][order(Date, Hen), V1]]
dailyGarten[Out == 0, DurationGarten := 0]
# latency to go out
# #careful: on vaccination days garten opened later! take out vacc days
# vacc =  c(ymd("2019-11-08"), ymd("2019-12-24"), ymd("2020-01-14"),
#           ymd("2020-02-18"), ymd("2020-03-03"), ymd("2020-04-14"), 
#           ymd("2020-06-09"), ymd("2020-06-23"))
# 
dailyGarten[Out == 1, EntryGarten := trackingData.1[Zone == "Wintergarten", clockTime[1], by = .(Hen, Date)][order(Date,Hen), V1]]
#dailyGarten[Out == 1, LatencyGarten := EntryGarten - ymd_hms(paste(dmy(Date), "10:00:00"))]

###### vertical travel distance ############### 
# remove weird incomplete days such as 24.12.2022
trackingData.1 <- subset(trackingData.1,Date !="24.12.2022")
data <- trackingData.1 %>% 
  mutate(hentime = paste(Hen, clockTime, sep = "_"))
setkeyv(data, "hentime")
sdata <- data %>% 
  mutate(henzonecompact = rleid(Zone))
unique_data <- distinct(sdata, henzonecompact, .keep_all = TRUE)
head(unique_data)


# number of vertically crossed zones during light hours, divided by the seconds of the animals spent inside
#create vector where wintergarden doesn't exist (replaced by litter)
unique_data[, distZone := Zone]
unique_data[, distZone := as.character(distZone)]

unique_data[Zone == "Wintergarten", distZone := "Litter"]

#add zone vector shifted by one
unique_data[, nextZone := as.character(shift(distZone, type = "lead")), by = .(Date, Hen)]
# replace NA by noZone
#trackingData.1[is.na(nextZone), nextZone := "noZone"]

#calculate distance travelled by using function defineDistance
unique_data[, distVertical := apply(X = cbind(distZone, nextZone), MARGIN = 1, FUN= defineDistance,date=Date,hen=Hen), by = .(Date,Hen)]
head(unique_data)

###### All movement behaviours together #######
#create data.table containing all daily measures
#vertical travel distance
# Calculate the sum of 'distVertical' for each unique combination of 'Hen' and 'Date'
varOfInterest <- unique_data[, .(vertTravelDist_tot = sum(distVertical)), by = .(Hen, Date,Pen)]

#add duration for each day per zone
# Calculate the sum of 'Duration' for each unique combination of 'Hen', 'Date', and 'Zone'
varOfInterest_zone <- trackingData.1[, .(Duration_zone = sum(Duration)), by = .(Hen, Date, Zone)]

# Cast the varOfInterest_zone data.table to wide format using dcast
varOfInterest_zone_wide <- dcast(varOfInterest_zone, Hen + Date ~ Zone, value.var = "Duration_zone", fill = 0)

# Merge the two summaries using a left join on 'Hen' and 'Date'
varOfInterest <- merge(varOfInterest, varOfInterest_zone_wide, by = c("Hen", "Date"), all.x = TRUE)

# Print the merged varOfInterest data.table
head(varOfInterest)

#varOfInterest = varOfInterest[dcast(trackingData.1[sum(Duration), by = .(Hen, Date, Zone)], formula = Hen + Date ~ Zone, value.var = "Duration", fill = 0), on = c("Hen", "Date")]
varOfInterest[, TotalDur := rowSums(cbind(Wintergarten, Tier_2, Tier_4, Litter, Ramp_Nestbox))]
#extract max zone per day
dailyMaxZone <- trackingData.1[, .(MaxZone = Zone[which.max(sum(Duration))]), by = .(Hen, Date)]
# Merge the dailyMaxZone information into varOfInterest
varOfInterest <- merge(varOfInterest, dailyMaxZone, by = c("Hen", "Date"), all.x = TRUE)

#careful with join direction not to loose the hens, for hens who don't go out for example
# -> full outer join
# wintergarden: did hen go out? duration in garten, latency to enter 
varOfInterest = dailyGarten[varOfInterest, on = c(Hen = "Hen", Date = "Date")]
# nestbox: did hen go in nest?, total duration in nest, time point of median duration in nest 
varOfInterest = dailyNest[varOfInterest, on = c(Hen = "Hen", Date = "Date")]

#make factor out of HenID & Pen with levels sorted in ascending Ratio order
varOfInterest[,Hen := factor(Hen)]
varOfInterest[,Pen := factor(Pen)]
#varOfInterest[,ZoneSleep := factor(ZoneSleep, levels = c("Litter", "Tier_2", "Ramp_Nestbox", "Tier_4"))]
varOfInterest[,MaxZone := factor(MaxZone, levels = c("Wintergarten", "Litter", "Tier_2", "Ramp_Nestbox", "Tier_4"))]
head(varOfInterest)
rm(trackingData.1)

write_fst(varOfInterest, 'varOfInterest_P14.fst')

IDs <- read_excel("/storage/research/vetsuisse_chicken_run/FATCAT/RFID/newTest/BirdID_Pen_ID.xlsx")

head(IDs)
names(IDs)[2] <- "Pen_Bird"

toepeckID <- read_excel("finalHA_Toepecking.xlsx")
head(toepeckID)
names(toepeckID)[1] <- "Pen_Bird_found"
str(IDs)
str(toepeckID)

head(IDs)
dim(IDs)
data.IDs <- merge(toepeckID,IDs,by.x="Hen_ID",by.y ="Serial_N",all.x=T)
head(data.IDs)
varOfInterest.1 <- merge(data.IDs,varOfInterest,by.x="Hen_ID",by.y ="Hen",all.y=T)
str(varOfInterest.1)

write_fst(varOfInterest.1, 'varOfInterest.1_P14.fst')
