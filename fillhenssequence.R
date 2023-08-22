data <- splitHen_new[[3]]
fillSeqHenData3 <- function (data, start = "00:00:00", end = "23:59:59", 
          interval = "sec") 
{
  if (nrow(data)) {
    days <- as.character(unique(data[, Date]))
    splitData <- list()
    previous.last.Zone <- NA
    days.cont <- diff(as.Date(days, format = "%Y.%m.%d"))
    days.cont <- c(days.cont, 1)
    i <- 1
    for (i in 1:length(days)) {
      day <- days[i]
      seq = data.table(clockTime = seq(ymd_hms(paste(day, start)), 
                                  ymd_hms(paste(day, end)), by = interval)) # changed from Time to clockTime
      relDat = data[Date == day, ]
      totlines <- dim(relDat)[1]
      missmatch <- length(which(relDat$Pen != relDat$Pen_Bird))
      missmatchProp <- missmatch/totlines
      #if (missmatchProp > 0) {
      #  nodatwarning <- warning(paste("Chicken ", 
      #                                unique(relDat$HenID), " had ", round(missmatchProp, 
      #                                                                     3) * 100, "% (", missmatch, ") wrong pen assignments on day ", 
      #                                day, sep = ""))
      #  print(nodatwarning)
      #  warnings <<- append(warnings, list(nodatwarning))
      #  penassignments <<- rbind(penassignments, data.frame(Hen = unique(relDat$Hen)[!is.na(unique(relDat$Hen))], 
      #                                                      Day = day, NrWrong = missmatch, PropWrong = round(missmatchProp, 
      #                                                                                                        3) * 100))
      #}
      #str(seq)
      relDat = relDat[seq, on = ("clockTime"="clockTime")]
      relDat = na.locf(relDat, na.rm = FALSE)
      #relDat$clockTime <- as.hms(format(relDat$Time, format = "%H:%M:%S"))
      relDat$Date <- day
      #str(relDat)
      #head(relDat)
      relDat$Hen <- unique(relDat$Hen)[!is.na(unique(relDat$Hen))]
      relDat$Pen_Bird <- unique(relDat$Pen_Bird)[!is.na(unique(relDat$Pen_Bird))]
      relDat$HenID <- unique(relDat$HenID)[!is.na(unique(relDat$HenID))]
      relDat$TRT <- unique(relDat$TRT)[!is.na(unique(relDat$TRT))]
      relDat$Feet_sum <- unique(relDat$Feet_sum)[!is.na(unique(relDat$Feet_sum))]
      nazone.nas <- sum(is.na(relDat$Zone))
      nazone.startnas <- sum(is.na(relDat$Zone[1:nazone.nas]))
      nazone.check <- nazone.nas == nazone.startnas
      if (!nazone.check) {
        zonewarning <- warning(paste("Chicken ", 
                                     unique(relDat$HenID), " had more NAs than leading lines on day ", 
                                     day, sep = ""))
        print(zonewarning)
        warnings <- append(warnings, list(zonewarning))
      }
      if (nazone.check) {
        relDat$Zone <- as.character(relDat$Zone)
        relDat$Zone[1:nazone.startnas] <- previous.last.Zone
        relDat$Zone <- as.factor(relDat$Zone)
      }
      if (days.cont[i] == 1) {
        previous.last.Zone <- as.character(relDat$Zone[length(relDat$Zone)])
      }
      else {
        previous.last.Zone <- NA
      }
      splitData <- append(splitData, list(relDat))
    }
  }
  else {
    splitData <- list(data)
  }
  return(splitData)
}

