?split
library(officer)
library(camtrapR)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
setwd("C:/Users/eliwi/OneDrive/Documents/UNR/")


#bring in camera data transcribed from word doc
Cam <- read.csv("C:/Users/eliwi/OneDrive/Documents/UNR/CamData2022.csv", na.strings = c("", "N/A"))
str(Cam)
Cam$First.Photo.Date <- as.Date(Cam$First.Photo.Date, format = "%m/%d/%y")
Cam$What.is.date.of.last.photo. <- as.Date(Cam$What.is.date.of.last.photo., format = "%m/%d/%y")
#make rows for detection dates of each species
Cam2 <- Cam%>%pivot_longer(cols = 14:38,
                   names_to = "species",
                   values_to = "Detection.Dates")
#remove dates where nothing was detected
Cam3 <- Cam2[!is.na(Cam2$Detection.Dates),]
str(Cam3)
#separate detection dates into individual rows for each detection
Cam4 <- separate_rows(Cam3, Detection.Dates, sep=",")

UnIDMam <- Cam4[Cam4$species == "unID.Mammal",]
table(Cam4$species)

#create key for folder to match filepath/date times to Cam4 dataframe*, not sure if unID animals were put into unknown folder or corresponding taxa folder
mammals <- c("Badger", "Beldings.Ground.Squirrel", "Black.Bear", "Bobcat", "Cows",
             'Coyote' , "Deer.Mouse" , "Dog" , "Kangaroo.Rat" , "Least.Chipmunk" ,
               "Marmot" , 'Mule.Deer' , 'Pronghorn' , "Tamias.spp.", "unID.Mammal", "unID.Rodent")
birds <- c("American Robin", "unID.Bird")

Cam4 <- Cam4%>% mutate(Taxa=case_when(species %in% mammals  ~ "Mammals",
                       species %in% birds ~ "Birds",
                       species == "unID.Herp" ~ "Herps"
                       ))
#clean up date column for key
Cam4$Date <- as.Date(Cam4$Detection.Dates, format= "%m/%d/%y")
table(is.na(Cam4$Date))

#CI Station Info similar to data(camtraps), input for summaries in CamTrapR
#get camera station info for trap effort
StationInfo <- Cam[Cam$First.Photo.Date > "2022-01-01" & Cam$What.is.date.of.last.photo. > "2022-01-01",] %>% summarise(min=min(First.Photo.Date),max=max(What.is.date.of.last.photo.))
StationInfo2 <- Cam[Cam$First.Photo.Date > "2022-01-01" & Cam$What.is.date.of.last.photo. > "2022-01-01",] %>% group_by(CamID) %>% summarise(min=min(First.Photo.Date),max=max(What.is.date.of.last.photo.))
StationInfoSite <- Cam[Cam$First.Photo.Date > "2022-01-01" & Cam$What.is.date.of.last.photo. > "2022-01-01",] %>% group_by(Spring) %>% summarise(min=min(First.Photo.Date),max=max(What.is.date.of.last.photo.))
StationInfo3 <- distinct(Cam4[,c(1,2,4,6)], CamID, .keep_all = T)
StationInfo4 <- left_join(StationInfo2, StationInfo3, by='CamID')  

camOps <- cameraOperation(CTtable = StationInfo4,
                          stationCol = 'CamID',
                          setupCol = 'min',
                          retrievalCol = 'max'
) # can include argument hasProblems=T for periods where camera was out of order
colnames(camOps)

#camOps effort by Spring
Springs <- unique(Cam4$Spring)
CamOpList <- list()
for(i in 1:length(Springs)){
camOp <- cameraOperation(CTtable = StationInfo4[StationInfo4$Spring == Springs[i],],
                          stationCol = 'CamID',
                          setupCol = 'min',
                          retrievalCol = 'max'
)
CamOpList[[Springs[i]]] <- camOp
}



#make detection history by Date  
species <- unique(Cam4$species)
dH <- data.frame(Date=seq.Date(from=StationInfo$min, to=StationInfo$max, by=1))
#watch out for wrong dates (e.g. 01-01-2018)
cameras <- unique(Cam4$CamID)

#we can modify this to get specific springs to test species curves by number of days
for (i in 1:length(species)){
  CamSub <- Cam4[Cam4$species == species[i],]
  x <- sapply(dH$Date, function (x) ifelse(x %in% CamSub$Date, length(CamSub$Date[CamSub$Date == x]),0))
  y <- sapply(dH$Date, function (Date) ifelse(Date %in% CamSub$Date, paste(paste(CamSub[CamSub$Date == Date,]$Spring, CamSub[CamSub$Date == Date,]$Line), collapse=", "),NA))
  dH[ ,ncol(dH)+1] <- x                # Append new column
  colnames(dH)[ncol(dH)] <- species[i] # Rename column name
  dH[ ,ncol(dH)+1] <- y
  colnames(dH)[ncol(dH)] <- colnames(dH)[ncol(dH)] <- paste(species[i], "SpringLine")
}

# setup and retrieval days default to 0.5 day effort from cameraOperation function
dH$TrapEffort <- sapply(dH$Date, function (x) sum(camOps[,as.character(x)], na.rm = T))
write.csv(dH, "./CamData22DetectionHistory.csv")
dH <- read.csv("./CamData22DetectionHistory.csv")


#Detection Histories by Site
Sites <- unique(Cam4$Spring)
species <- unique(Cam4$species)
dHList <- list()
for (s in 1:length(Sites)){
  Cam5 <- Cam4[Cam4$Spring == Sites[s],]
  dH <- data.frame(Date=seq.Date(from=StationInfo$min, to=StationInfo$max, by=1))

  for (i in 1:length (species)){
    CamSub <- Cam5[Cam5$species == species[i],]
    x <- sapply(dH$Date, function (x) ifelse(x %in% CamSub$Date, length(CamSub$Date[CamSub$Date == x]),0))
    y <- sapply(dH$Date, function (Date) ifelse(Date %in% CamSub$Date, paste(paste(CamSub[CamSub$Date == Date,]$Spring, CamSub[CamSub$Date == Date,]$Line), collapse=", "),NA))
    dH[ ,ncol(dH)+1] <- x                # Append new column
    colnames(dH)[ncol(dH)] <- species[i] # Rename column name
    dH[ ,ncol(dH)+1] <- y
    colnames(dH)[ncol(dH)] <- colnames(dH)[ncol(dH)] <- paste(species[i], "SpringLine")
    }
  dHList[[Sites[s]]] <- dH
  
  }

lapply(dHList, function (x) table(x$Cows))








DVS <- Cam4[Cam4$Spring == "DVS",]
DVSte <- as.data.frame(colSums(CamOpList[["DVS"]], na.rm = T))
colnames(DVSte)[1] <- "DailyEffort"
DVSte$Date <- as.Date(rownames(DVSte), format = "%Y-%m-%d")
DVSte$CumSum <- cumsum(DVSte$DailyEffort)
DVS <- left_join(DVS, DVSte, by="Date")
table(is.na(DVS$CumSum))

DVS <- DVS[order(DVS$Date),]
DVS <- DVS%>%ungroup()%>%mutate(Unq = cumsum(!duplicated(species)))
DVSUnq <- DVS%>%group_by(Date)%>%slice_max(Unq)%>%ungroup()
DVSUnq2 <- DVSUnq%>%distinct(Unq, .keep_all = T)


ggplot(DVSUnq, aes(x=CumSum, y=Unq)) + geom_smooth()#geom_line() + geom_point()



#Species Richness by Spring, not sure how to handle unknowns
Richness <- Cam4%>%group_by(Spring)%>%summarise(Richness=length(unique(species)), 
                                                Species=paste(unique(species), collapse=", "), 
                                                Mammals= length(unique(species[Taxa == "Mammals"])),
                                                Birds= length(unique(species[Taxa == "Birds"])),
                                                Herps= length(unique(species[Taxa == "Herps"])))


#Species Richness by Habitat Type (make Line data reflect Habitat Spring vs. Upland)
Richness <- Cam4%>%group_by(Line)%>%summarise(Richness=length(unique(species)), 
                                                Species=paste(unique(species), collapse=", "), 
                                                Mammals= length(unique(species[Taxa == "Mammals"])),
                                                Birds= length(unique(species[Taxa == "Birds"])),
                                                Herps= length(unique(species[Taxa == "Herps"])))

SpbySpring <- Cam4%>%select(-14)%>%pivot_wider(names_from = species,values_from = species, values_fn = length)
SpCol <- colnames(SpbySpring)[17:35]
SpbySpring[,17:35][is.na(SpbySpring[,17:35])] <- 0
SpbySpring2 <- SpbySpring %>% group_by(Spring) %>% summarise(across(SpCol, sum), .groups = "drop")

#GPS waypoints
GPS1 <- read.csv("./WayptsOnlyGPS1.csv")
GPS2 <- read.csv("./WayptsOnlyGPS2.csv")
GPS <- rbind(GPS1, GPS2)
table(GPS$name)
write.csv(GPS, "./GPSWayPts.csv")

 ggplot(dat2, aes(dat2[,Xcol], dat2[,Ycol])) +
   geom_point(cex =  cex_pt1_gg , fill = "white", colour = "black", pch = 21) +
   geom_point(aes(size = factor(t4$n_species)), colour = "black") +
   theme_bw() +
   theme(legend.position = "right",
         legend.box = "vertical",
         legend.key = element_blank()) +
   labs(x = Xcol,
        y = Ycol,
        title = "Species Richness")  +
   coord_fixed(ratio = 1) +
   scale_size_discrete(name = "n species")




#############################work in progress below this##################################
#come up with record table like data(recordTableSample) for input for summaries in CamTrapR  
#bring in dataframe with datetime metadata pulled from images
DateTimes <- readRDS("./CaptureDates.rds")
DateTimes$Taxa <- basename(dirname(DateTimes$File))
colnames(DateTimes)[2] <- "DateTime"
DateTimes$DateTime <- as.POSIXct(DateTimes$DateTime, format= "%Y:%m:%d %H:%M:%S", tz = "")
table(DateTimes$Taxa)
DateTimes$DetectionDate <- as.Date(DateTimes$DateTime)
DateTimes$FolderName <- str_extract(DateTimes$File, paste(FolderNames, collapse = "|"))
DateTimes$Subfolder <- str_extract(DateTimes$File, paste(SubFolderNames, collapse = "|"))
table(DateTimes$FolderName)
colnames(DateTimes)[4] <- "Date"
DateTimes[2272,]
str(DateTimes)


df <- left_join(Cam4, DateTimes, by=c("FolderName", "Taxa", "Date"), relationship="one-to-many")
df2 <- merge(Cam4, DateTimes, by=c("FolderName", "Taxa", "Date"))



minDeltaTime <- 0
intable <- data.frame(df,
                      delta.time.secs  = NA,
                      delta.time.mins  = NA,
                      delta.time.hours = NA,
                      delta.time.days  = NA,
                      independent      = ifelse(minDeltaTime == 0, TRUE, NA),   # all independent if no temporal filtering
                      stringsAsFactors = FALSE,
                      check.names      = FALSE)        # to prevent ":" being converted to ".", e.g. in EXIF:Mak

for (i in 1:length(species)) {
  sp <- species[i]
  dH <- detectionHistory(recordTable = df,
                         species = species[i],
                         camOp = camOps,
                         output = 'binary',
                         stationCol = "CamID",
                         speciesCol = "species",
                         recordDateTimeCol = "Date",
                         recordDateTimeFormat = "%Y-%m-%d",
                         occasionLength = 1,
                         day1= "station",
                         timeZone = "US/Pacific")
  
                         
                         
                        
                         
                        
    
  )
}


# sort records by station, species, then time
outtable <- camtrapR:::assessTemporalIndependence(intable = intable, deltaTimeComparedTo = "lastRecord",columnOfInterest = "Species",stationCol = "Station",minDeltaTime = 60,camerasIndependent = FALSE)


#scrap
c <- DateTimes[4799,]
b <- Cam4[34,]

a <- Cam4[Cam4$FolderName == "BBSCAM4_P11",]

Proof <- Cam[c(45, 6),]
d <- Cam4[duplicated(Cam4[,c(3,14,15)]),]
e <- DateTimes[duplicated(DateTimes[,c(3:5)]),]
f <- df[duplicated(df[,c(3,14,15)]),]
nas <- df[is.na(df$DateTime),]

g <- Cam4[Cam4$Spring == "BBS/UC",]
