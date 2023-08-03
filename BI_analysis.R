### working with Bioacoustic Index

##11/30/2020 - DCM

######
# CLEAR WORKSPACE
######

rm(list=ls())

library(ggplot2)
library(plyr)
library(dplyr)

DANIELLE = FALSE
KEVIN = TRUE

if(DANIELLE) setwd("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies/BI")

if(KEVIN){
  homedir <- "C:/Users/Kevin/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics"
  datadir <- paste0(homedir,"/Indicies/BI")
  setwd(datadir)
}

file_list <- list.files(".")

file_list <- file_list[grepl(".csv",file_list)]   # KTS added

# read in the csv files, and append their filename as a new column called "proj"
read_csv_filename <- function(file_list){
  ret <- read.csv(file_list)
  ret$site <- file_list 
  ret
}

# combine all the files in "file_list" into 1 csv, and apply the new project column
BI <- ldply(file_list, read_csv_filename)

# check that the number of projects matches what was read in
file_list
unique(BI$site) #great
#the 2017 files are an hour long so they are not included for the bootstrapping.

# write.csv(BI, "BI-Full.csv")
#After I added the site names to all of the files in the first set, I found that I had not included 2020 spring sheldon
#or 2020 spring ely, though I did run BI and NDSI on all of those

#will merge together after

if(DANIELLE) setwd("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies/BI/2020")

if(KEVIN){
  datadir <- paste0(homedir,"/Indicies/BI/2020")
  setwd(datadir)
}

file_list <- list.files(".")
file_list <- file_list[grepl(".csv",file_list)]

# read in the csv files, and append their filename as a new column called "proj"
read_csv_filename <- function(file_list){
  ret <- read.csv(file_list)
  ret$site <- file_list #EDIT
  ret
}

# combine all the files in "file_list" into 1 csv, and apply the new project column
BI2020 <- ldply(file_list, read_csv_filename)
# write.csv(BI2020, "BI2020.csv")

# setwd("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies")
if(DANIELLE) setwd("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies")
if(KEVIN){
  dir <- paste0(homedir,"/Indicies")
  setwd(dir)
}

if(DANIELLE) BI.Full <- read.csv("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies/BI-Full.csv")
if(KEVIN) BI.Full <- read.csv("BI-Full.csv")
# BI.Full <- read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Indicies/BI-Full.csv")
sitenames <- BI.Full$sitenames
sitenames<-unique(sitenames)
siteaverage<- as.data.frame(sitenames)

siteaverage$mean <- 'x'
nsites <-  length(sitenames)

file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(BI.Full,sitenames==thisfile)
  p<- rbind(sub$LEFT_CHANNEL, sub$RIGHT_CHANNEL)
  m <- mean(p)
  siteaverage$mean[siteaverage$sitenames == thisfile]<- m
}

siteaverage$max <- 'x'

file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(BI.Full,sitenames==thisfile)
  p<- rbind(sub$LEFT_CHANNEL, sub$RIGHT_CHANNEL)
  m <- max(p)
  siteaverage$max[siteaverage$sitenames == thisfile]<- m
}

if(DANIELLE) full<- read.csv("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/comparison_fh.csv")
if(KEVIN) full<- read.csv(paste0(homedir,"/comparison_fh.csv"))
# full <-read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/comparison_fh.csv")

full<- full[c("sitenames","Obsfull")]
full<- merge(siteaverage, full, by=c("sitenames"), all.x = T)

full$ninety<- 'x'
file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(BI.Full,sitenames==thisfile)
  p<- rbind(sub$LEFT_CHANNEL, sub$RIGHT_CHANNEL)
  m <- quantile(p, 0.9)
  full$ninety[full$sitenames == thisfile]<- m
}

plot(full$Obsfull, full$ninety) #terrible

n1<- lm(full$ninety ~ full$Obsfull)
summary(n1) # p-value: 0.7437, Multiple R-squared:  0.001656


full$seventyfive<- 'x'
file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(BI.Full,sitenames==thisfile)
  p<- rbind(sub$LEFT_CHANNEL, sub$RIGHT_CHANNEL)
  m <- quantile(p, 0.75)
  full$seventyfive[full$sitenames == thisfile]<- m
}

plot(full$Obsfull, full$seventyfive) 

n2<- lm(full$seventyfive ~ full$Obsfull)
summary(n2) # 0.8524, Multiple R-squared:  0.0005368

full$eighty<- 'x'
file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(BI.Full,sitenames==thisfile)
  p<- rbind(sub$LEFT_CHANNEL, sub$RIGHT_CHANNEL)
  m <- quantile(p, 0.8)
  full$eighty[full$sitenames == thisfile]<- m
}

plot(full$Obsfull, full$eighty) #they are all terrible

n3<- lm(full$eighty ~ full$Obsfull)
summary(n3) #p-value: 0.5627; Multiple R-squared:  0.005182

plot(full$Obsfull, full$max)
abline(a=0, b=1)

n4<- lm(full$max ~ full$Obsfull)
summary(n4) #p-value: 0.3044; Multiple R-squared:  0.01622

n5<- lm(full$mean ~ full$Obsfull)
summary(n5) #p-value: 0.9167; Multiple R-squared:  0.0001697

if(DANIELLE) rich <- read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Richness2020.csv")
if(KEVIN) rich <- read.csv(paste0(homedir,"/Richness2020.csv")) 

rich$sitenames<- paste(rich$Year, rich$Region, rich$Season, rich$Tripod)

full$richmax<- 'x'
full$richmean<- 'x'
full$rich90<- 'x'

file=1

for(file in 1:nsites){
  thisfile <- sitenames[file]
  sub <- subset(rich,sitenames==thisfile)
  m <- mean(sub$SpeciesActive)
  m2 <- max(sub$SpeciesActive)
  q9 <- quantile(sub$SpeciesActive, 0.9, na.rm =T)
  full$richmean[full$sitenames == thisfile]<- m
  full$richmax[full$sitenames == thisfile]<- m2
  full$rich90[full$sitenames == thisfile]<- q9
}

plot(full$Obsfull, full$richmean)
plot(full$Obsfull, full$richmax)
abline(a=0, b=1)


#cairo_pdf("MeanMax.pdf",width=6,height=6)
par(mfrow=c(2,2))
plot(full$mean, full$Obsfull, main= "Mean of Bioacoustic Index", ylab = "True Site Richness",
     xlab= "Mean of Bioacoustic Index Across Site Visit")
abline(a=0, b=1)
plot(full$max, full$Obsfull, main = "Maximum Bioacoustic Index", ylab = "True Site Richness",
     xlab= "Max Bioacoustic Index Across Site Visit")
abline(a=0, b=1)
plot(full$richmean, full$Obsfull, main= "Mean of True File Richness", ylab = "True Site Richness",
     xlab= "Mean File Level Richness Across Site Visit", xlim=c(1, 16))
abline(a=0, b=1)
plot(full$richmax, full$Obsfull, main= "Maximum True File Richness", ylab = "True Site Richness",
     xlab= "Max File Level Richness Across Site Visit", xlim=c(1, 16))
abline(a=0, b=1)

#dev.off()

## okay let's prep the data for the figures by time of day and by site

bi.len<-nrow(BI.Full)
BI.Full$time <- "x"


for (i in c(1:bi.len)) {                      
  BI.Full$time[i] <- unlist(strsplit(as.character(BI.Full$FILENAME[i]),split='_'))[3]} 

BI.Full$time<- as.character(BI.Full$time)
BI.Full$time<- substr(BI.Full$time, 1,2)
BI.Full$time<- as.integer(BI.Full$time)

BI.Full$sitenames <- as.factor(BI.Full$sitenames)

ggplot(data=BI.Full, aes(x=time, y=mean, group=sitenames)) +
  geom_line()


#that is too many to look at, let's isolate some high and low species richness sites
# high examples: 2018 Sheldon Spring B, 2020 Sheldon Spring B, 2019 Ely Spring A
# low 2018 Austin Spring C, 2018 Elko Fall F, 2020 Sheldon Spring D

bisub<- subset(BI.Full, sitenames==("2018 Sheldon Spring B"))
bisub1<- subset(BI.Full, sitenames==("2020 Sheldon Spring B"))
bisub2<- subset(BI.Full, sitenames==("2019 Ely Spring A"))
bisub3<- subset(BI.Full, sitenames==("2019 Sheldon Summer D"))
bisub4<- subset(BI.Full, sitenames==("2020 Ely Spring D"))
bisub5<- subset(BI.Full, sitenames==("2020 Sheldon Spring D"))

bis<- rbind(bisub, bisub1, bisub2, bisub3, bisub4, bisub5)
newdat <- bis %>% group_by(time,sitenames) %>% summarize(mean=mean(mean))   # KTS: I love that I could write the line "mean=mean(mean)" :)

ggplot(data=newdat, aes(x=time, y=mean, group=sitenames)) +
  geom_line(aes(color=sitenames), size = 3)
