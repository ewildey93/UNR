#### D.MILES 11/25/2020

#there were some errors in Richard's species names so the number of species was not correct
#some with typos were previously double counted. Here I am running the best monitoring periods again
#looking to compare all of those >80% of total species seen
#the unids were squished into one column and restricted to a maximum of 1 unid bird per file


## STARTING AT LINE 2127 - ## Bootstrapping to determine if there are detectable differences between seasons and sites 

######
# CLEAR WORKSPACE
######

rm(list=ls())

#setwd("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics")
setwd("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics")
library(vegan)
library(dplyr)
library(ggplot2)
library(reshape2)
#install.packages("tidyr")
library(tidyr)
require(utils)

#take note this is the modified file of richard's identified birds
fileinfo<- read.csv("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/birdspecies_fixed.csv")
#fileinfo <- read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/birdspecies_fixed.csv")
fileinfo$site<- paste(fileinfo$Year, fileinfo$Region, fileinfo$Season, fileinfo$Tripod)
fileinfo$site<- as.factor(fileinfo$site)
unique(fileinfo$site)
fileinfo$site<-as.character(fileinfo$site)
fileinfo$site[fileinfo$site == "2018 Austin Spring I "] <- "2018 Austin Spring I" 
fileinfo$site<- as.factor(fileinfo$site)

sitenames<- unique(fileinfo$site)
nsitenames<- length(unique(fileinfo$site))

filepool<- fileinfo
file = 1
poolsites<- as.data.frame(sitenames)
poolsites$Obsfull <- 'x'
poolsites$bootfull <- 'x'
poolsites$bootfullse <- 'x'

for(file in 1:nsitenames){
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile)
  sub1<- subset(sub, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  #estimates extrapolated species richness
  p1<-specpool(sub1, smallsample = TRUE)
  poolsites$Obsfull[poolsites$sitenames == thisfile]<- p1$Species
  poolsites$bootfull[poolsites$sitenames == thisfile]<- p1$boot
  poolsites$bootfullse[poolsites$sitenames == thisfile]<- p1$boot.se
}

#now let's do by times
filepool$FileStartTime<- as.character(filepool$FileStartTime)
filepool$time<- 'x'
sp.len<-nrow(filepool)
for (i in c(1:sp.len)) {                        # sp.len=The length of data
  filepool$time[i] <- unlist(strsplit(as.character(filepool$FileStartTime[i]),split=':'))[1]} # [1] references time before :

summary(filepool$time)
filepool$time<- as.numeric(filepool$time)

filepool$DATE <- lubridate::dmy(paste0(as.character(filepool$FileDate) ,"-", filepool$Year))
filepool$JDAY <- lubridate::yday(filepool$DATE)

filepool$JDAY <- as.factor(filepool$JDAY)

#now rerun the top subsets with the fixed data
#bootstrapping by random accumulation

#we will start with the general metric of how species richness declines with less files
#the maximum we have is 7 jdays, but this is 6 nights 
#so usually only 6.5 days of recording from setting up late on day one and taking down early day 2 - 132 files

compar_fh<- poolsites #fh will be full hours

fh1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh1) <- sitenames

this.days <- 7           # define number of sampling days
this.files <- 132            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh1[rownames(fh1) == thisfile,b]<- p1$boot
      
      
    }else{
      fh1[rownames(fh1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh1<- rowMeans(fh1)
compar_fh$fh1.se <- apply(fh1, 1, function(x) sd(x) / sqrt(length(x)))


fh2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh2) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 120            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh2[rownames(fh2) == thisfile,b]<- p1$boot
      
      
    }else{
      fh2[rownames(fh2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh2<- rowMeans(fh2)
compar_fh$fh2.se <- apply(fh2, 1, function(x) sd(x) / sqrt(length(x)))

fh3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh3) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 96            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh3[rownames(fh3) == thisfile,b]<- p1$boot
      
      
    }else{
      fh3[rownames(fh3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh3<- rowMeans(fh3)
compar_fh$fh3.se <- apply(fh3, 1, function(x) sd(x) / sqrt(length(x)))

fh4 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh4) <- sitenames

this.days <- 4           # define number of sampling days
this.files <- 72            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh4[rownames(fh4) == thisfile,b]<- p1$boot
      
      
    }else{
      fh4[rownames(fh4) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh4<- rowMeans(fh4)
compar_fh$fh4.se <- apply(fh4, 1, function(x) sd(x) / sqrt(length(x)))

fh5 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh5) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 48            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh5[rownames(fh5) == thisfile,b]<- p1$boot
      
      
    }else{
      fh5[rownames(fh5) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh5<- rowMeans(fh5)
compar_fh$fh5.se <- apply(fh5, 1, function(x) sd(x) / sqrt(length(x)))

fh6 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(fh6) <- sitenames

this.days <- 2           # define number of sampling days
this.files <- 24            # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) # define the hours of interest
#I do understand I don't need to subset if it is all of the hours, but I don't want to mess with the stuff in the loop below

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      fh6[rownames(fh6) == thisfile,b]<- p1$boot
      
      
    }else{
      fh6[rownames(fh6) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_fh$fh6<- rowMeans(fh6)
compar_fh$fh6.se <- apply(fh6, 1, function(x) sd(x) / sqrt(length(x)))

####################################################################################
# now for morning long - ml 
compar_ml<- poolsites

ml1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ml1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 48           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ml1[rownames(ml1) == thisfile,b]<- p1$boot
      
      
    }else{
      ml1[rownames(ml1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ml$ml1<- rowMeans(ml1)
compar_ml$ml1.se <- apply(ml1, 1, function(x) sd(x) / sqrt(length(x)))

ml2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ml2) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 40           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ml2[rownames(ml2) == thisfile,b]<- p1$boot
      
      
    }else{
      ml2[rownames(ml2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ml$ml2<- rowMeans(ml2)
compar_ml$ml2.se <- apply(ml2, 1, function(x) sd(x) / sqrt(length(x)))

ml3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ml3) <- sitenames

this.days <- 4           # define number of sampling days
this.files <- 32           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ml3[rownames(ml3) == thisfile,b]<- p1$boot
      
      
    }else{
      ml3[rownames(ml3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ml$ml3<- rowMeans(ml3)
compar_ml$ml3.se <- apply(ml3, 1, function(x) sd(x) / sqrt(length(x)))

ml4 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ml4) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 24           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ml4[rownames(ml4) == thisfile,b]<- p1$boot
      
      
    }else{
      ml4[rownames(ml4) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ml$ml4<- rowMeans(ml4)
compar_ml$ml4.se <- apply(ml4, 1, function(x) sd(x) / sqrt(length(x)))

####################################################
## now morning short 6-8
compar_ms<- poolsites

ms1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ms1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 18           # define number of files sampled each day
this.hours <- c(6,7,8) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ms1[rownames(ms1) == thisfile,b]<- p1$boot
      
      
    }else{
      ms1[rownames(ms1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ms$ms1<- rowMeans(ms1)
compar_ms$ms1.se <- apply(ms1, 1, function(x) sd(x) / sqrt(length(x)))


ms2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ms2) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 15           # define number of files sampled each day
this.hours <- c(6,7,8) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ms2[rownames(ms2) == thisfile,b]<- p1$boot
      
      
    }else{
      ms2[rownames(ms2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ms$ms2<- rowMeans(ms2)
compar_ms$ms2.se <- apply(ms2, 1, function(x) sd(x) / sqrt(length(x)))

ms3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ms3) <- sitenames

this.days <- 4           # define number of sampling days
this.files <- 12           # define number of files sampled each day
this.hours <- c(6,7,8) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ms3[rownames(ms3) == thisfile,b]<- p1$boot
      
      
    }else{
      ms3[rownames(ms3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_ms$ms3<- rowMeans(ms3)
compar_ms$ms3.se <- apply(ms3, 1, function(x) sd(x) / sqrt(length(x)))

# write.csv(compar_fh, "comparison_fh.csv")
# write.csv(compar_ml, "comparison_ml.csv")
# write.csv(compar_ms, "comparison_ms.csv")

compar_a<- poolsites

a1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(a1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 48           # define number of files sampled each day
this.hours <- c(7,8,9,10,11,12,13,14) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      a1[rownames(a1) == thisfile,b]<- p1$boot
      
      
    }else{
      a1[rownames(a1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_a$a1<- rowMeans(a1)
compar_a$a1.se <- apply(a1, 1, function(x) sd(x) / sqrt(length(x)))

a2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(a2) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 40           # define number of files sampled each day
this.hours <- c(7,8,9,10,11,12,13,14) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      a2[rownames(a2) == thisfile,b]<- p1$boot
      
      
    }else{
      a2[rownames(a2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_a$a2<- rowMeans(a2)
compar_a$a2.se <- apply(a2, 1, function(x) sd(x) / sqrt(length(x)))

compar_sevens<- poolsites

s1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(s1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 78           # define number of files sampled each day
this.hours <- c(7,8,9,10,11,12,13,14,15,16,17,18,19) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      s1[rownames(s1) == thisfile,b]<- p1$boot
      
      
    }else{
      s1[rownames(s1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_sevens$s1<- rowMeans(s1)
compar_sevens$s1.se <- apply(s1, 1, function(x) sd(x) / sqrt(length(x)))


s2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(s2) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 65           # define number of files sampled each day
this.hours <- c(7,8,9,10,11,12,13,14,15,16,17,18,19) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      s2[rownames(s2) == thisfile,b]<- p1$boot
      
      
    }else{
      s2[rownames(s2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_sevens$s2<- rowMeans(s2)
compar_sevens$s2.se <- apply(s2, 1, function(x) sd(x) / sqrt(length(x)))

compar_dusk<- poolsites
d1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(d1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 48           # define number of files sampled each day
this.hours <- c(5,6,7,8,17,18,19,20) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      d1[rownames(d1) == thisfile,b]<- p1$boot
      
      
    }else{
      d1[rownames(d1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$d1<- rowMeans(d1)
compar_dusk$d1.se <- apply(d1, 1, function(x) sd(x) / sqrt(length(x)))

d2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(d2) <- sitenames

this.days <- 5          # define number of sampling days
this.files <- 40           # define number of files sampled each day
this.hours <- c(5,6,7,8,17,18,19,20) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      d2[rownames(d2) == thisfile,b]<- p1$boot
      
      
    }else{
      d2[rownames(d2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$d2<- rowMeans(d2)
compar_dusk$d2.se <- apply(d2, 1, function(x) sd(x) / sqrt(length(x)))

d3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(d3) <- sitenames

this.days <- 4          # define number of sampling days
this.files <- 32           # define number of files sampled each day
this.hours <- c(5,6,7,8,17,18,19,20) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      d3[rownames(d3) == thisfile,b]<- p1$boot
      
      
    }else{
      d3[rownames(d3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}

compar_dusk$d3<- rowMeans(d3)
compar_dusk$d3.se <- apply(d3, 1, function(x) sd(x) / sqrt(length(x)))

d4 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(d4) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 24           # define number of files sampled each day
this.hours <- c(5,6,7,8,17,18,19,20) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      d4[rownames(d4) == thisfile,b]<- p1$boot
      
      
    }else{
      d4[rownames(d4) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$d4<- rowMeans(d4)
compar_dusk$d4.se <- apply(d4, 1, function(x) sd(x) / sqrt(length(x)))

#let's do long mornings and long dusk

dl1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(dl1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 66           # define number of files sampled each day
this.hours <- c(5,6,7,8,9,10,17,18,19,20,21) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      dl1[rownames(dl1) == thisfile,b]<- p1$boot
      
      
    }else{
      dl1[rownames(dl1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$dl1<- rowMeans(dl1)
compar_dusk$dl1.se <- apply(dl1, 1, function(x) sd(x) / sqrt(length(x)))

dl2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(dl2) <- sitenames

this.days <- 5          # define number of sampling days
this.files <- 55           # define number of files sampled each day
this.hours <- c(5,6,7,8,9,10,17,18,19,20,21) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      dl2[rownames(dl2) == thisfile,b]<- p1$boot
      
      
    }else{
      dl2[rownames(dl2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$dl2<- rowMeans(dl2)
compar_dusk$dl2.se <- apply(dl2, 1, function(x) sd(x) / sqrt(length(x)))

dl3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(dl3) <- sitenames

this.days <- 4          # define number of sampling days
this.files <- 44           # define number of files sampled each day
this.hours <- c(5,6,7,8,9,10,17,18,19,20,21) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      dl3[rownames(dl3) == thisfile,b]<- p1$boot
      
      
    }else{
      dl3[rownames(dl3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$dl3<- rowMeans(dl3)
compar_dusk$dl3.se <- apply(dl3, 1, function(x) sd(x) / sqrt(length(x)))

dl4 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(dl4) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 33           # define number of files sampled each day
this.hours <- c(5,6,7,8,9,10,17,18,19,20,21) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      dl4[rownames(dl4) == thisfile,b]<- p1$boot
      
      
    }else{
      dl4[rownames(dl4) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_dusk$dl4<- rowMeans(dl4)
compar_dusk$dl4.se <- apply(dl4, 1, function(x) sd(x) / sqrt(length(x)))

##adding one more dl for figure
dl5 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(dl5) <- sitenames

this.days <- 2           # define number of sampling days
this.files <- 22           # define number of files sampled each day
this.hours <- c(5,6,7,8,9,10,17,18,19,20,21) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      dl5[rownames(dl5) == thisfile,b]<- p1$boot
      
      
    }else{
      dl5[rownames(dl5) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
poolsites2<- poolsites
poolsites2$dl5<- rowMeans(dl5)
poolsites2$dl5.se <- apply(dl5, 1, function(x) sd(x) / sqrt(length(x)))

ml5 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(ml5) <- sitenames

this.days <- 2           # define number of sampling days
this.files <- 16           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      ml5[rownames(ml5) == thisfile,b]<- p1$boot
      
      
    }else{
      ml5[rownames(ml5) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
poolsites2$ml5<- rowMeans(ml5)
poolsites2$ml5.se <- apply(ml5, 1, function(x) sd(x) / sqrt(length(x)))

#write.csv(poolsites2, "dml5.csv")

# write.csv(compar_dusk, "comparison_dusk.csv")
# write.csv(compar_a, "comparison_a.csv")
# write.csv(compar_sevens, "comparison7.csv")

#now 4-9 and 21-9
compar_nines<- poolsites
n1 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n1) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 36           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n1[rownames(n1) == thisfile,b]<- p1$boot
      
      
    }else{
      n1[rownames(n1) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n1<- rowMeans(n1)
compar_nines$n1.se <- apply(n1, 1, function(x) sd(x) / sqrt(length(x)))


n2 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n2) <- sitenames

this.days <- 5           # define number of sampling days
this.files <- 30           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n2[rownames(n2) == thisfile,b]<- p1$boot
      
      
    }else{
      n2[rownames(n2) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n2<- rowMeans(n2)
compar_nines$n2.se <- apply(n2, 1, function(x) sd(x) / sqrt(length(x)))


n3 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n3) <- sitenames

this.days <- 4           # define number of sampling days
this.files <- 24           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n3[rownames(n3) == thisfile,b]<- p1$boot
      
      
    }else{
      n3[rownames(n3) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n3<- rowMeans(n3)
compar_nines$n3.se <- apply(n3, 1, function(x) sd(x) / sqrt(length(x)))

n4 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n4) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 18           # define number of files sampled each day
this.hours <- c(4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n4[rownames(n4) == thisfile,b]<- p1$boot
      
      
    }else{
      n4[rownames(n4) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n4<- rowMeans(n4)
compar_nines$n4.se <- apply(n4, 1, function(x) sd(x) / sqrt(length(x)))


n5 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n5) <- sitenames

this.days <- 6           # define number of sampling days
this.files <- 78           # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n5[rownames(n5) == thisfile,b]<- p1$boot
      
      
    }else{
      n5[rownames(n5) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n5<- rowMeans(n5)
compar_nines$n5.se <- apply(n5, 1, function(x) sd(x) / sqrt(length(x)))

n6 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n6) <- sitenames

this.days <- 5          # define number of sampling days
this.files <- 65           # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n6[rownames(n6) == thisfile,b]<- p1$boot
      
      
    }else{
      n6[rownames(n6) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n6<- rowMeans(n6)
compar_nines$n6.se <- apply(n6, 1, function(x) sd(x) / sqrt(length(x)))

n7 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n7) <- sitenames

this.days <- 4          # define number of sampling days
this.files <- 52           # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n7[rownames(n7) == thisfile,b]<- p1$boot
      
      
    }else{
      n7[rownames(n7) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n7<- rowMeans(n7)
compar_nines$n7.se <- apply(n7, 1, function(x) sd(x) / sqrt(length(x)))

n8 <- matrix(0,nrow=nsitenames,ncol=100)
rownames(n8) <- sitenames

this.days <- 3           # define number of sampling days
this.files <- 39           # define number of files sampled each day
this.hours <- c(21,22,23,0,1,2,3,4,5,6,7,8,9) # define the hours of interest

file =1
for(file in 1:nsitenames){
  
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  
  nBoot <- 100
  b=1
  for (b in 1:nBoot){
    
    sub1 <- subset(sub,time%in%this.hours)
    
    if(nrow(sub1)>0){
      
      alldays <- sort(unique(sub1$JDAY))
      
      if(length(alldays)>1){
        days <- sample(alldays,min(length(alldays),this.days),replace = T)
      }else{
        days <- rep(alldays,times=this.days)
      }
      
      sub2 <- subset(sub1,JDAY%in%days)
      
      allfiles <- sort(unique(sub2$file))
      files <- sample(allfiles,this.files,replace = T)
      sub3 <- subset(sub2,file%in%files)
      
      sub4<- subset(sub3, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec))
      
      
      p1<-specpool(sub4, smallsample = TRUE)
      n8[rownames(n8) == thisfile,b]<- p1$boot
      
      
    }else{
      n8[rownames(n8) == thisfile,b]<- NA
    }
    
    
    
  }
  
}
compar_nines$n8<- rowMeans(n8)
compar_nines$n8.se <- apply(n8, 1, function(x) sd(x) / sqrt(length(x)))

# write.csv(compar_nines, "comparison9.csv")

#okay I have compiled all of the best options that reduce file number but have on average greater than 80% of the species

top_raw <- read.csv("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/Top2_full.csv")
top_percent <- read.csv("C:/Users/dmiles/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/comparison_top2.csv")


upr <- qnorm(0.975,top_raw$bootfull,top_raw$bootfullse)
lwr <- qnorm(0.025,top_raw$bootfull,top_raw$bootfullse)

range<- data.frame("Subset"= c("fh1", "fh2", "fh3", "fh4", "dl1", "dl2", "fh5", "ml1", "dl3", "ml2", "n1", "ml3"))

range$lwr[range$Subset == "fh1"] <- mean(top_raw$Fh1/lwr,na.rm=T)
range$upr[range$Subset == "fh1"] <- mean(top_raw$Fh1/upr,na.rm=T)
range$average[range$Subset == "fh1"] <- mean(top_raw$Fh1/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "fh1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "fh1"]))
range$lowspl[range$Subset == "fh1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "fh1"]))
range$avgspl[range$Subset == "fh1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "fh1"]))

range$lwr[range$Subset == "fh2"] <- mean(top_raw$Fh2/lwr,na.rm=T)
range$upr[range$Subset == "fh2"] <- mean(top_raw$Fh2/upr,na.rm=T)
range$average[range$Subset == "fh2"] <- mean(top_raw$Fh2/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "fh2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "fh2"]))
range$lowspl[range$Subset == "fh2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "fh2"]))
range$avgspl[range$Subset == "fh2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "fh2"]))

range$lwr[range$Subset == "fh3"] <- mean(top_raw$Fh3/lwr,na.rm=T)
range$upr[range$Subset == "fh3"] <- mean(top_raw$Fh3/upr,na.rm=T)
range$average[range$Subset == "fh3"] <- mean(top_raw$Fh3/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "fh3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "fh3"]))
range$lowspl[range$Subset == "fh3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "fh3"]))
range$avgspl[range$Subset == "fh3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "fh3"]))

range$lwr[range$Subset == "fh4"] <- mean(top_raw$Fh4/lwr,na.rm=T)
range$upr[range$Subset == "fh4"] <- mean(top_raw$Fh4/upr,na.rm=T)
range$average[range$Subset == "fh4"] <- mean(top_raw$Fh4/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "fh4"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "fh4"]))
range$lowspl[range$Subset == "fh4"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "fh4"]))
range$avgspl[range$Subset == "fh4"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "fh4"]))

range$lwr[range$Subset == "fh5"] <- mean(top_raw$Fh5/lwr,na.rm=T)
range$upr[range$Subset == "fh5"] <- mean(top_raw$Fh5/upr,na.rm=T)
range$average[range$Subset == "fh5"] <- mean(top_raw$Fh5/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "fh5"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "fh5"]))
range$lowspl[range$Subset == "fh5"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "fh5"]))
range$avgspl[range$Subset == "fh5"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "fh5"]))

range$lwr[range$Subset == "dl1"] <- mean(top_raw$dl1/lwr,na.rm=T)
range$upr[range$Subset == "dl1"] <- mean(top_raw$dl1/upr,na.rm=T)
range$average[range$Subset == "dl1"] <- mean(top_raw$dl1/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "dl1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "dl1"]))
range$lowspl[range$Subset == "dl1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "dl1"]))
range$avgspl[range$Subset == "dl1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "dl1"]))

range$lwr[range$Subset == "dl2"] <- mean(top_raw$dl2/lwr,na.rm=T)
range$upr[range$Subset == "dl2"] <- mean(top_raw$dl2/upr,na.rm=T)
range$average[range$Subset == "dl2"] <- mean(top_raw$dl2/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "dl2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "dl2"]))
range$lowspl[range$Subset == "dl2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "dl2"]))
range$avgspl[range$Subset == "dl2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "dl2"]))

range$lwr[range$Subset == "dl3"] <- mean(top_raw$dl3/lwr,na.rm=T)
range$upr[range$Subset == "dl3"] <- mean(top_raw$dl3/upr,na.rm=T)
range$average[range$Subset == "dl3"] <- mean(top_raw$dl3/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "dl3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "dl3"]))
range$lowspl[range$Subset == "dl3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "dl3"]))
range$avgspl[range$Subset == "dl3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "dl3"]))

range$lwr[range$Subset == "ml1"] <- mean(top_raw$ml1/lwr,na.rm=T)
range$upr[range$Subset == "ml1"] <- mean(top_raw$ml1/upr,na.rm=T)
range$average[range$Subset == "ml1"] <- mean(top_raw$ml1/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "ml1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "ml1"]))
range$lowspl[range$Subset == "ml1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "ml1"]))
range$avgspl[range$Subset == "ml1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "ml1"]))

range$lwr[range$Subset == "ml2"] <- mean(top_raw$ml2/lwr,na.rm=T)
range$upr[range$Subset == "ml2"] <- mean(top_raw$ml2/upr,na.rm=T)
range$average[range$Subset == "ml2"] <- mean(top_raw$ml2/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "ml2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "ml2"]))
range$lowspl[range$Subset == "ml2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "ml2"]))
range$avgspl[range$Subset == "ml2"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "ml2"]))

range$lwr[range$Subset == "ml3"] <- mean(top_raw$ml3/lwr,na.rm=T)
range$upr[range$Subset == "ml3"] <- mean(top_raw$ml3/upr,na.rm=T)
range$average[range$Subset == "ml3"] <- mean(top_raw$ml3/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "ml3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "ml3"]))
range$lowspl[range$Subset == "ml3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "ml3"]))
range$avgspl[range$Subset == "ml3"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "ml3"]))

range$lwr[range$Subset == "n1"] <- mean(top_raw$n1/lwr,na.rm=T)
range$upr[range$Subset == "n1"] <- mean(top_raw$n1/upr,na.rm=T)
range$average[range$Subset == "n1"] <- mean(top_raw$n1/top_raw$bootfull,na.rm=T)

range$topspl[range$Subset == "n1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$upr[range$Subset == "n1"]))
range$lowspl[range$Subset == "n1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$lwr[range$Subset == "n1"]))
range$avgspl[range$Subset == "n1"] <- mean(top_raw$bootfull-(top_raw$bootfull*range$average[range$Subset == "n1"]))

# write.csv(range, "topranges.csv")

#### AUG 22 2021
## Bootstrapping to determine if there are detectable differences between seasons and sites - same start as above

fileinfo <- read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/birdspecies_fixed.csv")
fileinfo$site<- paste(fileinfo$Year, fileinfo$Region, fileinfo$Season, fileinfo$Tripod)
fileinfo$site<- as.factor(fileinfo$site)
unique(fileinfo$site)
fileinfo$site<-as.character(fileinfo$site)
fileinfo$site[fileinfo$site == "2018 Austin Spring I "] <- "2018 Austin Spring I" 
fileinfo$site<- as.factor(fileinfo$site)

sitenames<- unique(fileinfo$site)

filepool<- fileinfo
file = 1
poolsites<- as.data.frame(sitenames)

filepool$FileStartTime<- as.character(filepool$FileStartTime)
filepool$time<- 'x'
sp.len<-nrow(filepool)
for (i in c(1:sp.len)) {                        # sp.len=The length of data
  filepool$time[i] <- unlist(strsplit(as.character(filepool$FileStartTime[i]),split=':'))[1]} # [1] references time before :

summary(filepool$time)
filepool$time<- as.numeric(filepool$time)

filepool$DATE <- lubridate::dmy(paste0(as.character(filepool$FileDate) ,"-", filepool$Year))
filepool$JDAY <- lubridate::yday(filepool$DATE)

filepool$JDAY <- as.factor(filepool$JDAY)

##we will not do my chosen pairs, but select from all pairs in a stratified manner
pairs<- expand.grid(x = poolsites$sitenames, y =poolsites$sitenames)
Obs <- read.csv("C:/Users/hamme/Dropbox/GBLCC P-J Project/PAPERS/Avian Acoustics/comparison_ml.csv")
vari<- c("sitenames","Obsfull","bootfull","bootfullse")
Obs<- Obs[vari]
names(pairs)[names(pairs)=="x"]<-"sitenames"
PairsRich<- merge(pairs, Obs, by = c("sitenames"), all.x = TRUE)

names(PairsRich)[names(PairsRich)=="sitenames"]<-"SiteX"
names(PairsRich)[names(PairsRich)=="Obsfull"]<-"XObsfull"
names(PairsRich)[names(PairsRich)=="bootfull"]<-"Xbootfull"
names(PairsRich)[names(PairsRich)=="bootfullse"]<-"Xse"

names(PairsRich)[names(PairsRich)=="y"]<-"sitenames"
PairsRich<- merge(PairsRich, Obs, by = c("sitenames"), all.x = TRUE)
names(PairsRich)[names(PairsRich)=="sitenames"]<-"SiteY"
names(PairsRich)[names(PairsRich)=="Obsfull"]<-"YObsfull"
names(PairsRich)[names(PairsRich)=="bootfull"]<-"Ybootfull"
names(PairsRich)[names(PairsRich)=="bootfullse"]<-"Yse"

PairsRich$Rawdif <- PairsRich$XObsfull - PairsRich$YObsfull
PairsRich$Accudif<- PairsRich$Xbootfull - PairsRich$Ybootfull
hist(PairsRich$Rawdif)
hist(abs(PairsRich$Rawdif))
hist(abs(PairsRich$Accudif))
#okay we will sort by less 5, 5-15, greater than 15

LowDif<- PairsRich[ which(abs(PairsRich$Accudif) < 5),]
MedDif<- PairsRich[ which(abs(PairsRich$Accudif) > 5),]
MedDif<- MedDif[ which(abs(MedDif$Accudif) < 15),]
HighDif<- PairsRich[ which(abs(PairsRich$Accudif) > 15),]
#this.site <- c("2019 Sheldon Summer H", "2018 Ely Spring G", "2018 Sheldon Spring B", "2020 Austin Spring G")
#pairs are :2019 Sheldon Summer H and 2018 Sheldon Spring A
#2018 Ely Spring G and 2018 Ely Spring C
#2018 Sheldon Spring B and 2020 Sheldon Spring H
#2020 Austin Spring G and 2020 Austin Spring F

dif_low <- matrix(0,nrow=1001,ncol=5)
colnames(dif_low) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(dif_low)
dif_low<-as.data.frame(dif_low)
this.days <- 3           # define number of sampling days
this.files <- 72           # define number of files sampled each day

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(LowDif), 1)
  thisfile <- LowDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  alldays <- sort(unique(sub$JDAY))
  if(length(alldays) > 1){
    days <- sample(alldays,this.days,replace = T)
  }else{
    days<- alldays
  }
  sub1 <- subset(sub,JDAY%in%days)
  allfiles <- sort(unique(sub1$file))
  files <- sample(allfiles,this.files,replace = T)
  sub2 <- subset(sub1,file%in%files)
  sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  p1 <-specpool(sub3, smallsample = TRUE)
  dif_low$rich1[rownames(dif_low) == file]<- p1$boot
  dif_low$Rawdif[rownames(dif_low) == file]<- abs(LowDif$Rawdif[thisrow])
  dif_low$Accudif[rownames(dif_low) == file]<- abs(LowDif$Accudif[thisrow])
  thisfileY <- LowDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  alldaysY <- sort(unique(subY$JDAY))
  if(length(alldaysY) > 1){
    daysY <- sample(alldaysY,this.days,replace = T)
  }else{
    daysY<- alldaysY
  }
  sub1Y <- subset(subY,JDAY%in%daysY)
  allfilesY <- sort(unique(sub1Y$file))
  filesY <- sample(allfilesY,this.files,replace = T)
  sub2Y <- subset(sub1Y,file%in%filesY)
  sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  pY <-specpool(sub3Y, smallsample = TRUE)
  dif_low$rich2[rownames(dif_low) == file]<- pY$boot
}
dif_low$sampledif<- abs(dif_low$rich1 - dif_low$rich2)
plot(dif_low$Accudif ~ dif_low$sampledif)
l<- lm(dif_low$sampledif ~ dif_low$Accudif)
summary(l)
hist(dif_low$sampledif)

dif_med <- matrix(0,nrow=1001,ncol=5)
colnames(dif_med) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(dif_med)
dif_med<-as.data.frame(dif_med)
this.days <- 3           # define number of sampling days
this.files <- 72           # define number of files sampled each day

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(MedDif), 1)
  thisfile <- MedDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  alldays <- sort(unique(sub$JDAY))
  if(length(alldays) > 1){
    days <- sample(alldays,this.days,replace = T)
  }else{
    days<- alldays}
  sub1 <- subset(sub,JDAY%in%days)
  allfiles <- sort(unique(sub1$file))
  files <- sample(allfiles,this.files,replace = T)
  sub2 <- subset(sub1,file%in%files)
  sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  p1 <-specpool(sub3, smallsample = TRUE)
  dif_med$rich1[rownames(dif_med) == file]<- p1$boot
  dif_med$Rawdif[rownames(dif_med) == file]<- abs(MedDif$Rawdif[thisrow])
  dif_med$Accudif[rownames(dif_med) == file]<- abs(MedDif$Accudif[thisrow])
  thisfileY <- MedDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  alldaysY <- sort(unique(subY$JDAY))
  if(length(alldaysY) > 1){
    daysY <- sample(alldaysY,this.days,replace = T)
  }else{
    daysY<- alldaysY}
  sub1Y <- subset(subY,JDAY%in%daysY)
  allfilesY <- sort(unique(sub1Y$file))
  filesY <- sample(allfilesY,this.files,replace = T)
  sub2Y <- subset(sub1Y,file%in%filesY)
  sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  pY <-specpool(sub3Y, smallsample = TRUE)
  dif_med$rich2[rownames(dif_med) == file]<- pY$boot
}
dif_med$sampledif<- abs(dif_med$rich1 - dif_med$rich2)
plot(dif_med$sampledif~dif_med$Accudif, 
     xlim=c(0,30), ylim=c(0,30))

dif_high <- matrix(0,nrow=1001,ncol=5)
colnames(dif_high) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(dif_high)
dif_high<-as.data.frame(dif_high)
this.days <- 3           # define number of sampling days
this.files <- 72           # define number of files sampled each day

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(HighDif), 1)
  thisfile <- HighDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  alldays <- sort(unique(sub$JDAY))
  if(length(alldays) > 1){
    days <- sample(alldays,this.days,replace = T)
  }else{
    days<- alldays}
  sub1 <- subset(sub,JDAY%in%days)
  allfiles <- sort(unique(sub1$file))
  files <- sample(allfiles,this.files,replace = T)
  sub2 <- subset(sub1,file%in%files)
  sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  p1 <-specpool(sub3, smallsample = TRUE)
  dif_high$rich1[rownames(dif_high) == file]<- p1$boot
  dif_high$Rawdif[rownames(dif_high) == file]<- abs(HighDif$Rawdif[thisrow])
  dif_high$Accudif[rownames(dif_high) == file]<- abs(HighDif$Accudif[thisrow])
  thisfileY <- HighDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  alldaysY <- sort(unique(subY$JDAY))
  if(length(alldaysY) > 1){
    daysY <- sample(alldaysY,this.days,replace = T)
  }else{
    daysY<- alldaysY}
  sub1Y <- subset(subY,JDAY%in%daysY)
  allfilesY <- sort(unique(sub1Y$file))
  filesY <- sample(allfilesY,this.files,replace = T)
  sub2Y <- subset(sub1Y,file%in%filesY)
  sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  pY <-specpool(sub3Y, smallsample = TRUE)
  dif_high$rich2[rownames(dif_high) == file]<- pY$boot
}
dif_high$sampledif<- abs(dif_high$rich1 - dif_high$rich2)
plot(dif_high$sampledif ~ dif_high$Accudif, xlim=c(0,50), ylim=c(0,50))

# for(file in 1:nrownames){
#   
#   thisfile <- sample(this.site, 1)
#   sub <- subset(filepool,site==thisfile) ##choose file we are on now
#   alldays <- sort(unique(sub$JDAY))
#   days <- sample(alldays,min(length(alldays),this.days),replace = T)
#   sub1 <- subset(sub,JDAY%in%days)
#   allfiles <- sort(unique(sub1$file))
#   files <- sample(allfiles,this.files,replace = T)
#   sub2 <- subset(sub1,file%in%files)
#   sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
#   p1 <-specpool(sub3, smallsample = TRUE)
#   dif1$rich1[rownames(dif1) == file]<- p1$boot
#         
#   if(this.site = "2019 Sheldon Summer H"){
#     site2<- "2018 Sheldon Spring A"
#   }else{
#     if(this.site = "2018 Ely Spring G"){
#       site2<- "2018 Ely Spring C"
#     }else{
#       if(this.site = "2018 Sheldon Spring B"){
#         site2<- "2020 Sheldon Spring H"
#       }else{
#           site2<- "2020 Austin Spring F"
#         }
#         }
#       }
#   sub <- subset(filepool,site==site2) ##choose file we are on now
#   alldays <- sort(unique(sub$JDAY))
#   days <- sample(alldays,min(length(alldays),this.days),replace = T)
#   sub1 <- subset(sub,JDAY%in%days)
#   allfiles <- sort(unique(sub1$file))
#   files <- sample(allfiles,this.files,replace = T)
#   sub2 <- subset(sub1,file%in%files)
#   sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
#   p2 <-specpool(sub3, smallsample = TRUE)
#   dif1$rich2[rownames(dif1) == file]<- p2$boot
# }


###now for the morning only files
this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest
this.days <- 6           # define number of sampling days
this.files <- 48          # define number of files sampled each day

morn_low <- matrix(0,nrow=1001,ncol=5)
colnames(morn_low) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(morn_low)
morn_low<-as.data.frame(morn_low)

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(LowDif), 1)
  thisfile <- LowDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  subt <- subset(sub,time%in%this.hours)
  
  if(nrow(subt)>0){
  alldays <- sort(unique(subt$JDAY))
  days <- sample(alldays,min(length(alldays),this.days),replace = T)
  sub1 <- subset(subt,JDAY%in%days)
  allfiles <- sort(unique(sub1$file))
  files <- sample(allfiles,this.files,replace = T)
  sub2 <- subset(sub1,file%in%files)
  sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  p1 <-specpool(sub3, smallsample = TRUE)
  morn_low$rich1[rownames(morn_low) == file]<- p1$boot
  
  }else{
    morn_low$rich1[rownames(morn_low) == file]<- NA
  }
  morn_low$Rawdif[rownames(morn_low) == file]<- abs(LowDif$Rawdif[thisrow])
  morn_low$Accudif[rownames(morn_low) == file]<- abs(LowDif$Accudif[thisrow])
  thisfileY <- LowDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  subtY <- subset(subY,time%in%this.hours)
  if(nrow(subtY)>0){
  alldaysY <- sort(unique(subtY$JDAY))
  daysY <- sample(alldaysY,min(length(alldaysY),this.days),replace = T)
  sub1Y <- subset(subY,JDAY%in%daysY)
  allfilesY <- sort(unique(sub1Y$file))
  filesY <- sample(allfilesY,this.files,replace = T)
  sub2Y <- subset(sub1Y,file%in%filesY)
  sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  pY <-specpool(sub3Y, smallsample = TRUE)
  morn_low$rich2[rownames(morn_low) == file]<- pY$boot
  }else{
    morn_low$rich2[rownames(morn_low) == file]<- NA
  }
}
morn_low$sampledif<- abs(morn_low$rich1 - morn_low$rich2)
plot(morn_low$Accudif ~ morn_low$sampledif)
hist(morn_low$sampledif)

morn_med <- matrix(0,nrow=1001,ncol=5)
colnames(morn_med) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(morn_med)
morn_med<-as.data.frame(morn_med)

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(MedDif), 1)
  thisfile <- MedDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  subt <- subset(sub,time%in%this.hours)
  
  if(nrow(subt)>0){
    alldays <- sort(unique(subt$JDAY))
    days <- sample(alldays,min(length(alldays),this.days),replace = T)
    sub1 <- subset(subt,JDAY%in%days)
    allfiles <- sort(unique(sub1$file))
    files <- sample(allfiles,this.files,replace = T)
    sub2 <- subset(sub1,file%in%files)
    sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    p1 <-specpool(sub3, smallsample = TRUE)
    morn_med$rich1[rownames(morn_med) == file]<- p1$boot
    
  }else{
    morn_med$rich1[rownames(morn_med) == file]<- NA
  }
  morn_med$Rawdif[rownames(morn_med) == file]<- abs(MedDif$Rawdif[thisrow])
  morn_med$Accudif[rownames(morn_med) == file]<- abs(MedDif$Accudif[thisrow])
  thisfileY <- MedDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  subtY <- subset(subY,time%in%this.hours)
  if(nrow(subtY)>0){
    alldaysY <- sort(unique(subtY$JDAY))
    daysY <- sample(alldaysY,min(length(alldaysY),this.days),replace = T)
    sub1Y <- subset(subY,JDAY%in%daysY)
    allfilesY <- sort(unique(sub1Y$file))
    filesY <- sample(allfilesY,this.files,replace = T)
    sub2Y <- subset(sub1Y,file%in%filesY)
    sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    pY <-specpool(sub3Y, smallsample = TRUE)
    morn_med$rich2[rownames(morn_med) == file]<- pY$boot
  }else{
    morn_med$rich2[rownames(morn_med) == file]<- NA
  }
}
morn_med$sampledif<- abs(morn_med$rich1 - morn_med$rich2)
plot(morn_med$Accudif ~ morn_med$sampledif)
hist(morn_med$sampledif)

morn_high <- matrix(0,nrow=1001,ncol=5)
colnames(morn_high) <- c("rich1", "rich2", "Rawdif", "Accudif", "sampledif")
nrownames<- nrow(morn_high)
morn_high<-as.data.frame(morn_high)

file =1
for(file in 1:nrownames){
  thisrow <- sample(nrow(HighDif), 1)
  thisfile <- HighDif$SiteX[thisrow]
  sub <- subset(filepool,site==thisfile) ##choose file we are on now
  subt <- subset(sub,time%in%this.hours)
  
  if(nrow(subt)>0){
    alldays <- sort(unique(subt$JDAY))
    days <- sample(alldays,min(length(alldays),this.days),replace = T)
    sub1 <- subset(subt,JDAY%in%days)
    allfiles <- sort(unique(sub1$file))
    files <- sample(allfiles,this.files,replace = T)
    sub2 <- subset(sub1,file%in%files)
    sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    p1 <-specpool(sub3, smallsample = TRUE)
    morn_high$rich1[rownames(morn_high) == file]<- p1$boot
    
  }else{
    morn_high$rich1[rownames(morn_high) == file]<- NA
  }
  morn_high$Rawdif[rownames(morn_high) == file]<- abs(HighDif$Rawdif[thisrow])
  morn_high$Accudif[rownames(morn_high) == file]<- abs(HighDif$Accudif[thisrow])
  thisfileY <- HighDif$SiteY[thisrow] #now for the paired site
  subY <- subset(filepool,site==thisfileY) 
  subtY <- subset(subY,time%in%this.hours)
  if(nrow(subtY)>0){
    alldaysY <- sort(unique(subtY$JDAY))
    daysY <- sample(alldaysY,min(length(alldaysY),this.days),replace = T)
    sub1Y <- subset(subY,JDAY%in%daysY)
    allfilesY <- sort(unique(sub1Y$file))
    filesY <- sample(allfilesY,this.files,replace = T)
    sub2Y <- subset(sub1Y,file%in%filesY)
    sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    pY <-specpool(sub3Y, smallsample = TRUE)
    morn_high$rich2[rownames(morn_high) == file]<- pY$boot
  }else{
    morn_high$rich2[rownames(morn_high) == file]<- NA
  }
}
morn_high$sampledif<- abs(morn_high$rich1 - morn_high$rich2)
plot(morn_high$Accudif ~ morn_high$sampledif)
hist(morn_high$sampledif)

morning<- rbind(morn_low, morn_med, morn_high)
plot(morning$sampledif ~ morning$Accudif)
abline(1,1)
abline(-2.09,1)
l2<- lm(morning$sampledif ~ morning$Accudif)
summary(l2)

##now that I am coming back to this I don't know why I separated it into this stratified thing, like why didn't we just go through all the pairs 1000 times... 

DIF<- rbind(dif_low, dif_med, dif_high)
plot(DIF$sampledif ~ DIF$Accudif)
abline(1,1)
abline(-2.378,1)
l3<- lm(DIF$sampledif ~ DIF$Accudif)
summary(l3)

##okay we actually want to know if these sets are different not correlated
t<- t.test(DIF$Accudif, DIF$sampledif, paired = TRUE, alternative = "greater")
t #t = 18.386, df = 2948, p-value < 2.2e-16 
#mean of the differences 2.378014

##how often a difference not detected?
DIF2<- DIF[ which(DIF$Accudif < 1),] #208
DIF2<- DIF2[ which(DIF2$sampledif >1),] #176

176/3003 #5.861% of the time false positive for difference for 3 days straight 72 files across groups
DIF2<- DIF[ which(DIF$Accudif > 1),] #2795
DIF2<- DIF2[ which(DIF2$sampledif <1),] #175

175/3003 #5.8275% of the time false negative for difference for 3 days straight 72 files across groups

morn2<- morning[ which(morning$Accudif < 1),] #250
morn2<- morn2[ which(morn2$sampledif >1),] #203

203/3003 #6.759907% of time false positive for morning subset across groups

morn2<- morning[ which(morning$Accudif > 1),] #2753
morn2<- morn2[ which(morn2$sampledif <1),] #155
155/3003 #0.05161505 false negatives for morning subset across groups

tm<- t.test(morning$Accudif, morning$sampledif, paired = TRUE, alternative = "greater") #t = 18.191, df = 2848, p-value < 2.2e-16
tm #mean of the differences 2.099611 

#now by strata
dl<- t.test(dif_low$Accudif, dif_low$sampledif, paired = TRUE, alternative = "greater")
dl #t = -19.957, df = 975, p-value = 1, mean of the differences -2.899036 

dl2<- dif_low[ which(dif_low$Accudif < 1),] #208
dl2<- dl2[ which(dl2$sampledif >1),] #176
## 100% of false positives comes from low difference pairs - of course
dl2<- dif_low[ which(dif_low$Accudif > 1),] #793
dl2<- dl2[ which(dl2$sampledif <1),] #92 false negatives
dl3<- dif_med[ which(dif_med$sampledif <1),] #73 false negatives
dl4<- dif_high[ which(dif_high$sampledif <1),] #10 false negatives

dl5<- t.test(dif_med$Accudif, dif_med$sampledif, paired = TRUE, alternative = "greater")
dl5 #t = 12.781, df = 990, p-value < 2.2e-16
#mean of the differences 2.118249 
dl6<- t.test(dif_high$Accudif, dif_high$sampledif, paired = TRUE, alternative = "greater")
dl6 #t = 38.044, df = 981, p-value < 2.2e-16
# mean of the differences 7.884968

m<- morn_low[ which(morn_low$Accudif < 1),] #250
m<- m[ which(m$sampledif >1),] #203

m2<- morn_low[ which(morn_low$Accudif > 1),] #751
m2<- m2[ which(m2$sampledif <1),] #98 false negatives
m3<- morn_med[ which(morn_med$sampledif <1),] #55 false negatives
m4<- morn_high[ which(morn_high$sampledif <1),] #2 false negatives

m5<- t.test(morn_med$Accudif, morn_med$sampledif, paired = TRUE, alternative = "greater")
m5 #t = 13.16, df = 971, p-value < 2.2e-16 
#mean of the differences 1.979832 
m6<- t.test(morn_high$Accudif, morn_high$sampledif, paired = TRUE, alternative = "greater")
m6 #t = 37.331, df = 887, p-value < 2.2e-16
# mean of the differences 7.18416 


#I don't like the order I did this because there isn't an even amount of low difference pairings, medium, high etc...

## I think I should just do 10000 random pairs

PairsRich2<- PairsRich[ which(PairsRich$SiteX != PairsRich$SiteY),]
mean(abs(PairsRich2$Rawdif))#11.31409
sd(abs(PairsRich2$Rawdif)) ##8.299191
min(abs(PairsRich2$Rawdif)) #0
max(abs(PairsRich2$Rawdif)) #48

mean(abs(PairsRich2$Accudif)) #12.62947
sd(abs(PairsRich2$Accudif)) #9.252598
min(abs(PairsRich2$Accudif)) #0.00683879
max(abs(PairsRich2$Accudif)) #53.6161

randompair_3days<- data.frame(row.names = c(1:10000), 
                              SiteX=character(10000),
                              SiteY=character(10000),
                              Rawdif=numeric(10000),
                              Accudif=numeric(10000),
                              se=numeric(10000),
                              sampXraw=numeric(10000),
                              sampXacc=numeric(10000),
                              sampYraw=numeric(10000),
                              sampYacc=numeric(10000),
                              samp_se=numeric(10000),
                              stringsAsFactors = F)

#there are actually only 3655 unique pairs but all our doubled in PairsRich2

#  sub<-NULL
# sub1<-NULL
#  sub1Y<-NULL
#  sub2<-NULL
#  sub2Y<-NULL
#  sub3<-NULL
#  sub3Y<-NULL
#  subt<-NULL
#  subY<-NULL
#  subtY<-NULL

this.days <- 3           # define number of sampling days
this.files <- 72           # define number of files sampled each day

i=1
for(i in 1:10000){
  sites_row<- sample(7310, 1) #7310 in PairsRich2
  this_x<- PairsRich2$SiteX[sites_row]
  this_y<- PairsRich2$SiteY[sites_row]
  randompair_3days$SiteX[i] <- as.character(this_x)
  randompair_3days$SiteY[i] <- as.character(this_y)
  randompair_3days$Rawdif[i] <- PairsRich2$Rawdif[sites_row]
  randompair_3days$Accudif[i] <- PairsRich2$Accudif[sites_row]
  error<- PairsRich2$Xse[sites_row] + PairsRich2$Yse[sites_row]
  randompair_3days$se[i] <- error
  
    sub <- subset(filepool,site==this_x) ##choose file we are on now
    alldays <- sort(unique(sub$JDAY))
    days <- sample(alldays,min(length(alldays),this.days),replace = T)
    sub1 <- subset(sub,JDAY%in%days)
    allfiles <- sort(unique(sub1$file))
    files <- sample(allfiles,this.files,replace = T)
    sub2 <- subset(sub1,file%in%files)
    sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    p1 <-specpool(sub3, smallsample = TRUE)
    
    randompair_3days$sampXraw[i]<- p1$Species
    randompair_3days$sampXacc[i]<-p1$boot
    
    #now for the paired site
    subY <- subset(filepool,site==this_y) 
    alldaysY <- sort(unique(subY$JDAY))
    daysY <- sample(alldaysY,min(length(alldaysY),this.days),replace = T)
    sub1Y <- subset(subY,JDAY%in%daysY)
    allfilesY <- sort(unique(sub1Y$file))
    filesY <- sample(allfilesY,this.files,replace = T)
    sub2Y <- subset(sub1Y,file%in%filesY)
    sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    pY <-specpool(sub3Y, smallsample = TRUE)
    randompair_3days$sampYraw[i]<- pY$Species
    randompair_3days$sampYacc[i]<-pY$boot
    
    randompair_3days$samp_se[i]<-max(p1$boot.se, pY$boot.se)
    
}

randompair_3days$Rawdif<- abs(randompair_3days$Rawdif)
randompair_3days$Accudif<- abs(randompair_3days$Accudif)
mean(randompair_3days$Accudif) #12.61768
sd(randompair_3days$Accudif) #9.171526

randompair_3days$samp_Rawdif<- abs(randompair_3days$sampXraw - randompair_3days$sampYraw)

randompair_3days$samp_Accudif<- abs(randompair_3days$sampXacc - randompair_3days$sampYacc)
r3d<- randompair_3days[ which(randompair_3days$samp_Accudif != ""),]
mean(r3d$samp_Accudif) #9.748111
sd(r3d$samp_Accudif) #7.217138

t1<- t.test(randompair_3days$Accudif, randompair_3days$samp_Accudif, paired = TRUE, alternative = "greater")
t1 #t = 41.51, df = 9821, p-value < 2.2e-16
#mean of the differences 2.809838 

plot(randompair_3days$samp_Accudif ~ randompair_3days$Accudif)
abline(1,1)

t2<- t.test(randompair_3days$Rawdif, randompair_3days$samp_Rawdif, paired = TRUE, alternative = "greater")
t2

#do error bars overlap?
r3d$overlap<- 'x'
nobs<- nrow(r3d)

i=1
for(i in 1:nobs){
  dif<- abs(r3d$Accudif[i]-r3d$samp_Accudif[i])
  error<- r3d$se[i]+r3d$samp_se[i]
  if(dif <= error){
    r3d$overlap[i]<- 'TRUE'
  }else{
    if(r3d$Accudif[i] > r3d$samp_Accudif[i]){
      r3d$overlap[i]<- 'FALSE-low'
    }else{
    r3d$overlap[i]<- 'FALSE-high'
    }
  }
  
}

rwdt<- r3d[ which(r3d$overlap=="TRUE"),]
5739/9822 #0.5843005 there is overlap

rwdlow<- subset(r3d, overlap=="FALSE-low"&Accudif>1)
3219/9822 ## 0.3277337 of time the estimate is low

rwd0<- subset(rwdlow, samp_Accudif<1)
253/9798 #0.0258216 no difference detected when there is a difference

rwdhigh<- r3d[ which(r3d$overlap=="FALSE-high"),]
910/9798 #0.0928761% of time the estimate is high
rwd1<- subset(rwdhigh, Accudif<1)
129/9798 #0.01316595

## let's just do mornings to get it done
randompair_morns<- data.frame(row.names = c(1:10000), 
                              SiteX=character(10000),
                              SiteY=character(10000),
                              Rawdif=numeric(10000),
                              Accudif=numeric(10000),
                              se=numeric(10000),
                              sampXraw=numeric(10000),
                              sampXacc=numeric(10000),
                              sampYraw=numeric(10000),
                              sampYacc=numeric(10000),
                              samp_se=numeric(10000),
                              stringsAsFactors = F)

this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest
this.days <- 6           # define number of sampling days
this.files <- 48          # define number of files sampled each day

##okay let's first subset so that only these times exist in filepool
mornfiles<- subset(filepool,time%in%this.hours)
##okay perfect
unique(as.character(filepool$site))
unique(as.character(mornfiles$site))
#removes many sound files but only 2 sites, great
mornsites<- unique(as.character(mornfiles$site))

i=1
for(i in 1:10000){
  siteS<- sample(84, 2, replace = F) #84 sites in mornfiles
  this_x<- mornsites[siteS[1]]
  this_y<- mornsites[siteS[2]]
  
  sub <- subset(mornfiles,site==this_x) 
  subY <- subset(mornfiles,site==this_y) 
  
  alldays <- sort(unique(sub$JDAY))
  days <- sample(alldays,min(length(alldays),this.days),replace = T)
  sub1 <- subset(sub,JDAY%in%days)
  allfiles <- sort(unique(sub1$file))
  files <- sample(allfiles,this.files,replace = T)
  sub2 <- subset(sub1,file%in%files)
  sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  p1 <-specpool(sub3, smallsample = TRUE)
  randompair_morns$sampXraw[i]<- p1$Species
  randompair_morns$sampXacc[i]<-p1$boot
 
  alldaysY <- sort(unique(subY$JDAY))
  daysY <- sample(alldaysY,min(length(alldaysY),this.days),replace = T)
  sub1Y <- subset(subY,JDAY%in%daysY)
  allfilesY <- sort(unique(sub1Y$file))
  filesY <- sample(allfilesY,this.files,replace = T)
  sub2Y <- subset(sub1Y,file%in%filesY)
  sub3Y <- subset(sub2Y, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
  pY <-specpool(sub3Y, smallsample = TRUE)
  randompair_morns$sampYraw[i]<- pY$Species
  randompair_morns$sampYacc[i]<-pY$boot
  
  randompair_morns$SiteX[i] <- as.character(this_x)
  randompair_morns$SiteY[i] <- as.character(this_y)
  
  thispair<- subset(PairsRich2, SiteX==this_x&SiteY==this_y)
  randompair_morns$Rawdif[i] <- thispair$Rawdif
  randompair_morns$Accudif[i] <- thispair$Accudif
  error<- thispair$Xse + thispair$Yse
  randompair_morns$se[i] <- error
  randompair_morns$samp_se[i]<-max(p1$boot.se, pY$boot.se)
  
}

randompair_morns$Rawdif<- abs(randompair_morns$Rawdif)
randompair_morns$Accudif<- abs(randompair_morns$Accudif)
mean(randompair_morns$Accudif) #11.91026
sd(randompair_morns$Accudif) #8.687128

randompair_morns$samp_Rawdif<- abs(randompair_morns$sampXraw - randompair_morns$sampYraw)

randompair_morns$samp_Accudif<- abs(randompair_morns$sampXacc - randompair_morns$sampYacc)
rmor<- randompair_morns[ which(randompair_morns$samp_Accudif != ""),]
mean(rmor$samp_Accudif) #9.56776
sd(rmor$samp_Accudif) #6.920018

t4<- t.test(randompair_morns$Accudif, randompair_morns$samp_Accudif, paired = TRUE, alternative = "greater")
t4

plot(randompair_morns$samp_Accudif ~ randompair_morns$Accudif)
abline(1,1)

t5<- t.test(randompair_morns$Rawdif, randompair_morns$samp_Rawdif, paired = TRUE, alternative = "greater")
t5

#do error bars overlap?
rmor$overlap<- 'x'
nobs<- nrow(rmor)

i=1
for(i in 1:nobs){
  dif<- abs(rmor$Accudif[i]-rmor$samp_Accudif[i])
  error<- rmor$se[i]+rmor$samp_se[i]
  if(dif <= error){
    rmor$overlap[i]<- 'TRUE'
  }else{
    if(rmor$Accudif[i] > rmor$samp_Accudif[i]){
      rmor$overlap[i]<- 'FALSE-low'
    }else{
      rmor$overlap[i]<- 'FALSE-high'
    }
  }
  
}

rmtrue<- rmor[ which(rmor$overlap=="TRUE"),]
6350/9968 #0.6370385 there is overlap

rmlow<- rmor[ which(rmor$overlap=="FALSE-low"),]
2779/9968 ## 0.2787921 of time the estimate is low

rm0<- subset(rmor, samp_Accudif<1&overlap!="TRUE")
229/9968 #0.02297352 no difference detected when there is a difference

rmhigh<- subset(rmor, Accudif<1&overlap=="FALSE-high")
126/9968 #0.01264045 of time false positive 

rm1<- subset(rmor, samp_Accudif>1&overlap=="FALSE-high")
839/9968 #0.08416934 difference detected by higher estimate

AllTripods17.20 <- read.csv("C:/Users/hamme/Dropbox (Eagle Crest Realty)/GBLCC P-J Project/Field Sites/Set-up/AllTripods17-20.csv")

AllTripods17.20$Tripod[AllTripods17.20$Tripod == "i"] <- "I"
unique(AllTripods17.20$Tripod)

filehab<- merge(filepool, AllTripods17.20, by=c("Year","Region","Season","Tripod"), all.x = TRUE)

#write.csv(filehab, "pj-sage.csv")

pjs <- read.csv("C:/Users/hamme/Dropbox (Eagle Crest Realty)/GBLCC P-J Project/PAPERS/Avian Acoustics/pj-sage.csv")

## we will do a t-test of these two habitat types and then we will do these sites with subsets to see if we see this trend still with the subsets

pjsites<- unique(pjs$site)
#okay there are 58

pj_compare<- data.frame(site=character(58), 
                        Habitat=character(58),
                        AccumRich=numeric(58),
                        stringsAsFactors = F)

i=1
for(i in 1:58){
  thissite<- pjsites[i]
    sub <- subset(filepool,site==thissite)
    sub1<- subset(sub, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    hsub<- subset(pjs,site==thissite)
    p1<-specpool(sub1, smallsample = TRUE)
    pj_compare$site[i]<- thissite
    pj_compare$Habitat[i]<- hsub$Habitat[1]
    pj_compare$AccumRich[i]<- p1$boot
  
}

tp<- t.test(pj_compare$AccumRich ~ pj_compare$Habitat)
tp


## okay so that is the full data, but let's do these same sites with just 3 days of data

this.days <- 3           # define number of sampling days
this.files <- 72           # define number of files sampled each day

pj_3days<- data.frame(PJ_mean=numeric(1000),
                        Sage_mean=numeric(1000),
                        p_value=numeric(1000),
                        sig_dif=character(1000),
                        stringsAsFactors = F)

i=1
for(i in 1:1000){
  
  trial<- data.frame(Habitat=character(58),
    AccumRich=numeric(58),
    stringsAsFactors = F)
  
  SITE=1
  for(SITE in 1:58){
    thissite<- pjsites[SITE]
    sub <- subset(filepool,site==thissite)
    alldays <- sort(unique(sub$JDAY))
    days <- sample(alldays,min(length(alldays),this.days),replace = T)
    sub1 <- subset(sub,JDAY%in%days)
    allfiles <- sort(unique(sub1$file))
    files <- sample(allfiles,this.files,replace = T)
    sub2 <- subset(sub1,file%in%files)
    sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY))
    p1 <-specpool(sub3, smallsample = TRUE)
    hsub<- subset(pjs,site==thissite)
    trial$Habitat[SITE]<- hsub$Habitat[1]
    trial$AccumRich[SITE]<- p1$boot
  }
  
  tps<- t.test(trial$AccumRich ~ trial$Habitat)
  pj_3days$PJ_mean[i]<- tps$estimate[1]
  pj_3days$Sage_mean[i]<- tps$estimate[2]
  pj_3days$p_value[i]<- tps$p.value
  if(tps$p.value < 0.5){
    pj_3days$sig_dif[i]<- 'TRUE'
  }else{
    pj_3days$sig_dif[i]<- 'FALSE'
  }
}

unique(pj_3days$sig_dif)
###wooh 100%

this.hours <- c(4,5,6,7,8,9,10,11) # define the hours of interest
this.days <- 6           # define number of sampling days
this.files <- 48          # define number of files sampled each day

##okay let's first subset so that only these times exist in filepool
mornpj<- subset(pjs,time%in%this.hours)
##okay perfect
unique(as.character(mornpj$site))
#56 so removed 2 sites
mornsites<- unique(as.character(mornpj$site))

pj_morns<- data.frame(PJ_mean=numeric(1000),
                      Sage_mean=numeric(1000),
                      p_value=numeric(1000),
                      sig_dif=character(1000),
                      stringsAsFactors = F)

i=1
for(i in 1:1000){
  
  trial<- data.frame(Habitat=character(56),
                     AccumRich=numeric(56),
                     stringsAsFactors = F)
  
  SITE=1
  for(SITE in 1:56){
    thissite<- mornsites[SITE]
    sub <- subset(mornpj,site==thissite)
    alldays <- sort(unique(sub$JDAY))
    if(length(alldays) > 1){
      days <- sample(alldays,this.days,replace = T)
    }else{
      days<- alldays
    }
    sub1 <- subset(sub,JDAY%in%days)
    allfiles <- sort(unique(sub1$file))
    files <- sample(allfiles,this.files,replace = T)
    sub2 <- subset(sub1,file%in%files)
    sub3 <- subset(sub2, select = -c(Year, Region, Tripod, Season, FileDate, FileStartTime, file, site, Condition, rec, time, DATE, JDAY, Transect, Gradient, Cows, Habitat, RemovalType))
    p1 <-specpool(sub3, smallsample = TRUE)
    trial$Habitat[SITE]<- sub$Habitat[1]
    trial$AccumRich[SITE]<- p1$boot
  }
  
  tps<- t.test(trial$AccumRich ~ trial$Habitat)
  pj_morns$PJ_mean[i]<- tps$estimate[1]
  pj_morns$Sage_mean[i]<- tps$estimate[2]
  pj_morns$p_value[i]<- tps$p.value
  if(tps$p.value < 0.5){
    pj_morns$sig_dif[i]<- 'TRUE'
  }else{
    pj_morns$sig_dif[i]<- 'FALSE'
  }
}

unique(pj_morns$sig_dif)
###wooh 100%

## let's make a pretty box plot perhaps?
pj<- as.data.frame(pj_3days$PJ_mean)
names(pj)[names(pj)=="pj_3days$PJ_mean"]<-"AccumRich"
pj$sample<- "72-files 3-days"
pj$Habitat<- "PJ woodland"
sage<- as.data.frame(pj_3days$Sage_mean)
names(sage)[names(sage)=="pj_3days$Sage_mean"]<-"AccumRich"
sage$sample<- "72-files 3-days"
sage$Habitat<- "Sagebrush"
# pj2<- pj_compare
# pj2$site<-NULL
# pj2$sample<-"full"

pjm<- as.data.frame(pj_morns$PJ_mean)
names(pjm)[names(pjm)=="pj_morns$PJ_mean"]<-"AccumRich"
pjm$sample<- "48-files 6-mornings"
pjm$Habitat<- "PJ woodland"
sagem<- as.data.frame(pj_morns$Sage_mean)
names(sagem)[names(sagem)=="pj_morns$Sage_mean"]<-"AccumRich"
sagem$sample<- "48-files 6-mornings"
sagem$Habitat<- "Sagebrush"

# samps<- rbind(pj,pj2,sage,pjm,sagem)
# samps<- samps[ which(samps$AccumRich != "1.25"),]
# ggplot(samps, aes(x=sample, y=AccumRich, fill=Habitat)) +
#   geom_boxplot()
# # Change the position
 p<-ggplot(samps, aes(x=sample, y=AccumRich, fill=Habitat)) +
   geom_boxplot(position=position_dodge(1))
 p
#eh

##okay we don't care about the raw dataset let's just do a nice box plot of the 2 subseets pj and sage
samps2<- rbind(pj,sage,pjm,sagem)

cairo_pdf("PJ-sage.pdf", width=8,height=5)

p1<-ggplot(samps2, aes(x=sample, y=AccumRich, fill=Habitat)) +
  geom_boxplot(width=2)+theme_classic()+theme(axis.text= element_text(face="bold"), plot.title = element_text(hjust = 0.5), legend.position = c(0.93, 0.8), axis.title = element_text(face="bold"))+
  labs(title="Mean Species Richness in PJ and Sagebrush Habitat Using Top Monitoring Protocols", x="Monitoring Protocol", y="Mean Extrapolated Species Richness")
p1+scale_fill_manual(values=c("gray97", "gray67"))

dev.off()

##okay I should also probably do a figure for the random pairs and stratified pairs but they are such dense figures it looks like a black mob of dots unless I thinned it out so I'll ignore that

