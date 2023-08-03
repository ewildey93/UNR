library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(mapview)
library(tidyverse)
library(leafpop)
library(vegan)
library(amt)
library(lubridate)
setwd("C:/Users/eliwi/OneDrive/Documents/UNR")

Springs <- read.csv("C:/Users/eliwi/OneDrive/Documents/UNR/Spring Locs.csv")
Springs$Code <- c("BBS", "DVS", "UC", "UIS")
Smammals <-read.csv('./SmammalData.csv') 
str(Smammals)
unique(Smammals$Species)
Smammals$Species <- gsub(pattern = "spp.",replacement = "sp.",x = Smammals$Species)
Smammals$Species <- gsub(pattern = "Tamius",replacement = "Tamias",x = Smammals$Species)

Smammals <- mutate(Smammals, labels= case_when(
                                    Species == "Peromyscus maniculatus" ~ "P. maniculatis",
                                    Species == "Tamias sp." ~ "Tamias sp.",
                                    Species == "Microtus montanus" ~ "M. montanus",
                                    Species == "Tamias minimus"  ~ "T. minimus",
                                    Species == "Lemmiscus curtatus" ~ "L.curtatus",
                                    Species == "Dipodomys californicus" ~ "D.californicus",
                                    Species == "Woodrat sp." ~ "Woodrat sp.",
                                    Species == "Pocket mouse?" ~ "Pocket mouse",
                                    Species == "Kangaroo Rat" ~ "Kangaroo Rat",
                                    Species == "Microtus sp." ~ "Microtus sp."
))



ggplot(Smammals[Smammals$Spring == "BBS" & Smammals$Recap== "N",], aes(x=labels, fill=labels))  +
  geom_bar(stat="count", position = "dodge") +
  xlab("Species") + ylab("Count of Individuals") +
  theme(legend.position = "none") + scale_x_discrete(breaks=unique(Springs$labels), 
                                                     labels=unique(Springs$xlabels2))

my_list <- list()  
loop<-for (i in 1:length(Springs$Code)) {
  print(Springs$Code[i])
  Spring<-Smammals%>% filter(Spring==Springs$Code[i])
  plot<-ggplot(Spring[Spring$Recap == "N",], aes(x = labels, fill=labels)) + 
    geom_bar(stat="count", position = "dodge") +
    ylab("Number of Individuals") + xlab("Species") +
    theme(legend.position = "none") +
    coord_flip()
  my_list[[i]] <- plot
}


map <- leaflet(data=Springs) %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  addMarkers(lng = ~Lon, lat = ~Lat, 
             label = ~Name,
             popup = popupGraph(my_list))

map





## 'leaflet' objects (image above)
m <- leaflet() %>% addTiles()
mapshot(m, file = "~/Rplot.png")

## 'mapview' objects (image below)
m2 <- mapview(map)
mapshot(m2, file = "~/breweries.png")
mapshot(map, url = "SpringsMammalMap.html")


#rarefy- combine smammal data, bat data, and camera data (and bird/frog data)?
Smammals%>%group_by(Spring)%>%summarise(Richness = length(unique(Species)))
SmammalCount <- Smammals%>%filter(Recap == "N")%>%
  select(4,9)%>%
  pivot_wider(names_from = Species,values_from = Species, values_fn = length)
SmammalCount[is.na(SmammalCount)] <- 0

rarefy(SmammalCount[-1], sample=12, MARGIN=1)
raremax <- min(rowSums(SmammalCount[,-1]))
rarecurve(SmammalCount[,-1])


#################Species Accumulation Curves, by Trap Night, also by Time of Year?##########################
Smammals$Date <- as.Date(Smammals$Date, format= "%m/%d/%Y")
Smammals <- Smammals[
  with(Smammals, order(Spring, Date,Check.)),
]
#some rows have missing time and we need time to assign to time of day and check number
Smammals$DateTime <- as.POSIXct(paste(Smammals$Date, Smammals$Time), format= "%Y-%m-%d %H:%M")
#can't have ggplot package loaded or fill will be masked
Smammals3 <- fill(Smammals, DateTime)
Smammals3$DateTime2 <- Smammals$DateTime
Smammals3$Line <- gsub("Water East", "Water", Smammals3$Line)
Smammals3$Line <- gsub("Water U-Line", "Water", Smammals3$Line)
Smammals3$Month <- month(Smammals3$Date)
Smammals3$Year <- year(Smammals3$Date)


Effort <- read.csv("./MammalTrappingEffort.csv", na.strings=c("","NA"))
Effort$TrapNights <- Effort$Total.Traps*Effort$Trap.Checks
Effort$Date <- as.Date(Effort$Date, format= "%m/%d/%y")
Effort<- fill(Effort, Spring, Line, .direction = "down")
colnames(Effort)[4] <- "Check."
Effort$Month <- month(Effort$Date)
Effort$Check. <- as.character(Effort$Check.)
str(Effort)
str(Smammals3)
table(Smammals3$Line)
table(Effort$Line)
table(is.na(Smammals$Date))


Smammals4 <- Smammals3[Smammals3$Date > "2023-01-01",]
Smammals5 <- left_join(Smammals4, Effort, by=c("Spring", "Line","Month", "Check."), relationship= "many-to-one")
table(is.na(Smammals5$TrapNights))
Check <- Smammals5[is.na(Smammals5$TrapNights),]

Smammals5$site <- as.factor(paste(Smammals5$Spring, Smammals5$Line,Smammals5$Month, Smammals5$Year))
levels(Smammals5$site)
Smammals6 <- Smammals5%>%pivot_wider(names_from = Species,values_from = Species, values_fn = length)
Smammals6[,40:46][is.na(Smammals6[,40:46])] <- 0
#Recaps included? yes or no
Smammals6 <- Smammals6
SmammalsCount <- Smammals6%>%group_by(Spring,Line, Check.)%>%mutate("P.maniculatis" = sum(`Peromyscus maniculatus`),
                                                      "T.minimus" = sum(`Tamias minimus`),
                                                      "Tamias sp" = sum(`Tamias sp.`),
                                                      "K Rat" = sum(`Kangaroo Rat`),
                                                      "Woodrat" = sum(`Woodrat sp.`),
                                                      "Pocket Mouse" = sum(`Pocket mouse?`),
                                                      "Microtus sp" = sum(`Microtus sp.`))%>%distinct(site, .keep_all = T)%>%select(1,6,7,38,39,47:53)
SmammalsOccurence <- Smammals6%>%group_by(Spring,Line, Check.)%>%mutate("P.maniculatis" = ifelse(sum(`Peromyscus maniculatus`) > 0, 1, 0),
                                                                   "T.minimus" = ifelse(sum(`Tamias minimus`) > 0, 1, 0),
                                                                   "Tamias sp" = ifelse(sum(`Tamias sp.`) > 0, 1,0),
                                                                   "K Rat" = ifelse(sum(`Kangaroo Rat`) > 0, 1,0),
                                                                   "Woodrat" = ifelse(sum(`Woodrat sp.`) >0 ,1,0),
                                                                   "Pocket Mouse" = ifelse(sum(`Pocket mouse?`) > 0, 1,0),
                                                                   "Microtus sp" = ifelse(sum(`Microtus sp.`) < 0 ,1 ,0))%>%distinct(site, .keep_all = T)%>%select(1,6,7,38,39,47:53)

sitenames<- unique(SmammalsOccurence$site)
nsitenames<- length(unique(SmammalsOccurence$site))

filepool<- SmammalsOccurence
file = 1
poolsites<- as.data.frame(sitenames)
poolsites$Obsfull <- 'x'
poolsites$bootfull <- 'x'
poolsites$bootfullse <- 'x'

#based on occurence specpool function
for(file in 1:nsitenames){
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile)
  sub1<- subset(sub, select = -c(Spring, Line, Date.x, Check.,TrapType, TrapNights,site))
  #estimates extrapolated species richness
  p1<-specpool(sub1, smallsample = TRUE)
  poolsites$Obsfull[poolsites$sitenames == thisfile]<- p1$Species
  poolsites$bootfull[poolsites$sitenames == thisfile]<- p1$boot
  poolsites$bootfullse[poolsites$sitenames == thisfile]<- p1$boot.se
}

filepool<- SmammalsCount
file = 1
poolsites<- as.data.frame(sitenames)
poolsites$Obsfull <- 'x'
poolsites$bootfull <- 'x'
poolsites$bootfullse <- 'x'
#based on counts estimateR function
for(file in 1:nsitenames){
  thisfile <- sitenames[file]
  sub <- subset(filepool,site==thisfile)
  sub1<- subset(sub, select = -c(Spring, Line, Date.x, Check.,TrapType, TrapNights,site))
  #estimates extrapolated species richness
  p1<-estimateR(sub1)
  poolsites$Obsfull[poolsites$sitenames == thisfile]<- p1$Species
  poolsites$bootfull[poolsites$sitenames == thisfile]<- p1$boot
  poolsites$bootfullse[poolsites$sitenames == thisfile]<- p1$boot.se
}




BBS <- Smammals5[Smammals5$Spring == "BBS",]
BBS <- BBS%>%pivot_wider(names_from = Species,values_from = Species, values_fn = length)
data(BCI)
specaccum(xvar=TrapNights, w=TrapNights)

###############scrap##################
beetles <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/carabid-beetles-boreal-forest.txt', row.names = 1)
Tbeetles <- as.data.frame(t(beetles))

p
p1
