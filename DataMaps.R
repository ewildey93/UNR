library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(mapview)
library(tidyverse)
library(leafpop)
library(vegan)
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
  theme(legend.position = "none") + scale_x_discrete(breaks=unique(Spring$labels), 
                                                     labels=unique(Spring$xlabels2))

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




###############scrap##################
beetles <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/carabid-beetles-boreal-forest.txt', row.names = 1)
Tbeetles <- as.data.frame(t(beetles))
