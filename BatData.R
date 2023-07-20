####Sonobat Data###########
library(dplyr)


BatDat <- read.delim("C:/Users/eliwi/OneDrive/Documents/UNR/DVSTRI4_B2SonoBat.txt")
str(BatDat)
BatDat2 <- BatDat[,c(1:6, 9, 10)]
table(BatDat2$SppAccp)
table(BatDat2$X.Spp)
unique(BatDat2$SppAccp)
BatDat2 <- BatDat2%>%mutate("CommonName" = case_when(SppAccp =="Myev" ~ "Long-Eared Myotis", 
          SppAccp == "Epfu" ~"Big Brown Bat",
          SppAccp == "Myci" ~ "Western Small-Footed Myotis",
          SppAccp == "Lano" ~ "Silver-Haired Bat",
          SppAccp == "Myth" ~ "Fringed Myotis",
          SppAccp == "Tabr" ~ "Brazilian Free-Tailed Bat",
          SppAccp == "Myca" ~ "California Myotis",
          SppAccp == "Mylu" ~ "Little Brown Myotis",
          SppAccp == "Pahe" ~ "Western Pipistrelle",
          SppAccp == "Myvo" ~"Long-Legged Myotis"))
"Anpa" ~ "Pallid Bat"
"Coto" ~ "Townsend's Big-Eared Bat"
"Labl" ~ "Western Red Bat"
"Laci" ~ "Hoary Bat"
"Myyu" ~ "Yuma Myotis"

str(BatDat2)
BatDat2$SppAccp
BatDat2$SppAccp[BatDat2$SppAccp == ""] <- NA
BatCount <- BatDat2%>%
  pivot_wider(names_from = SppAccp,values_from = SppAccp, values_fn = length)
BatCount[,9:19][is.na(BatCount[,9:19])] <- 0
BatCount2 <- BatCount %>% summarise(across(c(10:19), sum), .groups = "drop")
