####Sonobat Data###########
library(dplyr)
library(tidyr)

BatDat <- read.delim("C:/Users/eliwi/OneDrive/Documents/UNR/DVSTRI4_B2SonoBat.txt", na.strings=c("NA", ""))
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

BatDat3 <- BatDat2%>%separate_wider_delim(X.Spp, delim="/", names=c("SppA", "SppB", "SppC", "SppD"), 
                                          too_few = "align_start")
BatDat3 <- BatDat3%>%separate_wider_delim(X.Prob, delim="/", names=c("ProbA", "ProbB", "ProbC", "ProbD"),
                                          too_few= "align_start")
BatDat3[BatDat3 == "NaN"] <- NA
BatDat3$Prob[is.na(BatDat3$Prob)] <- 0
BatDat3$ProbA[is.na(BatDat3$ProbA)] <- 0
BatDat4 <- BatDat3[BatDat3$ProbA >= 0.8 | BatDat3$Prob >=0.8,]
table(BatDat4$SppAccp)
table(BatDat4$SppA)
BatDat5 <- BatDat4[,c(2,5,6,7,11)]%>%pivot_longer(cols= -Filename, names_to = c("Species", ".value"), 
                                                  names_sep="_", values_drop_na = TRUE)


str(BatDat2)
BatDat2$SppAccp
BatDat2$SppAccp[BatDat2$SppAccp == ""] <- NA
BatCount <- BatDat2%>%
  pivot_wider(names_from = SppAccp,values_from = SppAccp, values_fn = length)
BatCount[,9:19][is.na(BatCount[,9:19])] <- 0
BatCount2 <- BatCount %>% summarise(across(c(10:19), sum), .groups = "drop")


#Activity levels
BatAct <- BatDat2%>%separate_wider_delim(Filename,"_", names=c("SerialNo","Date","Time"), too_few="debug")
BatAct$Time <- gsub(pattern=".wav",replacement="",BatAct$Time)
BatAct <- BatAct[BatAct$Filename_pieces == 3 & !is.na(BatAct$Filename),]
BatAct$Date <- as.Date(BatAct$Date, format= "%Y%m%d")
BatAct$DateTime <- as.POSIXct(paste(BatAct$Date, BatAct$Time), format="%Y-%m-%d %H%M%S")
BatAct <- BatAct[!is.na(BatAct$SppAccp) | !is.na(BatAct$X.Spp),]
table(BatAct$Date)
