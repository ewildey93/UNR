library(lubridate)
library(stringr)
library(gt)
setwd("C:/Users/eliwi/OneDrive/Documents/UNR/")

Herps <- read.csv("./HerpSurveys.csv")
Herps$DateTimeSt <- paste(Herps$Date, Herps$Start.Time)
Herps$DateTimeEnd <- paste(Herps$Date, Herps$End.Time)
Herps[,28:29] <- lapply(Herps[,28:29], function (x) as.POSIXct(x,format= "%m/%d/%y %H:%M"))
Herps$SurveyTime <- difftime(Herps$DateTimeEnd, Herps$DateTimeSt)
Herps$Site.Name[Herps$Site.Name == "UC" | Herps$Site.Name =="Control"] <- "Orange"
table(Herps$Site.Name)
sum(Herps$SurveyTime, na.rm = T)
!is.na(Herps[,14:25])
HerpTable <- data.frame(Spring=c("UIS","Orange", "DVS", "BBS"), SpeciesDetected= c("Unidentified Rodent or Lizard", "Common Garter Snake, Western Terrestrial Garter Snake, Great Basin Fence Lizard, Tadpoles", "Western Chorus Frog", "Tadpoles"))
SamplingEffort <- Herps%>%group_by(Site.Name)%>%summarise(Effort=sum(SurveyTime, na.rm=T))
colnames(SamplingEffort)[1] <- "Spring"
HerpTable <- merge(HerpTable, SamplingEffort, by="Spring")

HerpTablePub <- HerpTable%>%gt()%>%
  tab_header(
    title= "Species Encoutnered During Visual Encounter Surveys"
  )%>%
  opt_row_striping(row_striping=TRUE)%>%
  cols_label(
    Spring= "Spring",
    SpeciesDetected= "Species Detected",
    Effort= "Sampling Effort (min)")%>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = 2))%>%
  tab_options(data_row.padding = px(15)) %>%
  fmt_markdown(columns = 2) %>%
  cols_align(align = "center", columns = 2)
HerpTablePub
gtsave(HerpTablePub, "HerpTable.png")
