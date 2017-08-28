###########################
#' 00. In Situ Plot 1 Script
#'
#' This script allows plot the outputs of the InSituReader.R script in different ways:
#' * reprentation of the GPS values
#' * 
#' 
###########################
source("C:/Users/MRossi/Documents/07_Codes/PhD_Thesis/R/00_BaseFunctions.R")

### 01. Plots 2 GPS ####

load("07_FieldCampaign17/02_Station/GPS.RData") # gps

stats<-gps$Stat %>% unique

for (i in 1:length(stats)){
  
  gps1<-gps %>% filter(Stat==stats[i]) %>% filter(Sensor=="InSitu") %>% as.tibble 
  
  ggplot(gps1,aes(Lat,Lon))+
    geom_point()
  
  m<-leaflet() %>% addTiles() %>% addMarkers(
    lng=gps1$Lon %>% as.character %>% as.numeric,
    lat=gps1$Lat %>% as.character %>% as.numeric,
    popup=gps1$Stat)
  
  mylocations<-cbind(gps1$Lon %>% unfac,gps1$Lat %>% unfac) %>% as.tibble
  ext<-extent(mylocations)
  
  MyMap<-get_map(location=mylocations[1,],source="google",maptype="satellite",zoom=16)+
    geom_point(mylocations,aes(V1,V2))
  
  
}
