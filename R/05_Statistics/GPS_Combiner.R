# 1. Initialization ----
source("R/BaseFunctions.R")

load(file=paste0(InSitu_dir,"07_FieldCampaign17/02_Station/GPS.RData")) # gps

unqstats<-unique(gps$Stat)

for (i in 1:unqstats){
  
  unqstat<-unqstats[i]
  
  gps1<-gps %>% filter(.,Stat==unqstat)
  
  leaflet() %>% addTiles() %>% 
    addMarkers(lng=gps1$Lon %>% as.character %>% as.numeric,
               lat=gps1$Lat %>% as.character %>% as.numeric,
               popup = paste0("FOI:",gps1$Stat,"<br>",
                              "Sensor:",gps1$Sensor,"<br>",
                              "Date:",gps1$Date,"<br>",
                              "ID:",gps1$SubID,"<br>")
    )
  
  
}

