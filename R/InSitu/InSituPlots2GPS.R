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

options(digits=10)

load(paste0(InSitu_dir,"07_FieldCampaign17/02_Station/GPS.RData")) # gps

stats<-gps$Stat %>% unique
griddir<-"C:/Users/MRossi/Documents/08_Temp/04_Shapefile"
lf<-list.files(griddir,pattern=".shp.shp")

for (i in 1:length(stats)){
  
  gps1<-gps %>% filter(Stat==stats[i]) %>% filter(Sensor=="InSitu") %>% as.tibble 
  stat1<-gps1$Stat %>% unique
  
  # m<-leaflet() %>% addTiles() %>% addMarkers(
  #   lng=gps1$Lon %>% as.character %>% as.numeric,
  #   lat=gps1$Lat %>% as.character %>% as.numeric,
  #   popup=gps1$Stat)
  
  mylocations<-cbind(gps1$Lon %>% unfac, gps1$Lat %>% unfac) %>% as.tibble
  mylocations[["Date"]]<-gps1$Date
  ext<-cbind(gps1$Lon %>% unfac, gps1$Lat %>% unfac) %>% extent

  ifelse(stat1!="Vimes1500",zoom<-19,zoom<-15)
  MyMap<-get_map(location=mylocations[1,1:2],source="google",maptype="hybrid",zoom=zoom)
  
  grids<-lf %>% grep(stat1 %>% toupper,.) %>% lf[.]
  grids<-substr(grids,1,nchar(grids)-4)
  if(length(grids)==0)  {print(paste(stat1,"Not Found")); next}
  grid10<-readOGR(griddir,grids[1])
  grid20<-readOGR(griddir,grids[2])
  
  
  g1<-ggmap(MyMap)+
    geom_point(aes(V1,V2,color=Date),data=mylocations,alpha = .5, color="darkred", size = 3)+
    ggtitle(paste0("Fieldwork Points in ",stat1))+
    geom_polygon(aes(x=long,y=lat,group=id),data=grid10, color ="yellow", fill =NA)+
    geom_polygon(aes(x=long,y=lat,group=id),data=grid20, color ="cyan", fill =NA)
  
  fname<-paste0(InSitu_dir,"09_Visualization/GPS/FPoints_",stat1,".png")
  ggsave(g1,filename = fname )
  
}




