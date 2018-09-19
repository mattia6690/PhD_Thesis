###########################
#' 00. In Situ Plot 1 Script
#'
#' This script allows plot the outputs of the InSituReader.R script in different ways:
#' * reprentation of the GPS values
#' * 
#' 
###########################
source("R/BaseFunctions.R")
library("ggmap")
library("sf")
library("tibble")
library("lubridate")


### 01. Plots 2 GPS ####
options(digits=10)

dir<-paste0(dirfield,"04_Combined/")
insitu_raw<-read_rds(paste0(dir,"InSituMetrics1_tidy_combined.rds"))

gps.bio<-insitu_raw %>% 
  filter(Scale2=="Biomass") %>% 
  filter(OP2=="Location")

gps.LAI<-insitu_raw %>% 
  filter(Scale2=="LAI2200") %>% 
  filter(OP3=="Lon"|OP3=="Lat") %>% 
  mutate(OP2="Location")

gps.spec<-insitu_raw %>% 
  filter(Scale2=="Spectrometer") %>% 
  filter(OP2=="GPS") %>% 
  mutate(OP2="Location")
  
gps<-rbind(gps.bio,gps.LAI)


stats<-gps$Station %>% unique
griddir<-"C:/Users/MRossi/Documents/08_Temp/04_Shapefile"
lf<-list.files(griddir,pattern=".shp.shp")

gglist1<-gglist2<-gglist3<-gglist4<-list()
gglist1.n<-gglist2.n<-gglist3.n<-gglist4.n<-list()
file<-list()
for(i in stats){
  
  gps1<-gps %>% filter(Station==i) 
  
  gps2<-gps1 %>% as.data.frame %>% 
    spread(key=OP3,value=Value) %>%
    filter(!is.na(Lat)&!is.na(Lon)) %>% 
    sf::st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(4326)
  
  gps3<- gps2 %>% 
    mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
    mutate(DOY=yday(Date)) 
  
  g1<-ggplot(gps3)+geom_sf(aes(color=OP1,pch=Scale2))+facet_wrap(~DOY)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste0(i," GPS coordiantes per Day of Year (DOY) 2017"))
  
  gglist1[[length(gglist1)+1]]<-g1
  gglist1.n[[length(gglist1.n)+1]]<-paste0(i,"_GPS_LAI_Biomass_All.png")
  
  gpsB1<-gps3 %>% 
    group_by(Date,Station,Scale1,Scale2,OP1,DOY) %>% 
    nest() %>% 
    mutate(newcol=map(data,function(j){
      
      box<-j$geometry %>% st_bbox
      mn<-cbind.data.frame(Lat=mean(c(box[2],box[4]),na.rm=T),
                           Lon=mean(c(box[1],box[3]),na.rm=T)) %>% return
      
    }))
  
  gpsB11<-gpsB1 %>% dplyr::select(-data) %>% unnest %>% as.data.frame() %>% 
    sf::st_as_sf(coords = c("Lon","Lat")) %>% 
    st_set_crs(4326) 
  
  g2<-ggplot(gpsB11)+geom_sf(aes(color=OP1,pch=Scale2))+facet_wrap(~DOY)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste0(i," GPS coordiantes per Day of Year (DOY) 2017"))
  
  gglist2[[length(gglist2)+1]]<-g2
  gglist2.n[[length(gglist2.n)+1]]<-paste0(i,"_GPS_LAI_Biomass_Centre.png")
  
  g3<-ggplot(filter(gpsB11,Scale2=="LAI2200"))+geom_sf(aes(color=OP1))+facet_wrap(~DOY)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste0(i," GPS coordiantes per Day of Year (DOY) 2017"))
  
  gglist3[[length(gglist3)+1]]<-g3
  gglist3.n[[length(gglist3.n)+1]]<-paste0(i,"_GPS_LAI_CentreOnly.png")
  
  noLAI<-gps3 %>% 
    dplyr::select(Date,Station,Scale1,Scale2,OP1) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    distinct %>% table
  
  
  # Chose the right Pixel
  
  noLAI<-gps3 %>% 
    dplyr::select(Date,Station,Scale1,Scale2,OP1) %>% 
    as.data.frame() %>% 
    dplyr::select(-geometry) %>% 
    distinct %>% 
    table %>% 
    as.data.frame() %>% 
    group_by(Date,Station,Scale1,OP1) %>% nest
  
  noLAI2<-noLAI %>% mutate(doBio=map(data,function(j){
    
    las<- j %>% filter(Scale2=="LAI2200") %>% dplyr::select(Freq) %>% as.numeric()
    bas<- j %>% filter(Scale2=="Biomass") %>% dplyr::select(Freq) %>% as.numeric()
    
    if(las==1) return(0)
    if(las==0 && bas==0) return(0)
    if(las==0 && bas==1) return(1)
    
  }))
  
  noLAI3<-noLAI2 %>% dplyr::select(-data) %>% unnest %>% filter(doBio==1) %>% dplyr::select(-doBio) %>% 
    split(., seq(nrow(.)))
  
  noLAI4<-map(noLAI3,function(x,y=gpsB11){
    
    pp<-x[k,]
    
    kg<-y %>% 
      filter(Date==as_date(pp$Date)) %>% 
      filter(Scale2=="Biomass") %>% 
      filter(OP1==pp$OP1) 
    
  }) %>% do.call(rbind,.)
    
  
  gpsFilled<-rbind((gpsB11 %>% filter(Scale2=="LAI2200")),noLAI4)
  
  g4<-ggplot(gpsFilled)+geom_sf(aes(color=OP1))+facet_wrap(~DOY)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    ggtitle(paste0(i," GPS coordiantes per Day of Year (DOY) 2017"))
  
  gglist4[[length(gglist4)+1]]<-g4
  gglist4.n[[length(gglist4.n)+1]]<-paste0(i,"_GPS_LAI_FilledGPS.png")
  
  gpsFilledExp<-gpsFilled %>% arrange(Date) %>% dplyr::select(Date,Station,OP1)
  file[[i]]<-gpsFilled
  
  
}

map2(gglist1,gglist1.n,function(x,y) ggsave(x,filename = paste0(dirgps,"01_LAI_Biomass_all/",y),device = "png"))
map2(gglist2,gglist2.n,function(x,y) ggsave(x,filename = paste0(dirgps,"02_LAI_Biomass_centre/",y),device = "png"))
map2(gglist3,gglist3.n,function(x,y) ggsave(x,filename = paste0(dirgps,"03_LAI_only/",y),device = "png"))
map2(gglist4,gglist4.n,function(x,y) ggsave(x,filename = paste0(dirgps,"04_Final_Choice/",y),device = "png"))

file2<-do.call(rbind,file)
st_write(file2,paste0(dirgps,"FilteredGPS.shp"),driver="ESRI Shapefile",delete_layer=T)


# for (i in 1:length(stats)){
#   
#   gps1<-gps %>% filter(Station==stats[i])  %>% as.tibble 
#   stat1<-stats[i]
#   
#   gps2<-gps1 %>% 
#     spread(key=OP3,value=Value) %>%
#     filter(!is.na(Lat)&!is.na(Lon)) %>% 
#     as.data.frame() %>% 
#     sf::st_as_sf(coords = c("Lon","Lat")) %>% 
#     st_set_crs(4326) %>% 
#     mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
#     mutate(DOY=yday(Date))
#   
#   m<-leaflet() %>% addTiles() %>% addMarkers(
#     lng=gps1$Lon %>% as.character %>% as.numeric,
#     lat=gps1$Lat %>% as.character %>% as.numeric,
#     popup=gps1$Stat)
#   
#   ggplot(gps2)+geom_sf(aes(color=OP1))+facet_wrap(~DOY)+
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#     ggtitle(paste0(stat1," GPS coordiantes per Day of Year 2017"))
#     
#   
#   mylocations<-cbind(gps1$Lon %>% unfac, gps1$Lat %>% unfac) %>% as.tibble
#   mylocations[["Date"]]<-gps1$Date
#   ext<-cbind(gps1$Lon %>% unfac, gps1$Lat %>% unfac) %>% extent
# 
#   ifelse(stat1!="Vimes1500",zoom<-19,zoom<-15)
#   MyMap<-get_map(location=mylocations[1,1:2],source="google",maptype="hybrid",zoom=zoom)
#   
#   grids<-lf %>% grep(stat1 %>% toupper,.) %>% lf[.]
#   grids<-substr(grids,1,nchar(grids)-4)
#   if(length(grids)==0)  {print(paste(stat1,"Not Found")); next}
#   grid10<-readOGR(griddir,grids[1])
#   grid20<-readOGR(griddir,grids[2])
#   
#   
#   g1<-ggmap(MyMap)+
#     geom_point(aes(V1,V2,color=Date),data=mylocations,alpha = .5, color="darkred", size = 3)+
#     ggtitle(paste0("Fieldwork Points in ",stat1))+
#     geom_polygon(aes(x=long,y=lat,group=id),data=grid10, color ="yellow", fill =NA)+
#     geom_polygon(aes(x=long,y=lat,group=id),data=grid20, color ="cyan", fill =NA)
#   
#   fname<-paste0(InSitu_dir,"09_Visualization/GPS/FPoints_",stat1,".png")
#   ggsave(g1,filename = fname )
#   
# }
# 
# 
# 
# 
