
source("C:/Users/MRossi/Documents/07_Codes/PhD_Thesis/R/BaseFunctions.R")
loadandinstall("ggmap")
loadandinstall("ggsn")
loadandinstall("broom")
loadandinstall("sf")
loadandinstall("purrr")

stations<-st_read(paste0(WorkspaceDir,"/01_Data/KML"),"Mattia_Stations_PhD")[,1] %>% filter(Name!="P2")
stations<-stations %>% 
  mutate(Region=map(Name,function(x){
    
    if(grepl("Vi",x)) return("Vinschgau")
    if(grepl("Do",x)) return("Dolomites")
    
  })) %>% 
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),lat=map_dbl(geometry, ~st_centroid(.x)[[2]]))



ST<-readOGR(paste0(DataDir,"/Shapes/00_General"),"SouthTyrol") %>% st_as_sf()
ITA<-readOGR(paste0(DataDir,"/Shapes/00_General"),"ITA_adm0") %>% st_as_sf()


extvi<-stations %>% filter(Region=="Vinschgau") %>% st_bbox %>% st_as_sfc %>% st_buffer(dist=.1)
extdo<-stations %>% filter(Region=="Dolomites") %>% st_bbox %>% st_as_sfc %>% st_buffer(dist=.1)

extST1<-ST %>% extent %>% as.matrix %>% as.numeric
extST2<-ST %>% st_buffer(dist=.5) %>% extent %>% as.matrix %>% as.numeric

mymap<-get_map(location=extST2,source="google",maptype="satellite")
coords<-cbind(stations@coords[,1],stations@coords[,2]) %>% as.data.frame()

# Plot the Study Site with the Location Bouundaries divided in Dolomites and Vinschgau Valley
g1<-ggmap(mymap)+
  geom_sf(data=ST,inherit.aes = FALSE,fill="darkolivegreen3",color="darkolivegreen2",size=1.5,alpha=.2)+
  geom_sf(data=stations,inherit.aes = F,cex=4,pch=17)+
  geom_sf(data=extvi,inherit.aes = F,fill=NA,size=1.2,color="red")+
  geom_sf(data=extdo,inherit.aes = F,fill=NA,size=1.2,color="deepskyblue")+
  geom_label(data=stations, aes(x=lon, y=lat, label=Name),hjust = 0, nudge_x = 0.05,size=10) +
  xlab("Longitude")+ylab("Latitude")+
  coord_sf(xlim=c(extST1[1],extST1[3]),ylim=c(extST1[2],extST1[4]))+
  theme(legend.position="none")
  
ggsave(g1,filename = paste0(DataDir,"Images/StudySites_gg.png"),device = "png",height=9,width=16)

eurac.color<-rgb(208,77,31,maxColorValue = 255)
plot(ITA,border="mediumseagreen")
plot(ST,add=TRUE,col=eurac.color,border="mediumseagreen")

# Italy and South Tyrol Plot
g2<-ggplot()+theme_light()+
  geom_sf(data=ITA,inherit.aes = F)+
  geom_sf(data=ST,inherit.aes = F,fill="darkolivegreen2")+
  geom_sf(data=extvi,inherit.aes = F,fill=NA,size=1.2,color="red")+
  geom_sf(data=extdo,inherit.aes = F,fill=NA,size=1.2,color="deepskyblue")

ggsave(g2,filename = paste0(DataDir,"Images/ItalyST_gg.png"),device = "png",height=12,width=8)


# Smoothed generic Plot

g1<- ggplot()+ theme_bw()+
  geom_sf(data=ST,inherit.aes = FALSE,fill="darkolivegreen3",color="darkolivegreen2",size=1.5,alpha=.1)+
  geom_sf(data=stations,inherit.aes = F,cex=4,pch=17)+
  geom_sf(data=extvi,inherit.aes = F,fill=NA,size=1.2,color="red")+
  geom_sf(data=extdo,inherit.aes = F,fill=NA,size=1.2,color="deepskyblue")+
  geom_text(data=stations, aes(x=lon, y=lat, label=Name),hjust = .6, nudge_y = 0.05,size=6, position = ) +
  xlab("Longitude")+ylab("Latitude")+
  theme(legend.position="none")+
  annotation_scale(location = "bl", width_hint = 0.3)+
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering)

ggsave(g1,filename = paste0(DataDir,"Images/StudySites_generic.png"),device = "png",height=7,width=12)



# Get Station Coordinates
loadandinstall("measurements")
stations<-st_read(paste0(WorkspaceDir,"/01_Data/KML"),"Mattia_Stations_PhD")[,1] %>% filter(Name!="P2")
st_geometry(stations) %>% unlist %>% matrix(.,ncol=3,byrow=T)
