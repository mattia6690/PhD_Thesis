
source("C:/Users/MRossi/Documents/07_Codes/PhD_Thesis/R/BaseFunctions.R")
laodandinstall("ggmap")
loadandinstall("ggsn")
loadandinstall("broom")

stations<-readOGR(paste0(WorkspaceDir,"/01_Data/KML"),"Mattia_Stations_PhD")
ST<-readOGR(paste0(DataDir,"/Shapes/00_General"),"SouthTyrol")
ITA<-readOGR(paste0(DataDir,"/Shapes/00_General"),"ITA_adm0")

ST2<-ST %>%broom::tidy()
ITA2<-ITA %>% broom::tidy()

stations_vi<-stations[c(1,4,5),]
stations_do<-stations[c(2,3),]

extall<-c(10,46,13,47.5)

extvi<-extent(stations_vi)
extvi[c(1,3)]<-extvi[c(1,3)]-.05
extvi[c(2,4)]<-extvi[c(2,4)]+.05
extvi<- as(extvi,"SpatialPolygons")
crs(extvi)<-projection(stations_vi)

extdo<-extent(stations_do)
extdo[c(1,3)]<-extdo[c(1,3)]-.05
extdo[c(2,4)]<-extdo[c(2,4)]+.05
extdo<- as(extdo,"SpatialPolygons")
crs(extdo)<-projection(stations_vi)

extST<-extent(ST) %>% as.matrix %>% as.numeric


mymap<-get_map(location=extall,source="google",maptype="satellite")
coords<-cbind(stations@coords[,1],stations@coords[,2]) %>% as.data.frame()

# Plot the Study Site with the Location Bouundaries divided in Dolomites and Vinschgau Valley
g1<-ggmap(mymap)+
  geom_polygon(data=ST2,aes(x=long,y=lat,group=group),fill="gray54",color="gray54",size=1.5,alpha=.3)+
  geom_polygon(aes(x=long,y=lat,group=id,color="Vinschgau"),data=extvi,fill=NA,size=1.2)+
  geom_polygon(aes(x=long,y=lat,group=id,color="Dolomites"),data=extdo,fill=NA,size=1.2)+
  geom_point(aes(x=V1,y=V2,color="Vinschgau"),data=coords[c(1,4,5),],size=2)+
  geom_point(aes(x=V1,y=V2,color="Dolomites"),data=coords[c(2,3),],size=2)+
  xlab("Longitude")+ylab("Latitude")+
  scale_color_manual("Site Location",values=c("green","red"))+
  coord_map(xlim=c(extST[1],extST[3]),ylim=c(extST[2],extST[4]))

ggsave(g1,filename = paste0(DataDir,"Images/StudySites.png"),device = "png",height=8,width=11)

eurac.color<-rgb(208,77,31,maxColorValue = 255)
plot(ITA,border="mediumseagreen")
plot(ST,add=TRUE,col=eurac.color,border="mediumseagreen")

# Italy and South Tyrol Plot
g2<-ggplot()+theme_minimal()+
  geom_polygon(data=ITA2,aes(x=long,y=lat,group=group),fill="lightgoldenrod")+
  geom_polygon(data=ST2,aes(x=long,y=lat,group=group),fill="lightcoral")+
  xlab("Longitude")+ylab("Latitude")

ggsave(g2,filename = paste0(DataDir,"Images/ItalyST.png"),device = "png",height=6,width=5)
               
