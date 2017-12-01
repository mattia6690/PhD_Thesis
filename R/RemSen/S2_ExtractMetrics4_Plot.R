# 1. Initialization ----

# Source other Scripts
source("R/RemSen/S2_ExtractMetrics3_Filter.R")


#* 2.2. Cloud Coverage Plots ----
# Per Date and Station

ml<-readRDS(paste0(MetricsDir,"S2_MaskingValues.rds"))
ml.unq<-ml$Station %>% unique %>% .[-1] # Tile Deleted



mluni<-ml %>% select(Station,OP1,Date,Value) %>% unite(.,"Unified",c(Station,OP1),sep="_")
mluni$Unified<-factor(mluni$Unified,levels=rev(unique(mluni$Unified)))
lines<-seq(2.5,mluni$Unified %>% unique %>% length,2)

corplot<-ggplot(mluni, aes(x=factor(Date), y=Unified, fill=Value)) + 
  geom_tile(color = "white")+
  geom_text(aes(as.character(Date), Unified, label = round(Value)))+
  scale_fill_gradient2(low = "green", high = "red", mid = "yellow", 
                       midpoint = 50, limit = c(0,100), space = "Lab", 
                       name="Masked\nPixel") +
  geom_hline(yintercept=lines)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Extent")+xlab("Date")+
  ggtitle("Percent of Masked Pixel per Sentinel-2 Acquisition in 2017")

ggsave(corplot,filename = paste0(sentineldir,"Clouds/S2_Cloud_Detection_Heatmap.png"),device="png",height = 7,width=15)


#* 2.3. NDVI Plots ----

ml<-readRDS(paste0(MetricsDir,"S2_Filtered_NDVI.rds"))


ml1<-ml %>% filter(Station=="vimes1500")

ggplot(ml1,aes(Date,Value/10000,color=OP1))+
  geom_point()+ylim(0,1)+
  ylab("NDVI value")+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))


ggplot(ml6,aes(x=as.Date(as.character(Date),format="%Y%m%d"),y=as.numeric(Value)))+
  geom_point(aes(color=OP1))+
  ylab("NDVI value")+xlab("Date")+ ylim(0,10000)+
  labs(title=paste0("Sentinel 2 NDVI Mean, 2017 in Station ",i))+
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %y"))

ggsave(g1,filename = paste0(sentineldir,"Metrics/S2_NDVI_2017_",i,"_nopoint.png"),device="png",height = 7,width=14)





# 6.1.1 Ggplots With Point
for(i in ml.unq){
  
  ml2<-ml %>% filter(mnls_stations==i)
  ml2$date<-S2_dir2date(ml2$dir.x)
  ml2$nas <-ml2$Original_Nas/ml2$Original_Ncells %>% `/`(100)
  
  g1<-ggplot(ml2,aes(date,Weigted_Mean))+
    geom_point(aes(y=Original_Mean,color="Shape All"))+
    geom_point(aes(color="Shape Weighted"))+
    geom_smooth(aes(color="Shape Weighted"),linetype=3,se=F)+
    geom_point(aes(y=as.numeric(Points),color="Point"))+
    scale_x_date()+
    ylab("NDVI value")+xlab("Date")+ylim(0,10000)+
    labs(title=paste0("Sentinel 2 NDVI Mean, 2017 in Station ",i))+
    scale_colour_manual(breaks=c("Shape All","Shape Weighted","Point"),
                        values=c("red","cyan","blue"))
  
  ggsave(g1,filename = paste0(sentineldir,"Metrics/S2_NDVI_2017_",i,".png"),device="png")
  
}

# 6.1.2 Ggplots Without Points
for(i in ml.unq){
  
  ml2<-ml %>% filter(mnls_stations==i)
  ml2$date<-S2_dir2date(ml2$dir.x)
  ml2$nas <-ml2$Original_Nas/ml2$Original_Ncells %>% `/`(100)
  
  g1<-ggplot(ml2,aes(date,Weigted_Mean))+
    geom_point(aes(y=Original_Mean,color="Shape All"))+
    geom_point(aes(color="Shape Weighted"))+
    geom_smooth(aes(color="Shape Weighted"),linetype=3,se=F)+
    scale_x_date()+
    ylab("NDVI value")+xlab("Date")+ylim(0,10000)+
    labs(title=paste0("Sentinel 2 NDVI Mean, 2017 in Station ",i))+
    scale_colour_manual(breaks=c("Shape All","Shape Weighted"),
                        values=c("cyan","blue"))
  
  ggsave(g1,filename = paste0(sentineldir,"Metrics/S2_NDVI_2017_",i,"_nopoint.png"),device="png",height = 7,width=14)
  
}

#* 6.2 Levelplot alternative ----

lp(r2)

####### Tests ----

# ggproj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# tras<-r1
# tobjP<-oi2[5,]
# tobjS<-oi2_shape[6,]
# ext<-as(extent(tras),"SpatialPolygons")
# crs(ext)<-projection(tras)
# buff<-gBuffer(tobjP,width=1000,quadsegs = 100)
# buff2<-gBuffer(tobjP,width=1500,quadsegs = 100)
# 
# extlatlon<-buff2 %>% 
#   spTransform(.,CRS(ggproj)) %>% 
#   extent %>% 
#   .[c(1,3,2,4)]
# 
# tobjP1<-spTransform(tobjP,CRS(ggproj))
# tobjS1<-spTransform(tobjS,CRS(ggproj))
# buff1<-spTransform(buff,CRS(ggproj))
# tobjP2<-tobjP1 %>% coordinates %>% as.data.frame
# 
# mymap<-get_map(location=extlatlon,source="google",maptype="hybrid")
# ggmap(mymap)+
#   geom_polygon(data=buff1,aes(x=long,y=lat,group=group,color="1,5 Km Buffer"),fill=NA)+
#   geom_polygon(data=tobjS1,aes(x=long,y=lat,group=group,color="Site Extent"),fill=NA)+
#   geom_point(data=tobjP2,aes(x=coords.x1,y=coords.x2,color="MONALISA Station"))+
#   xlab("Longitude")+
#   ylab("Latitude")+
#   scale_color_manual("Legend",values = c("red","orange","yellow"))


# g1<-ggplot(ml,aes(x=Date,y=Value))+
#   geom_point()+
#   geom_vline(aes(xintercept=Date),linetype=2,color="grey")+
#   geom_point(aes(color=OP1),size=2)+
#   xlab("Date")+ylab("Percentage")+
#   ggtitle(paste("Percentage of Masked Pixels in Sentinel2 MSI calculated NDVI maps for",i,"site"))+
#   scale_color_manual("Scale",values=c("dodgerblue2","dodgerblue4","deepskyblue"))+
#   scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))
# 
# ggsave(g1,filename = paste0(sentineldir,"Clouds/CloudCoverage_",i,".png"),device="png",width=10,height=6)
# 
# tile<-ml %>% filter(Station=="Tile")
# 
# ggplot(ml,aes(x=as.Date(Date,format="%Y%m%d")))+
#   geom_bar(data=ml %>% filter(Station=="Tile"),aes(y=Value %>% as.matrix %>% as.numeric),stat = "identity")
