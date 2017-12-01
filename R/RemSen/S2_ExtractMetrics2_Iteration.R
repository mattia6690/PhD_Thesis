
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")
source("R/RemSen/S2_ExtractMetrics1_Initialization.R")

# 2. Extract Metrics ----
# Initialize the Base Information for the Final Data Frame
# The Layout of the Dataframe will then remain the same

Scale1<-"Sentinel2"
Scale2<-"MSI"
names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")
metricsList<-list()

# Start of the Central Script to combine the Metrics
# The extract process will be done separately for (i) the whole Image
for(i in 1:length(sao_ndvi_lf)){

  print(paste("Start image",i,"of",length(sao_ndvi_lf)))
  start<-Sys.time()
  
  # Load Subset
  scene<-sao_ndvi_lf[i]
  df_scene<-df %>% filter(dir==scene)
  timestamp<-unique(df_scene$AcqDate)
  
  r1<- raster(scene)
  
  print(paste("1/5 Initialization FINISHED"))
  
  #* 4.1  Image ----
  OP1<-"Scene"
  OP2<-"Mask"
  OP3<-c("NPixel","Masked","MakedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
  
  mvals<-seq(-26000,-21000,1000)
  v1<-values(r1)
  
  OP_parlist<-list()
  OP_parlist[[1]]<-length(v1)
  OP_parlist[[2]]<-length(which(is.element(v1,mvals)))
  OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
  OP_parlist[[4]]<-which(v1==mvals[1]) %>% length
  OP_parlist[[5]]<-which(v1==mvals[2]) %>% length
  OP_parlist[[6]]<-which(v1==mvals[3]) %>% length
  OP_parlist[[7]]<-which(v1==mvals[4]) %>% length
  OP_parlist[[8]]<-which(v1==mvals[5]) %>% length
  OP_parlist[[9]]<-which(v1==mvals[6]) %>% length
  names(OP_parlist)<-OP3
  
  OP3list<-list()
  for(j in 1:length(OP_parlist)){
    
    combiner<-do.call(cbind,list(timestamp,"Tile",Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
    colnames(combiner)<-names
    OP3list[[j]]<-combiner
  }
  
  imagelist<-do.call(rbind,OP3list)
  rownames(imagelist)<-NULL
  
  #* 4.2 Buffer ----
  print("2/5 Scene FINISHED")

  rasters<- cutrasters(r1,oi2buff)
  
  OP1<-"Buffer"
  OP2<-"Mask"
  OP3<-c("NPixel","Masked","MakedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
  
  mvals<-seq(-26000,-21000,1000)
  
  OP_parlist<-list()
  OP_parlist[[1]]<-lapply(rasters, function(x) ncell(x)) %>% do.call(rbind,.)
  OP_parlist[[2]]<-lapply(rasters, function(x) length(which(is.element(values(x),mvals)))) %>% do.call(rbind,.)
  OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
  OP_parlist[[4]]<-lapply(rasters, function(x) length(which(values(x)==mvals[1]))) %>% do.call(rbind,.)
  OP_parlist[[5]]<-lapply(rasters, function(x) length(which(values(x)==mvals[2]))) %>% do.call(rbind,.)
  OP_parlist[[6]]<-lapply(rasters, function(x) length(which(values(x)==mvals[3]))) %>% do.call(rbind,.)
  OP_parlist[[7]]<-lapply(rasters, function(x) length(which(values(x)==mvals[4]))) %>% do.call(rbind,.)
  OP_parlist[[8]]<-lapply(rasters, function(x) length(which(values(x)==mvals[5]))) %>% do.call(rbind,.)
  OP_parlist[[9]]<-lapply(rasters, function(x) length(which(values(x)==mvals[6]))) %>% do.call(rbind,.)
  names(OP_parlist)<-OP3
  
  OP3list<-list()
  for(j in 1:length(OP_parlist)){
    
    combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
    colnames(combiner)<-names
    OP3list[[j]]<-combiner
  }
  
  bufferlist<-do.call(rbind,OP3list)
  rownames(bufferlist)<-NULL
  
  print("3/5 Buffer Zone FINISHED")
  #* 4.3 Shapefile ----
  
  rasters<- cutrasters(r1,oi2_shape)
  
  OP1<-"Shapefile"
  OP2<-"Mask"
  OP3<-c("NPixel","Masked","MakedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
  
  mvals<-seq(-26000,-21000,1000)
  
  OP_parlist<-list()
  OP_parlist[[1]]<-lapply(rasters, function(x) ncell(x)) %>% do.call(rbind,.)
  OP_parlist[[2]]<-lapply(rasters, function(x) length(which(is.element(values(x),mvals)))) %>% do.call(rbind,.)
  OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
  OP_parlist[[4]]<-lapply(rasters, function(x) length(which(values(x)==mvals[1]))) %>% do.call(rbind,.)
  OP_parlist[[5]]<-lapply(rasters, function(x) length(which(values(x)==mvals[2]))) %>% do.call(rbind,.)
  OP_parlist[[6]]<-lapply(rasters, function(x) length(which(values(x)==mvals[3]))) %>% do.call(rbind,.)
  OP_parlist[[7]]<-lapply(rasters, function(x) length(which(values(x)==mvals[4]))) %>% do.call(rbind,.)
  OP_parlist[[8]]<-lapply(rasters, function(x) length(which(values(x)==mvals[5]))) %>% do.call(rbind,.)
  OP_parlist[[9]]<-lapply(rasters, function(x) length(which(values(x)==mvals[6]))) %>% do.call(rbind,.)
  names(OP_parlist)<-OP3
  
  OP3list<-list()
  for(j in 1:length(OP_parlist)){
    
    combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
    colnames(combiner)<-names
    OP3list[[j]]<-combiner
    
  }
  
  shapelist1<-do.call(rbind,OP3list)
  rownames(shapelist1)<-NULL
  
  # The Mask excluded Raster list
  rasters2<-rasterLmask(rasters,mvals)
  
  OP2<-"NDVI"
  OP3<-c("Mean","Stdev","MoransI")
  
  mean<-array(dim=length(rasters2))
  sd<-array(dim=length(rasters2))
  mi<-array(dim=length(rasters2))
  
  for(j in 1:length(rasters2)){
    
    r1j<-rasters2[[j]]
    rastersname<-names(rasters2)[j]
    oij<-oi2_shape[which(oi2_shape@data$Name==rastersname),]
    mean[j]<-extract2(r1j,oij,narm = T)$Mean %>% round(.,2)
    sd[j]<-extract2(r1j,oij,narm = T)$Stdev %>% round(.,2)
    mi[j]<-Moran(r1j)
  }
  
  OP_parlist<-list(mean,sd,mi)
  names(OP_parlist)<-OP3
  
  OP3list<-list()
  for(j in 1:length(OP_parlist)){
    
    combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
    colnames(combiner)<-names
    OP3list[[j]]<-combiner
    
  }
  
  shapelist2<-do.call(rbind,OP3list)
  rownames(shapelist2)<-NULL
  
  shapelist<-rbind(shapelist1,shapelist2)
  
  print("4/5 Shapefile Zone FINISHED")
  
  #* 4.4 Point ----
  OP1<-"Point"
  OP2<-"NDVI"
  OP3<-"Absolute"
  
  pnt<-array(dim=length(rasters2))
  for(j in 1:length(rasters2)){
    
    r1j<-rasters2[[j]]
    rastersname<-names(rasters2)[j]
    oij<-oi2[which(oi2@data$Name==rastersname),]
    pnt[j]<-extract2(r1j,oij,na.rm=T)$Mean %>% round(.,2)
    
  }
  
  combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3,pnt))
  colnames(combiner)<-names
  
  pointlist<-combiner
  
  
  print("5/5 Point FINISHED")
  
  metricsList[[i]]<-rbind(imagelist,bufferlist,pointlist,shapelist)
  end<-Sys.time()
  diff<-end-start
  print(paste("Raster at Timestep",timestamp,"FINISHED in",diff))
  
}

ml<-do.call(rbind,metricsList) %>% as.tibble
saveRDS(ml, paste0(MetricsDir,"Sentinel2_NDVI_metrics.rds"))


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
ggproj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

tras<-r1
tobjP<-oi2[5,]
tobjS<-oi2_shape[6,]
ext<-as(extent(tras),"SpatialPolygons")
crs(ext)<-projection(tras)
buff<-gBuffer(tobjP,width=1000,quadsegs = 100)
buff2<-gBuffer(tobjP,width=1500,quadsegs = 100)

extlatlon<-buff2 %>% 
  spTransform(.,CRS(ggproj)) %>% 
  extent %>% 
  .[c(1,3,2,4)]

tobjP1<-spTransform(tobjP,CRS(ggproj))
tobjS1<-spTransform(tobjS,CRS(ggproj))
buff1<-spTransform(buff,CRS(ggproj))
tobjP2<-tobjP1 %>% coordinates %>% as.data.frame

mymap<-get_map(location=extlatlon,source="google",maptype="hybrid")
ggmap(mymap)+
  geom_polygon(data=buff1,aes(x=long,y=lat,group=group,color="1,5 Km Buffer"),fill=NA)+
  geom_polygon(data=tobjS1,aes(x=long,y=lat,group=group,color="Site Extent"),fill=NA)+
  geom_point(data=tobjP2,aes(x=coords.x1,y=coords.x2,color="MONALISA Station"))+
  xlab("Longitude")+
  ylab("Latitude")+
  scale_color_manual("Legend",values = c("red","orange","yellow"))
  


