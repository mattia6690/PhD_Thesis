
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")

# 2. Input ----
#* 2.1 Read the MONALISA Data ----

oi<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland")
oi_shape<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland Shapefile")

mnls_lf<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"),full.names = T)
mnls_lf.short<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"))
mnls_stations<-do.call(rbind,strsplit(mnls_lf.short, "\\_|\\-| ")) %>% .[,2]

#* 2.2 Read the Sentinel Data ----

sao_ndvi_lf<-list.files(SAO_NDVIdir,pattern=".tif",full.names = T,recursive = T)
sao_ndvi_lf.short<-list.files(SAO_NDVIdir,recursive=T,pattern=".tif")

#* 2.3 Define Global Input ----

sen2tile<-"T32TPS"
sen2proj<-"LAEA"
NArange <-c(-10000,10000)


# 3. Tidy ----
#* 3.1 Tidy the Table ----

df<-S2_avail(sao_ndvi_lf.short,sen2names) %>% 
  add_column(dir=sao_ndvi_lf)

df<-df %>% 
  expand(df,nesting(mnls_stations)) %>% 
  filter(Tile==sen2tile) %>% 
  filter(Projection==sen2proj)

sao_dates<-df %>%
  filter(AcqDate>2017) %>%
  dplyr::select(dir)

sao_ndvi_lf<- sao_dates %>% 
  as.matrix %>% 
  unlist %>% 
  unique

#* 3.2 Change Projection ----
proj<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
oi2<-spTransform(oi,CRS(proj))
oi2_shape<-spTransform(oi_shape,CRS(proj))
oi2@data$Name<-oi2@data$Name %>% tolower
oi2_shape@data$Name<-oi2_shape@data$Name %>% tolower

#* 3.3 Generate Buffer ----
oi2buff<-gBuffer(oi2,byid=T,width=1500)

# 4. Extract Metrics ----
# The scale for extraction is Sentinel2_MSI

Scale1<-"Sentinel2"
Scale2<-"MSI"
names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")
# Central For Iteration
metricsList<-list()
for(i in 1:length(sao_ndvi_lf)){

  print(paste("Start image",i,"of",length(sao_ndvi_lf)))
  start<-Sys.time()
  
  # Load Subset
  scene<-sao_ndvi_lf[i]
  df_scene<-df %>% filter(dir==scene)
  timestamp<-unique(df_scene$AcqDate)
  
  print(paste("1/5 Initialization FINISHED"))
  #* 4.1  Image ----
  r1<- raster(scene)
  
  OP1<-"Image"
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
    
    combiner<-do.call(cbind,list(timestamp,Tile,Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
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
  
  # The Mask excluded Raster list
  rasters2<-lapply(rasters, function(x){
    check<-is.element(values(x),mvals)
    if(any(check)) x[which(check)]<-NA
    return(x)
  })
  
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
  rasters2<-lapply(rasters, function(x){
    check<-is.element(values(x),mvals)
    if(any(check)) x[which(check)]<-NA
    return(x)
  })
  
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
    pnt[j]<-extract2(r1j,oij)$Mean
    
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

# 5. Tidy Metrics ----

# Create and Save Data Frame
ml<-do.call(rbind,metricsList) %>% as.tibble
saveRDS(ml, paste0(MetricsDir,"Sentinel2_NDVI_metrics.rds"))

# 6. Plot Metrics ----

#* 6.1 GGplots ----
ml<-readRDS(paste0(MetricsDir,"Sentinel2_NDVI_metrics.rds"))

ml.unq<-ml$Station %>% unique
dat.unq<-ml$Date %>% unique

i=ml.unq[1]
for(i in ml.unq){
  
  ml3<-ml %>% filter(Station==i)
  ml3_mask_B<- ml3 %>% filter(OP3=="MakedPercent") %>% filter(OP1=="Buffer"|OP1=="Shapefile")
  ml3_mask_shapeOnly<-ml3_mask_B %>% filter(OP1=="Shapefile") 
  
  ggplot(ml3_mask_B,aes(x=as.Date(Date,format="%Y%m%d"),y=as.numeric(Value)))+
    geom_point()+
    geom_vline(aes(xintercept=as.Date(Date,format="%Y%m%d")),linetype=2,color="grey")+
    geom_point(aes(color=OP1),size=2)+
    geom_line(ml3_mask_shapeOnly,aes(x=as.Date(Date,format="%Y%m%d"),y=as.numeric(Value)))+
    xlab("Date")+ylab("Percentage")+
    ggtitle("Percentage of Masked Pixels ")
    
  
}

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
  


