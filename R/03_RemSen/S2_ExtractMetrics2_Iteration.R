
# 1. Initialization ----

# Source other Scripts
#source("R/RemSen/S2_ExtractMetrics1_Initialization.R")
writeCSV=T

# 2. Extract Metrics ----
# Initialize the Base Information for the Final Data Frame
# The Layout of the Dataframe will then remain the same

Scale1<-"Sentinel2"
Scale2<-"MSI"
names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")
year<-"2017"
metricsList<-list()
tiles<-array(dim=length(sao_ndvi_lf))

suffix<-"140518"

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
  tiles[i]<-df_scene$Tile
  
  
  
  rext<-as(extent(r1),"SpatialPolygons")
  projection(rext)<-projection(r1)
  
  wh<-which(gIntersects(oi2_shape,rext,byid=T))
  
  if(length(wh)>0){
    
    print(paste("1/5 Initialization FINISHED"))
    oi3_shape<-oi2_shape[wh,]
    
    OP1<-"Scene"
    OP2<-"Mask"
    OP3<-c("NPixel","Masked","MaskedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
    
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
      
      combiner<-do.call(cbind,list(timestamp,tile,Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
      colnames(combiner)<-names
      OP3list[[j]]<-combiner
    }
    
    imagelist<-do.call(rbind,OP3list)
    rownames(imagelist)<-NULL
    
    #* 2.1 Buffer ----
    print("2/5 Scene FINISHED")
    
    rasters.buff<- cutrasters(r1,oi2buff)
    
    OP1<-"Buffer"
    OP2<-"Mask"
    OP3<-c("NPixel","Masked","MaskedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
    
    mvals<-seq(-26000,-21000,1000)
    
    OP_parlist<-list()
    OP_parlist[[1]]<-lapply(rasters.buff, function(x) ncell(x)) %>% do.call(rbind,.)
    OP_parlist[[2]]<-lapply(rasters.buff, function(x) length(which(is.element(values(x),mvals)))) %>% do.call(rbind,.)
    OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
    OP_parlist[[4]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[1]))) %>% do.call(rbind,.)
    OP_parlist[[5]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[2]))) %>% do.call(rbind,.)
    OP_parlist[[6]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[3]))) %>% do.call(rbind,.)
    OP_parlist[[7]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[4]))) %>% do.call(rbind,.)
    OP_parlist[[8]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[5]))) %>% do.call(rbind,.)
    OP_parlist[[9]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[6]))) %>% do.call(rbind,.)
    names(OP_parlist)<-OP3
    
    OP3list<-list()
    for(j in 1:length(OP_parlist)){
      
      combiner<-do.call(cbind,list(timestamp,names(rasters.buff),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
      colnames(combiner)<-names
      OP3list[[j]]<-combiner
    }
    
    bufferlist<-do.call(rbind,OP3list) %>% as.tibble
    rownames(bufferlist)<-NULL
    
    print("3/5 Buffer Zone FINISHED")
    #* 2.2 Shapefile ----
    
    rasters<- cutrasters(r1,oi3_shape)
    
    OP1<-"Shapefile"
    OP2<-"Mask"
    OP3<-c("NPixel","Masked","MaskedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
    
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
      allstat<-all(is.na(values(r1j)))
      
      if(!isTRUE(allstat)){
        rastersname<-names(rasters2)[j]
        oij<-oi3_shape[which(oi3_shape@data$Name==rastersname),]
        mean[j]<-Mfunctions::extract2(r1j,oij,narm = T)$Mean %>% round(.,2)
        sd[j]<-Mfunctions::extract2(r1j,oij,narm = T)$Stdev %>% round(.,2)
        mi[j]<-Moran(r1j)
      }
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
    
    #* 2.3 Point ----
    #** 2.3.1 Monalisa ----
    
    OP1<-"Point"
    OP2<-"NDVI"
    OP3<-"Absolute"
    
    pnt<-map2(rasters2,names(rasters2),function(x,y,z=oi2){
      
      oij<-z[which(z@data$Name==y),]
      pnt<-Mfunctions::extract2(x,oij,narm=T)$Mean %>% round(.,2)
      return(pnt)
      
    }) %>% unlist
    
    combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3,pnt))
    colnames(combiner)<-names
    
    pointlist<-combiner
    
    
    #** 4.4.2 Subplots ----
    
    # P2 is filtered out since I still have to define the boundaries of the Station
    
    gpsdata2<-gpsdata %>% filter(Station!="P2")
    
    arr<-array(dim=nrow(gpsdata2))
    for(k in 1:nrow(gpsdata2)){
      
      x<-gpsdata2[k,]
      station<- x$Station
      t1<- x$Date
      t2<- timestamp %>% as.Date(.,format="%Y%m%d")
      tdiff<-t1-t2
      
      if(tdiff<5){
        
        g1<-spTransform(g<-as(x,"Spatial"),projection(rasters.buff[[1]]))
        rrnew<-rasters.buff[[which(names(rasters.buff)==tolower(station))]]
        Mean<-Mfunctions::extract2(rrnew,g1,narm=T)$Mean
        arr[k]<-Mean
        
      }
    }
    
    wh1<-which(!is.na(arr))
    
    if(length(wh1>0)) gpsdata2$Value[wh1]<-arr[wh1]
    
    idlist<-gpsdata2 %>% as.data.frame() %>% select(-c(geometry,DOY)) %>% filter(!is.na(Value))
    print("5/5 Points FINISHED")
    
    metricsList[[i]]<-rbind(imagelist,bufferlist,pointlist,shapelist,idlist)
    print(paste("Raster at Timestep",timestamp,"FINISHED in",diff))
    
  } else {print("No Shapefile lies within this extent")}
  
  end<-Sys.time()
  diff<-end-start
}

ml<-do.call(rbind,metricsList) %>% as.tibble
saveRDS(ml, paste0(MetricsDir,"Sentinel2_metrics_",suffix,".rds"))

if(writeCSV==T) write.csv(ml, paste0(MetricsDir,"Sentinel2_metrics_",suffix,".csv"))



# subtab<-gpsdata %>% as.data.frame() %>% group_by(Date,Station) %>% filter(Station!="P2")
# subtab %>% mutate(newfield=map2(data,Station,function(x,xx,y=gpsdata,rr=rasters.buff){
#   
#   print(xx)
#   station<-xx
#   t1<- x$Date
#   t2<- timestamp %>% as.Date(.,format="%Y%m%d")
#   tdiff<-t1-t2
#   mindiff<-min(tdiff)
#   mindiff.pos<-which(tdiff==mindiff)
#   
#   if(mindiff<5){
#     
#     gps1<-gpsdata[mindiff.pos,]
#     gps1.t<-st_transform(gps1,projection(rr[[1]]))
#     
#     g<-as(gps1,"Spatial")
#     
#     g1<-spTransform(g,projection(rr[[1]]))
#     Mfunctions::extract2(rr[[6]],g1,narm=T)
#     
#     rrnew<-rr[[which(names(rr)==tolower(xx))]]
#     Mean<-Mfunctions::extract2(rrnew,g1,narm=T)$Mean
#     
#     return(Mean)
#   }
# }))
