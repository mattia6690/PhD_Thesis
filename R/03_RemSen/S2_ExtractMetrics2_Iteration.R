
# 1. Initialization ----

# Source other Scripts
#source("R/RemSen/S2_ExtractMetrics1_Initialization.R")
writeCSV=T
doscene<-F
dobuffer<-F
# 2. Extract Metrics ----
# Initialize the Base Information for the Final Data Frame
# The Layout of the Dataframe will then remain the same

Scale1<-s1<-"Sentinel2"
Scale2<-s2<-"MSI"
year<-"2017"
suffix<-"230618"
mvals<-seq(-26000,-21000,1000)

gdal.trsl<-'"C:/Program Files/GDAL/bin/gdal/apps/gdal_translate.exe"'

# Start of the Central Script to combine the Metrics
# The extract process will be done separately for (i) the whole Image
for(i in 1:nrow(df)){
  
  #* Initialize For Loop ----
  print(paste("Start image",i,"of",nrow(df)))
  if(i==1) metricsList<-pointlist.id<-list()
  start<-Sys.time()
  bind.df<-bind.pntID<-data.frame()
  
  #* Select from df ----
  row<-df[i,]
  scene<-row$dir
  timestamp<-row$AcqDate
  tile<-row$Tile
  
  r1<- raster(scene)
  wi<-st_within(oi2_shape,st_as_sfc(st_bbox(r1))) %>% unlist
  if(!any(wi)) {print("No Shapefile lies within this raster");next}
  
  print(paste("1/5 Initialization FINISHED"))
  
  #* Scene Statistics ----
  if(doscene){
    
    v1<-values(r1)
    
    imagedat<-oi2_buffer %>% 
      as_data_frame %>% 
      select(-c(Station,OP2,OP3,geometry)) %>% distinct %>% 
      add_column(Date=as.Date(timestamp,format="%Y%m%d"),.before=T) %>% 
      mutate(OP1="Image")
    
    imagemask<-map(v1, function(x,m=mvals){
      
      NPixel<- ncell(x)
      Masked<- length(which(is.element(x,m)))
      MaskedPercent<-round((Masked/NPixel)*100,2)
      NoData<- length(which(x==m[1]))
      LandCover<- length(which(x==m[2]))
      Clouds<- length(which(x==m[3]))
      ROR<- length(which(x==m[4]))
      Shadow<- length(which(x==m[5]))
      Snow<- length(which(x==m[6]))
      
      return(cbind(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow))
      
    }) %>% do.call(rbind,.)
    
    imagelist<- imagedat %>% 
      cbind(.,imagemask) %>% 
      add_column(OP2="Mask",.after="OP1") %>% 
      gather(.,key=OP3,value=Value,colnames(imagemask))
    
    bind.df<-rbind(bind.df,imagelist)
    
  }
  
  print("2/5 Scene FINISHED")
  
  #* Buffer Statistics ----
  
  gdal.trsl<-'"C:/Program Files/GDAL/bin/gdal/apps/gdal_translate.exe"'
  timestamp2<-timestamp %>% as.Date(.,format="%Y%m%d")
  
  ns2<-ns %>% 
    mutate(temp =map (data, function(i) tempfile(fileext = ".tif"))) %>% 
    mutate(bbox =map (data, function(i) st_bbox(i) %>% .[order(c(1,4,3,2))] %>% paste(collapse=" "))) %>% 
    mutate(gdal =map2(bbox,temp,function(i,j,r=scene,g=gdal.trsl) paste(g,"-projwin",i,r,j,"-q"))) %>% 
    mutate(gcal =map (gdal,function(i) system(i))) %>% 
    mutate(rast =map (temp,function(i) raster(i))) %>% 
    select(-gcal)
  
  
  i=ns2$rast[[3]]
  j=ns2$data[[3]]
  
  ns3<-ns2 %>% 
    mutate(extract=map2(rast,data,function(i,j){
      
      pt<-j[st_is(j,"POINT"),]
      sh<-j[st_is(j,"POLYGON"),]
      
      e1<-raster::extract(i,as_Spatial(pt)) %>% as.list()
      if(nrow(sh)>0) {
        
        e2<-raster::extract(i,as_Spatial(sh))
        ll<-c(e1,e2)
        
      } else { ll<-e1 }
      
      names(ll)<-c(1:length(ll))
      lst<-melt(ll)[order(c(2,1))]
      return(lst)
      
    }))
  
  i=ns3$extract[[1]]
  x<-ii$data[[25]]
  ns4<-ns3 %>% mutate(Metrics=map(extract,function(i,m=mvals) {
    
    # Create List by extracted Values for each Polygon of each Station
    ii<-i %>% group_by(L1) %>% nest
    ii2<-ii %>% mutate(mask=map(data,function(x,m=mvals){
      
      x<-unlist(x)
      NPixel<- length(x)
      Masked<- length(which(is.element(x,m)))
      MaskedPercent<-round((Masked/NPixel)*100,2)
      NoData<- length(which(x==m[1]))
      LandCover<- length(which(x==m[2]))
      Clouds<- length(which(x==m[3]))
      ROR<- length(which(x==m[4]))
      Shadow<- length(which(x==m[5]))
      Snow<- length(which(x==m[6]))
      
      x[x<0]<-NA
      y<-na.omit(x)
      if(length(x)>1){
        
        Absolute<-NA
        Mean<-ifelse(length(y)>3,round(mean(y,na.rm=T),2),NA)
        Sd  <-ifelse(length(y)>3,round(sd(y,na.rm=T),2),NA)
        stat<-cbind(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow,Absolute,Mean,Sd)
        
      }else{
        
        Absolute<-ifelse(length(y)>0,x,NA)
        Mean <- NA
        Sd   <- NA
        stat<-cbind(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow,Absolute,Mean,Sd)
        
      }
    }))
    
    Metrics<-ii2$mask %>% do.call(rbind,.)
    return(Metrics)
    
  }))
  
  ns5<-ns4 %>% 
    mutate(Date2=map(data,function(i) bind_cols(timestamp2,(i$Date-timestamp))) %>% 
    mutate(Tdiff=map2(data,Date2,function(i,j) as.numeric(i$Date-j)))
  
  w<-ns5$data[[3]]
  x<-ns5$Metrics[[3]]
  y<-ns5$Tdiff[[3]]
  
  
  ns6<-ns5 %>%
    mutate(result=pmap(list(data,Metrics,Tdiff),function(w,x,y){
      
      i1<- as.data.frame(w) %>% select(-c(OP3,OP2,geometry)) 
      j1<- x %>% as.tibble %>% select(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow) 
      j2<- x %>% as.tibble %>% select(Absolute,Mean,Sd)
      k1<- as.tibble(data.frame(Value=y))
      
      print(i1[1,])
      
      cb1<-cbind(i1,j1) %>% 
        gather(.,key=OP3,value=Value,colnames(j1)) %>% 
        add_column(OP2="Mask",.before="OP3")
      
      cb2<-cbind(i1,j2) %>% 
        gather(.,key=OP3,value=Value,colnames(j2)) %>% 
        add_column(OP2="NDVI",.before="OP3") 
      
      cb3<-cbind(i1,k1) %>% 
        add_column(OP2="Time",.after="OP1") %>% 
        add_column(OP3="Difference",.after="OP2") 
      
      return(rbind(cb1,cb2,cb3))
      
    }))
  
  ns7<-ns6 %>% select(Station,result) %>% unnest
  
  

  
  rasterlist<-list()
  for(j in 1:nrow(oi2_buffer)){
    
    station<-oi2_buffer[j,]$Station %>% as.character
    file.c<-oi2_buffer[j,] %>% st_bbox
    
    tmpfile<-tempfile(fileext = ".tif")
    command<-paste(gdal.trsl,"-projwin",file.c[1],file.c[4],file.c[3],file.c[2],scene,tmpfile,"-q")
    system(command)
    
    r<-raster(tmpfile)
    names(r)<-station
    rasterlist[[j]]<-r
    
  }
  
  rasternames<-sapply(rasterlist,names)
  oi2_buffer
  
  
  # Sort the Shapefile accor
  
  if(dobuffer){
    
    rasters.bff<- cutrasters(r1,oi2_buffer) %>% 
      map(.,function(i){
        if(length(i)>1) raster::values(i) else NA
        })
    
    bufferdat<-oi2_buffer %>% 
      as_data_frame %>% 
      select(-c(OP2,OP3,geometry))%>% 
      add_column(Date=as.Date(timestamp,format="%Y%m%d"),.before=T)
    
    buffermask<-map(rasters.bff, function(x,m=mvals){
      
      NPixel<- ncell(x)
      Masked<- length(which(is.element(x,m)))
      MaskedPercent<-round((Masked/NPixel)*100,2)
      NoData<- length(which(x==m[1]))
      LandCover<- length(which(x==m[2]))
      Clouds<- length(which(x==m[3]))
      ROR<- length(which(x==m[4]))
      Shadow<- length(which(x==m[5]))
      Snow<- length(which(x==m[6]))
      
      
      
      
      return(cbind(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow))
      
    }) %>% do.call(rbind,.)
    
    bufferlist<- bufferdat %>% 
      cbind(.,buffermask) %>% 
      add_column(OP2="Mask",.after="OP1") %>% 
      filter(NPixel>1) %>% 
      gather(.,key=OP3,value=Value,colnames(buffermask))
    
    bind.df<-rbind(bind.df,bufferlist)
    
  }
  
  print("3/5 Buffer Zone FINISHED")
  
  #* Shapefile Statistics ----
  
  # Extract Values from the Rasters
  rasters.shp<-purrr::map(rasterlist,function(p,oi=oi2_shape) {
    
    o<-oi %>% filter(Station==names(p)) %>% as_Spatial
    ex<-raster::extract(p,o)
    
    }) %>% unlist(recursive = F)
  
  shapedat<-oi2_shape %>% 
    as_data_frame %>% 
    mutate(Station=sapply(rasterlist,names)) %>% 
    select(-c(OP2,OP3,geometry))%>% 
    add_column(Date=as.Date(timestamp,format="%Y%m%d"),.before=T)
  
  # Calculate the Mask occurences
  shpmask<-map(rasters.shp, function(x,m=mvals){
    
    NPixel<- ncell(x)
    Masked<- length(which(is.element(x,m)))
    MaskedPercent<-round((Masked/NPixel)*100,2)
    NoData<- length(which(x==m[1]))
    LandCover<- length(which(x==m[2]))
    Clouds<- length(which(x==m[3]))
    ROR<- length(which(x==m[4]))
    Shadow<- length(which(x==m[5]))
    Snow<- length(which(x==m[6]))
    
    return(cbind(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow))
    
  }) %>% do.call(rbind,.)
  
  shapelist1<- shapedat %>% 
    cbind(.,shpmask) %>% 
    add_column(OP2="Mask",.after="OP1") %>% 
    filter(NPixel>0) %>% 
    gather(.,key=OP3,value=Value,colnames(shpmask))
  
  shapendvi<-map(rasters.shp, function(x,m=mvals){
    
    x[x<0]<-NA
    y<-na.omit(x)
    Mean<-ifelse(length(y)>3,round(mean(y,na.rm=T),2),NA)
    Sd  <-ifelse(length(y)>3,round(sd(y,na.rm=T),2),NA)
    
    return(cbind(Mean,Sd))
    
  }) %>% do.call(rbind,.)
  
  shapelist2<- shapedat  %>% 
    cbind(.,shapendvi) %>% 
    add_column(OP2="NDVI",.after="OP1") %>% 
    gather(.,key=OP3,value=Value,colnames(shapendvi)) %>% 
    filter(!is.na(Value))
  
  shapelist<-rbind(shapelist1,shapelist2)
  bind.df<-rbind(bind.df,shapelist)
  
  print("4/5 Shapefile Zone FINISHED")
  
  #* Point Statistics ----
  
  timestamp2<-timestamp %>% as.Date(.,format="%Y%m%d")
  
  
  ex.p.1<-purrr::map(rasterlist,function(p,oi=oi2) {
    
    o<-oi %>% filter(Station==names(p)) %>% as_Spatial
    ex<-raster::extract(p,o)
    
  }) %>% unlist(recursive = F)
  
  
  ex.p.1<-rasters.pnt<-raster::extract(r1,oi2)
  ex.p.2<-rasters.pnt<-raster::extract(r1,gpsdata)
  
  pointdat.1<-oi2 %>% 
    as_data_frame %>% 
    select(-geometry) %>% 
    add_column(Date=timestamp2,.before=T) %>% 
    add_column(Value=as.numeric(ex.p.1)) %>% 
    filter(Value>-1000 & !is.na(Value))
  
  bind.df<-rbind(bind.df,pointdat.1)
  metricsList[[i]]  <- bind.df
  
  Tdiff<-as.numeric(difftime(gpsdata$Date,timestamp2))
  pointdat.2<-gpsdata %>% 
    as_data_frame %>% 
    select(-geometry) %>% 
    add_column(Value=as.numeric(ex.p.2)) %>% 
    add_column(Tdiff=Tdiff) %>% 
    filter(Tdiff>(-14) & Tdiff<14) %>% 
    filter(Value>-1000 & !is.na(Value))
  
  pointlist.id[[i]] <- pointdat.2

  end<-Sys.time()
  diff<-end-start
  print(paste("Raster at Timestep",timestamp,tile,"FINISHED in",diff))
  
}

ml<-do.call(rbind,metricsList) %>% as.tibble
saveRDS(ml, paste0(MetricsDir,"Sentinel2_metrics_",suffix,".rds"))

ml.p<-do.call(rbind,pointlist.id) %>% as.tibble
saveRDS(ml.p, paste0(MetricsDir,"Sentinel2_metrics_points_",suffix,".rds"))


# 
# OP1<-"Point"
# OP2<-"NDVI"
# OP3<-"Absolute"
# 
# pnt<-map2(rasters.buff,names(rasters.buff),function(x,y,z=oi2){
#   
#   oij<-z %>% filter(Name==y) %>% as(.,"Spatial")
#   pnt<-Mfunctions::extract2(x,oij,narm=T)$Mean %>% round(.,2)
#   return(pnt)
#   
# }) %>% unlist
# 
# combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3,pnt))
# rownames(shapelist2)<-NULL
# colnames(combiner)<-names
# 
# pointlist<- combiner %>% as_tibble %>% mutate(Date=as_date(Date)) %>% mutate(Value=as.numeric(Value))
# bind.df<-rbind(bind.df,pointlist)
# 

# # P2 is filtered out since I still have to define the boundaries of the Station
# 
# gpsdata2<-gpsdata.utm %>% filter(Station!="P2")
# arr<-diff<-array(dim=nrow(gpsdata2))
# 
# for(k in 1:nrow(gpsdata2)){
#   
#   x<-gpsdata2[k,]
#   station<- x$Station
#   t1<- x$Date
#   t2<- as.Date(timestamp,format="%Y%m%d")
#   tdiff<-t1-t2
#   diff[k]<-unique(tdiff)
#   
#   if(tdiff<14 & any(names(rasters.buff)==station)){
#     
#     rrnew<-rasters.buff[[station]]
#     arr[k]<-raster::extract(rrnew,as_Spatial(x))
#     
#   }
# }
# 
# wh1<-which(!is.na(arr))
# 
# if(length(wh1>0)) {
#   
#   gpsdata2$Value<-arr
#   gpsdata2$TDiff<-diff
#   data<-gpsdata2 %>% 
#     as.data.frame() %>% 
#     select(-c(geometry,DOY)) %>% 
#     filter(!is.na(Value)) %>% 
#     as_tibble 
#   pointlist.id[[i]]<- data %>% mutate(Scale1=s1) %>% mutate(Scale2=s2)
#   
# }
# 
# metricsList[[i]]<-bind.df
# print("5/5 Points FINISHED")
# 
# end<-Sys.time()
# diff<-end-start
# print(paste("Raster at Timestep",timestamp,tile,"FINISHED in",diff))
# 
# if(i==nrow(df)){
#   
#   ml1<-do.call(rbind,metricsList) %>% as.tibble
#   ml2<-pointlist.id %>% .[lapply(.,length)>1] %>% do.call(rbind,.) %>% as.tibble %>% filter(Value>0)
# }



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
# 
# 
#' Crop by distance
# 
# box<-st_buffer(x,dist = 30) %>% st_as_sf %>% as_Spatial()
# rrnew<-crop(rrnew1,box)
# 
# 
# ptr<-rasterToPolygons(rrnew) %>% st_as_sf() 
# ptr.sf<- ptr %>% setNames(c("Values","geometry"))
# ptr.sf$distance<-st_distance(x,ptr.sf) %>% as.numeric
# 
# Value<-arrange(ptr.sf,distance) %>% 
#   filter(!is.na(Values)) %>% 
#   as.data.frame() %>% 
#   .[1,1]
# 
# arr[k]<-Value

# 
# 
# 
# # The Mask excluded Raster list
# rasters2<-rasterLmask(rasters.buff,mvals)
# 
# OP2<-"NDVI"
# OP3<-c("Mean","Stdev","MoransI")
# 
# mean<-array(dim=length(rasters2))
# sd<-array(dim=length(rasters2))
# mi<-array(dim=length(rasters2))
# 
# for(j in 1:length(rasters2)){
#   
#   r1j<-rasters2[[j]]
#   allstat<-all(is.na(values(r1j)))
#   
#   if(!isTRUE(allstat)){
#     rastersname<-names(rasters2)[j]
#     oij<-oi2_shape %>% filter(Name==rastersname) %>% as(.,"Spatial")
#     mean[j]<-Mfunctions::extract2(r1j,oij,narm = T)$Mean %>% round(.,2)
#     sd[j]<-Mfunctions::extract2(r1j,oij,narm = T)$Stdev %>% round(.,2)
#     mi[j]<-Moran(r1j)
#   }
# }
# 
# OP_parlist<-list(mean,sd,mi)
# names(OP_parlist)<-OP3
# 
# OP3list<-list()
# for(j in 1:length(OP_parlist)){
#   
#   combiner<-do.call(cbind,list(timestamp,names(rasters),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
#   colnames(combiner)<-names
#   OP3list[[j]]<-combiner
#   
# }
# 
# shapelist2<-do.call(rbind,OP3list) %>% as_tibble %>% mutate(Date=as_date(Date)) %>% mutate(Value=as.numeric(Value))
# rownames(shapelist2)<-NULL
# 
# shapelist<-rbind(shapelist1,shapelist2)
# 
# bind.df<-rbind(bind.df,shapelist)
# 
# print("4/5 Shapefile Zone FINISHED")

# 
# 
# OP1<-"Buffer"
# OP2<-"Mask"
# OP3<-c("NPixel","Masked","MaskedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
# 
# mvals<-seq(-26000,-21000,1000)
# 
# OP_parlist<-list()
# OP_parlist[[1]]<-lapply(rasters.buff, function(x) ncell(x)) %>% do.call(rbind,.)
# OP_parlist[[2]]<-lapply(rasters.buff, function(x) length(which(is.element(values(x),mvals)))) %>% do.call(rbind,.)
# OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
# OP_parlist[[4]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[1]))) %>% do.call(rbind,.)
# OP_parlist[[5]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[2]))) %>% do.call(rbind,.)
# OP_parlist[[6]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[3]))) %>% do.call(rbind,.)
# OP_parlist[[7]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[4]))) %>% do.call(rbind,.)
# OP_parlist[[8]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[5]))) %>% do.call(rbind,.)
# OP_parlist[[9]]<-lapply(rasters.buff, function(x) length(which(values(x)==mvals[6]))) %>% do.call(rbind,.)
# names(OP_parlist)<-OP3
# 
# OP3list<-list()
# for(j in 1:length(OP_parlist)){
#   
#   combiner<-do.call(cbind,list(timestamp,names(rasters.buff),Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
#   colnames(combiner)<-names
#   OP3list[[j]]<-combiner
# }
# 
# bufferlist<-do.call(rbind,OP3list) %>% as.tibble %>% mutate(Date=as_date(Date)) %>% mutate(Value=as.numeric(Value))
# rownames(bufferlist)<-NULL
# 



# 
# OP1<-"Scene"
# OP2<-"Mask"
# OP3<-c("NPixel","Masked","MaskedPercent","NoData","LandCover","Clouds","ROR","Shadow","Snow")
# 
# mvals<-seq(-26000,-21000,1000)
# v1<-values(r1)
# 
# 
# Mfunctions::extract2(r1,as_Spatial(oi2_buffer))
# 
# 
# OP_parlist<-list()
# OP_parlist[[1]]<-length(v1)
# OP_parlist[[2]]<-length(which(is.element(v1,mvals)))
# OP_parlist[[3]]<-round((OP_parlist[[2]]/OP_parlist[[1]])*100,2)
# OP_parlist[[4]]<-which(v1==mvals[1]) %>% length
# OP_parlist[[5]]<-which(v1==mvals[2]) %>% length
# OP_parlist[[6]]<-which(v1==mvals[3]) %>% length
# OP_parlist[[7]]<-which(v1==mvals[4]) %>% length
# OP_parlist[[8]]<-which(v1==mvals[5]) %>% length
# OP_parlist[[9]]<-which(v1==mvals[6]) %>% length
# names(OP_parlist)<-OP3
# 
# OP3list<-list()
# for(j in 1:length(OP_parlist)){
#   
#   combiner<-do.call(cbind,list(timestamp,tile,Scale1,Scale2,OP1,OP2,OP3[j],OP_parlist[[j]]))
#   colnames(combiner)<-names
#   OP3list[[j]]<-combiner
# }
# 
# imagelist<-do.call(rbind,OP3list) %>% as.tibble %>% mutate(Date=as_date(Date)) %>% mutate(Value=as.numeric(Value))
# rownames(imagelist)<-NULL



