
source("R/BaseFunctions.R")
source("R/RemSen/S2_ExtractMetrics1_Initialization.R")

# Base information for tidy rows
Scale1<-s1<-"Sentinel2"
Scale2<-s2<-"MSI"
year<-"2017"
suffix<-"050718"
mvals<-seq(-26000,-21000,1000)
gdal.trsl<-'"C:/Program Files/GDAL/bin/gdal/apps/gdal_translate.exe"'

ns<-data.all %>% group_by(Station) %>% nest

# Start of the Central Script to combine the Metrics
# The extract process will be done separately for (i) the whole Image
for(i in 1:nrow(df)){
  
  # Initialize - Loop----
  #* Create Variables ----
  print(paste("Start image",i,"of",nrow(df)))
  if(i==1) metricsList<-pointlist.id<-list()
  start<-Sys.time()
  bind.df<-bind.pntID<-data.frame()
  
  #* Select from df ----
  row<-df[i,]
  scene<-row$dir
  timestamp<-row$AcqDate
  timestamp2<-timestamp %>% as.Date(.,format="%Y%m%d")
  tile<-row$Tile
  
  cat("Initialization FINISHED...")
  
  # Subset - use GDAL----
  ns2<-ns %>%
    mutate(temp =map (data, function(i) tempfile(fileext = ".tif"))) %>% 
    mutate(bbox =map (data, function(i) st_bbox(i) %>% .[order(c(1,4,3,2))] %>% paste(collapse=" "))) %>% 
    mutate(gdal =map2(bbox,temp,function(i,j,r=scene,g=gdal.trsl) paste(g,"-projwin",i,r,j,"-q"))) %>% 
    mutate(gcal =map2(gdal,data,function(i,j,r=scene) {
      
      overlaps<-st_contains(st_as_sfc(st_bbox(raster(r))),st_as_sfc(st_bbox(j))) %>% as.numeric
      if(!is.na(overlaps)) { system(i);return(TRUE) } else {return(FALSE)}
      
      })) %>% 
    mutate(rast =map2 (temp,gcal,function(i,j) {if(isTRUE(j)) raster(i)})) %>% 
    .[which(.$gcal==T),]
  
  cat("Subsetting FINISHED...")
  
  # j<-ns2$data[[10]]
  # i<-ns2$rast[[9]]
  # Extract - Raster Values ----
  ns3<-ns2 %>% 
    mutate(extract=map2(rast,data,function(i,j){
      
      #' Divide by Point or Polygon. Geometry Types are not accepted yet.
      #' Afterwards the st Objects have to be transformed to Spatial (sp) Objects
      #' since they are not interoperable with the raster package yet
      
      if(!is.na(overlaps)) {
        
        pt<-j[st_is(j,"POINT"),]
        sh<-j[st_is(j,"POLYGON"),]
        
        e1<-Mfunctions::extract2(i,as_Spatial(pt),returnVals = T)
        
        if(nrow(sh)>0) {
          e2<-Mfunctions::extract2(i,as_Spatial(sh),returnVals = T)
          ll<-c(e1,e2)
          
        } else { ll<-e1 }
        
        lst<-melt(ll) %>% select(L1,value)
        
      } else{ lst<-rep(NA,nrow(j)) %>% as.list %>% melt %>% select(L1,value) }
      return( lst )
      
    }))
  
  cat("Extract FINISHED...")
  
  # Metrics - Count ----
  ns4<-ns3 %>% mutate(Metrics=map(extract,function(i,m=mvals) {
    
    # Create List by extracted Values for each Polygon of each Station
    ii<-i %>% group_by(L1) %>% nest
    ii2<-ii %>% mutate(mask=map(data,function(x,m=mvals){
      
      # Count number of occurences by Mask Value
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
      
      # Mean or Absolute NDVI Computation
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
  
  cat("Metrics FINISHED...")

  # Combine - all ----
  ns5<-ns4 %>% 
    mutate(result=map2(data,Metrics,function(w,x){
      
      # Add the date column to the data list
      i1<- as.data.frame(w) %>% 
        select(-c(OP3,OP2,geometry)) %>% 
        add_column(Date.Sen=timestamp2,.after="Date")
      
      # Divide between Mask and NDVI Related Columns
      j1<- x %>% as.tibble %>% select(NPixel,Masked,MaskedPercent,NoData,LandCover,Clouds,ROR,Shadow,Snow) 
      j2<- x %>% as.tibble %>% select(Absolute,Mean,Sd)
      
      # Make them TIDY
      cb1<-cbind(i1,j1) %>% 
        gather(.,key=OP3,value=Value,colnames(j1)) %>% 
        add_column(OP2="Mask",.before="OP3")
      
      cb2<-cbind(i1,j2) %>% 
        gather(.,key=OP3,value=Value,colnames(j2)) %>% 
        add_column(OP2="NDVI",.before="OP3") 
      
      return(rbind(cb1,cb2))
      
    }))
  
  cat("Combination FINISHED...")
  
  # Time - Filter ----
  ns6<-ns5 %>%
    mutate(Tfilter=map(result,function(i) {
    
    #' Select either the entries with NA time difference(Shapes or points) 
    #' and those with less than 14 days
    i$Tdiff<-as.numeric(i$Date-i$Date.Sen) %>% abs
    i2<-i %>% filter(Tdiff<14 | is.na(Tdiff)) %>% select(-Tdiff)
      
    }))
  
  ns.fin<-ns6 %>% select(Station, Tfilter) %>% unnest
  ns.ras<-ns6 %>% select(Station, rast) %>% add_column(Date=timestamp2,.after="Station")
  if(i==1) finlist<-ns.fin else rbind(finlist,ns7)
  if(i==1) raslist<-ns.fin else rbind(finlist,ns7)
  
  cat("Time Filter FINISHED...")
  
  end<-Sys.time()
  diff<-end-start
  print(paste("Raster at Timestep",timestamp,tile,"FINISHED in",diff))
} 




# TestVars 
# i=ns3$extract[[1]]
# x<-ii$data[[25]]
# 
# w<-ns5$data[[3]]
# x<-ns5$Metrics[[3]]
# y<-ns5$Tdiff[[3]]
# z<-ns5$Date2[[3]]
# 
# i<-ns6$result[[1]]
# 
# i=ns2$rast[[3]]
# j=ns2$data[[3]]
  