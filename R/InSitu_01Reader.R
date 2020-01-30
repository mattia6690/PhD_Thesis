# 1. Initialization ----
source("R/BaseFunctions.R")

# 2. Files ----

lf_full<-list.files(paste0(dirfield,"00_Raw"),pattern =".csv",full.names = T)
metadir.hyp<-"C:/Users/MRossi/Documents/03_Data/03_InSitu/05_HyperSpec/Combined/"
# 3. Computation ----

scale1 <- "Ground"
names  <- c("Date","Station","Scale1","Scale2","Prop1","Prop2","Prop3","Value")
suffix <- 251019

for(i in 1:length(lf_full)){
  
  #* 3.1 Input ----
  itab_raw <-read_csv(lf_full[i],col_names = F,col_types = cols())
  itab<-.remheader(itab_raw)
  
  foi <-itab_raw %>% filter(X1=="FOI") %>% select(X2) %>% as.character
  date<-itab_raw %>% 
    filter(X1=="Date") %>% 
    select(X2) %>% 
    as.numeric %>% 
    sprintf("%06d",.) %>% 
    as.Date(format="%d%m%y")
  
  data.raw<-itab %>% 
    gather(Prop1,Acquisition,grep("ID",colnames(itab),value = T))  %>% 
    add_column(Scale1=scale1,.before=T) %>%
    add_column(Station=foi,.before=T) %>%
    add_column(Date=date,.before=T)
  
  print(paste("Files of",foi,"on",date,"read - Iteration=",i))
  
  #* 3.2 Hyperspectral ----
  
  print("Hyperspectral metrics")
  hy.data<-getVals.spectrometer(data.raw,date,dirhyp)
  

  if(nrow(hy.data)>0){
    
    # FIrst the combined values
    dunnest<-hy.data %>% dplyr::select(-Metadata) %>% unnest
    if(nrow(dunnest)>0) saveRDS(dunnest,file = paste0(dirhyp,"01b_Selected/",foi,"_",format(date,"%Y%m%d"),".rds"))
    
    
    dt<-hy.data$Data
    PRI_1  <-map_dbl(dt,function(x) hypindices(x,c(530,534),c(568,572),stat="PRI"))
    PRI_2  <-map_dbl(dt,function(x) hypindices(x,c(520,544),c(558,582),stat="PRI"))
    NDVI_1 <-map_dbl(dt,function(x) hypindices(x,c(648,652),c(808,812),stat="NDVI"))
    NDVI_2 <-map_dbl(dt,function(x) hypindices(x,c(638,662),c(798,822),stat="NDVI"))
    NDVI_3 <-map_dbl(dt,function(x) hypindices(x,c(635,695),c(727,957),stat="NDVI"))
    
    hy.data.index <- hy.data %>% 
      mutate(PRI_1,PRI_2,NDVI_1,NDVI_2,NDVI_3)%>% 
      gather(PRI_1,PRI_2,NDVI_1,NDVI_2,NDVI_3,key="Indices",value="Value") %>% 
      separate(col="Indices",into=c("Prop2","Prop3"),sep="_")
    
    hy.data.index<- hy.data.index %>% 
      dplyr::select(Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value)
    
    if(nrow(hy.data.index)>0) saveRDS(hy.data.index,file = paste0(dirhyp,"02_Combined/",foi,"_",format(date,"%Y%m%d"),".rds"))
    
  }
  
  #* 3.2 LAI ----
  print("Calculating LAI")
  lai.data<-getVals.lai(data.raw,date,directory = dirlai)
  
  if(nrow(lai.data)>0) saveRDS(lai.data,file = paste0(dirlai,"02_Combined/",foi,"_",format(date,"%Y%m%d"),".rds"))
  
  #* 3.3 Biomass ----
  print("Calculating Biomass")
  bio.data<-getVals.biomass(data.raw,date)

  # Save the data
  
  if(nrow(bio.data)>0) saveRDS(bio.data,file = paste0(dirbio,"02_Combined/",foi,"_",format(date,"%Y%m%d"),".rds"))
  
  # Add the coordinates
  
  coords<-data.raw %>% 
    filter(Type=="Location") %>% 
    select(-Type) %>% 
    spread(key=Name,value = Acquisition) %>% 
    mutate(Lat=as.numeric(Lat)) %>% 
    mutate(Lon=as.numeric(Lon))
  
  nas<-is.na(coords$Lat)
  
  if(any(isTRUE(nas))){
    
    coords_lai<-data %>% 
      group_by(Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3) %>% 
      dplyr::summarize(Value=mean(Value)) %>% 
      spread(Prop3,Value) %>% 
      ungroup
    
  }
  
  laibiogps<-rbind(bio.data,lai.data) %>% left_join(.,coords)
  laibiogps2<-st_as_sf(laibiogps,coords=c("Lon","Lat")) %>% 
    st_set_crs(4326)
  
  name<- paste0(InSitu_dir,"04e_LaiBioGps/",foi,"_",format(date,"%Y%m%d"),".shp")
  
  if(!file.exists(name)){
    
    if(nrow(laibiogps2)>0) st_write(laibiogps2,dsn = name,update = T,quiet = T)
    
  }
  
}


#   
#     select(contains("ID")) %>% 
#     rownames_to_column() %>% 
#     add_column(Nr=seq(1,nrow(.),1),.before=TRUE) %>% 
#     gather(c(2:ncol(.)),key="IDs",value="File")
#   
#   hyperMetrics<-list()
#   for(j in 1:nrow(files)){
#     
#     files_red<-files %>% slice(j)
#     id<-files_red$File %>% as.numeric
#     
#     if(is.na(id)) next
#     
#     OP11<-files_red$IDs
#     OP12<-files_red$Nr
#     OP1<-paste(OP11,OP12,sep="_")
#     
#     hyp_dat<-paste(strsplit(date, "", fixed=T)[[1]][c(3,4,1,2,5,6)],collapse='')
#     hyp_nm <-paste("HRPDA",hyp_dat,sprintf("%04d",id),"sig",sep=".")
#     
#     hyp_lf<-list.files(dirhyp,full.names = T)
#     hyp_lf2<-hyp_lf[which(is.element(basename(hyp_lf),hyp_nm))]
#     
#     if(length(hyp_lf2)==0) next
#     
#     rd<-read_table(hyp_lf2,col_types = cols("/*** Spectra Vista SIG Data ***/" = col_character()))
#     colnames(rd)<-"data"
#     
#     gpslon<-toString(rd$data[which(grepl("longitude",rd$data))]) %>%
#       str_replace("longitude=","") %>% str_replace_all("E","") %>%
#       str_split(",") %>% unlist %>% as.numeric %>% mean(na.rm=T) %>% "/" (100) 
#     gpslon <-c(date,foi,scale1,scale2,OP1,"GPS","Lon",gpslon)
#     
#     gpslat<-toString(rd$data[which(grepl("latitude",rd$data))]) %>% 
#       str_replace("latitude=","") %>% str_replace_all("N","") %>%  
#       str_split(",") %>% unlist %>% as.numeric %>% mean(na.rm=T) %>% "/" (100) 
#     gpslat <-c(date,foi,scale1,scale2,OP1,"GPS","Lat",gpslat)
#     
#     rd<-rd[which(rd[1]=="data=")+1:nrow(rd),]
#     s1<-na.omit(separate(rd,col = data, into=c("Wavel","Rup","Rdn","Refl"),sep="  ",convert=))
#     class(s1$Wavel)<-"numeric"
#     class(s1$Refl)<-"numeric"
#     
#     # PRI and NDVI Production
#     pri1 <-hypindices(s1,c(530,534),c(568,572),stat="PRI")
#     pri1 <-c(date,foi,scale1,scale2,OP1,"PRI","1",pri1)
#     pri2 <-hypindices(s1,c(520,544),c(558,582),stat="PRI")
#     pri2 <-c(date,foi,scale1,scale2,OP1,"PRI","2",pri2)
#     ndvi1<-hypindices(s1,c(648,652),c(808,812),stat="NDVI")
#     ndvi1<-c(date,foi,scale1,scale2,OP1,"NDVI","1",ndvi1)
#     ndvi2<-hypindices(s1,c(638,662),c(798,822),stat="NDVI")
#     ndvi2<-c(date,foi,scale1,scale2,OP1,"NDVI","2",ndvi2)
#     ndvi3<-hypindices(s1,c(635,695),c(727,957),stat="NDVI")
#     ndvi3<-c(date,foi,scale1,scale2,OP1,"NDVI","3",ndvi3)
#     
#     # Combiantion in a Matrix and then in a list
#     hymat<-rbind(gpslon,gpslat,pri1,pri2,ndvi1,ndvi2,ndvi3)
#     colnames(hymat)<-names
#     hyperMetrics[[j]]<-hymat
#   }
#   
#   # Total Dataframe of the hyperspectral metrics
#   if(length(hyperMetrics)>0){
#     hyperMetricsBind<-do.call(rbind,hyperMetrics)
#     rownames(hyperMetricsBind)<-seq(1,nrow(hyperMetricsBind),1)
#   }else{
#     hyperMetricsBind<-hyperMetrics
#   }
# }
#   
#   
#   #* 3.3 LAI
#   
#   scale2<-"LAI2200"
#   lnames<-c("LAI","SEL","ACF","DIFN","MTA","SEM","SMP")
#   
#   files<-itab %>% filter(Type=="Leaf Area") %>% select(contains("ID")) %>% 
#     add_column(Nr=seq(1,nrow(.),1),.before=TRUE) %>% 
#     gather(c(2:ncol(.)),key="IDs",value="File")
#   
#   laiMetrics<-list()
#   
#   for(j in 1:nrow(files)){
#     
#     files_red<-files %>% slice(j)
#     id<-files_red$File
#     
#     if(is.na(id)) next
#     
#     OP11<-files_red$IDs
#     OP12<-files_red$Nr
#     OP1<-paste(OP11,OP12,sep="_")
#     
#     lai_nm<-paste(date,id,sep="_")
#     rd   <-read.csv(paste0(dirlai,"/",lai_nm,".csv"))
#     la1  <-rd$V2[which(is.element(rd$V1,lnames))] %>% as.character
#     
#     laitab<-cbind(date,foi,scale1,scale2,OP1,"LAI",lnames,la1)
#     
#     
#     gps1<-rd %>% filter(V1=="GPSLAT") %>% select(V2)  %>% as.matrix %>% as.numeric %>% {if(length(.)==0) NA else .}
#     gpslat<-cbind(date,foi,scale1,scale2,OP1,"LAI","Lat",gps1)
#     gps2<-rd %>% filter(V1=="GPSLONG") %>% select(V2) %>% as.matrix %>% as.numeric %>% {if(length(.)==0) NA else .}
#     gpslon<-cbind(date,foi,scale1,scale2,OP1,"LAI","Lon",gps2)
#     
#     # Combiantion in a Matrix and then in a list
#     mat<-rbind(gpslon,gpslat,laitab)
#     colnames(mat)<-names
#     laiMetrics[[j]]<-mat
#     
#   }
#   
#   if(length(laiMetrics)>0){
#     laiMetricsBind<-do.call(rbind,laiMetrics)
#     rownames(laiMetricsBind)<-seq(1,nrow(laiMetricsBind),1)
#   }else{
#     laiMetricsBind<-laiMetrics
#   }
#   
#   #* 3.4 Biomass
#   
#   scale2<-"Biomass"
#   
#   files<-itab %>% filter(Type=="Biomass") %>% select(contains("ID")) %>% 
#     add_column(Nr=seq(1,nrow(.),1),.before=TRUE) %>% 
#     gather(c(2:ncol(.)),key="IDs",value="File")
#   
#   gps<- itab %>% filter(Type=="Location") %>% gather(c(3:ncol(.)),key="IDs",value="File") %>% mutate(IDs=str_c(IDs,"_1")) %>% .[c(3,1,2,4)]
#   
#   OP1<-paste(files$IDs,"1",sep="_")
#   uniqueID<-unique(OP1)
#   OP2<-"Phytomass"
#   
#   namesBIO<-itab %>% filter(Type=="Biomass") %>% select(Name)
#   OP3<-lapply(namesBIO, function(x) str_split(x,"\\("))
#   OP3<-lapply(OP3$Name, `[[`, 1) %>% unlist %>% str_replace(.," ","")
#   
#   bioMetricsbio<-cbind(date,foi,scale1,scale2,OP1,OP2,OP3,files$File)
#   colnames(bioMetricsbio)<-names
#   bioMetricsgps<-cbind(date,foi,scale1,scale2,gps) %>% distinct
#   colnames(bioMetricsgps)<-names
#   
#   bioMetrics1<-rbind(bioMetricsbio,bioMetricsgps)
#   
#   colnames(bioMetrics1)<-names
#   
#   OP2<-"Laboratory"
#   OP3<-c("BioWet","BioDryRaw","BioWatRawPerc","MeanErr")
#   
#   bioMetrics2<-list()
#   for(j in 1:length(uniqueID)){
#     
#     OP1temp<-uniqueID[j]
#     sub<-bioMetrics1 %>% as.data.frame %>% 
#       filter(OP1==OP1temp) %>% select(Value) %>% 
#       as.matrix %>% as.numeric
#     
#     if(is.na(sub[5])){sub[5]<-sub[4]} # Replace missing W2
#     bwet<-  sub[4]-sub[1] # Wet Weight
#     bdry<-  sub[7]-sub[3] # Dry Weight
#     bwat<-  round((1-(bdry/bwet))*100,2) # Water Content Raw
#     bunc<-  abs(sub[4]-sub[5]) + # Difference between 
#       abs(sub[2]-sub[1]) + # Rest in Plastic Bag
#       abs((sub[5]-sub[2])- # Rest not within Plastic Bag
#             (sub[6]-sub[3])) %>% round(.,5)
#     
#     sub2<-c(bwet,bdry,bwat,bunc)
#     bioMetrics2[[j]]<-cbind(date,foi,scale1,scale2,OP1temp,OP2,OP3,sub2)
#     
#   }
#   
#   bioMetrics2<-do.call(rbind,bioMetrics2)
#   colnames(bioMetrics2)<-names
#   bioMetricsBind<-rbind(bioMetrics1,bioMetrics2)
#   
#   
#   #* 3.5 Soil Water Content
#   
#   scale2<-"SWC"
#   
#   files<-itab %>% filter(Type=="Water") %>% slice(1:4) %>% select(contains("ID")) %>% 
#     add_column(Nr=seq(1,nrow(.),1),.before=TRUE) %>% 
#     gather(c(2:ncol(.)),key="IDs",value="File")
#   
#   files<-na.omit(files)
#   
#   OP1<-paste(files$IDs,files$Nr,sep="_")
#   OP2<-"Soil Water"
#   OP3<-"Percent"
#   
#   swcMetricsBind<-cbind(date,foi,scale1,scale2,OP1,OP2,OP3,files$File)
#   colnames(swcMetricsBind)<-names
#   
#   #* 3.6 Combine and Save 
#   mat<-rbind(hyperMetricsBind,laiMetricsBind,bioMetricsBind,swcMetricsBind)
#   if(length(mat)>0) rownames(mat)<-seq(1,nrow(mat),1)
#   mat<-mat %>% as.matrix
#   
#   write.csv(mat,file=paste0(dirfield,"03_DaySOS/",foi,"_",date,"_combinedSOS_",suffix,".csv"))
# }
# 
