
# Input -------------------------------------------------------------------
source("R/BaseFunctions.R")

stations<-c("Domef1500","Domef2000","Vimef2000","Vimes1500")
suffix<-"110119"
subfolder<-"01_Combination"

insitu.data.all <- paste0(dirfield,"/04_Combined","/InSituMetrics1_tidy_combined.rds")
insitu.data <- paste0(dirfield,"/04_Combined","/InSituMetrics1_tidy_combined_ndvi.rds")
srs.data    <- paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_100818.rds")
pheno.data  <- paste0(MNLS_Phenodir,"00_Combination/Filter/02_MovingWindow_byday.rds") 
s2.data     <- paste0(Workspacedir,"07_Products/Metrics_S2_Station_Selection_1608.rds")

combdir<-paste0(MetricsDir,subfolder,"/")

# Data Import -------------------------------------------------------------
#* In-situ -----------------------------------------------------------------
insitu.raw.all<-readRDS(insitu.data.all) 
insitu.raw<-readRDS(insitu.data) 
loclink<-insitu.raw %>% 
  dplyr::select(Date,Station,OP1,ROI) %>% 
  .[complete.cases(.),]
loclink_all<-rbind(loclink,loclink2)

insitu1<-insitu.raw %>% 
  mutate(Plot=ROI) %>% 
  mutate(OP3="mean") %>% 
  dplyr::select(-c(OP1,ROI)) %>% 
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value) 

insitu2<-insitu1 %>% 
  group_by(Date,Station,Scale1,Scale2,OP2) %>% 
  dplyr::summarize(Value=mean(Value)) %>% ungroup %>% 
  mutate(Plot=paste0(Station,"_Site")) %>% 
  mutate(OP3="Mean") %>%
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)

insitu<-rbind(insitu1,insitu2) %>% arrange(Date)

#* Monalisa ----------------------------------------------------------------
srs.raw<- readRDS(srs.data) %>% ungroup

decagon1<- srs.raw %>% 
  mutate(Station=map_chr(Station,simpleCap)) %>% 
  mutate(Plot=paste0(OP1,"_Station")) %>% 
  dplyr::select(-OP1) %>% 
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)%>% 
  slice(rep(1:n(),each=4)) %>% 
  mutate(Plot=str_c(Station,"_",LETTERS[1:4])) %>% 
  mutate(Scale1=map_chr(Scale1,function(x) simpleCap(tolower(x)))) %>% 
  mutate(Scale2="Decagon_SRS")

decagon<-decagon1 %>% 
  group_by(Date,Station,Scale1,Scale2,OP2) %>% 
  dplyr::summarize(Value=mean(Value)) %>% ungroup %>% 
  mutate(Plot=paste0(Station,"_Site")) %>% 
  mutate(OP3="Mean") %>%
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)

#* Phenocam ----------------------------------------------------------------
pheno.raw<-readRDS(pheno.data)

phenocam1<- pheno.raw %>% 
  mutate(Station=map_chr(station,simpleCap)) %>% 
  mutate(Date=Date2) %>% 
  mutate(Plot=map_chr(ROI, function(x) simpleCap(as.character(x),first=T))) %>% 
  add_column(Scale1="Proximal") %>% 
  add_column(Scale2="Phenocam") %>%
  add_column(OP3="90Percentile") %>% 
  gather(.,key="OP2",value=Value,NDVI,GCC) %>% 
  filter(Type=="Point") %>% 
  filter(OP2=="NDVI") %>%
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)

phenocam2<-phenocam1 %>% 
  group_by(Date,Station,Scale1,Scale2,OP2) %>% 
  dplyr::summarize(Value=mean(Value)) %>% ungroup %>% 
  mutate(Plot=paste0(Station,"_Site")) %>% 
  mutate(OP3="Mean") %>%
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)

phenocam<-rbind(phenocam1,phenocam2) %>% arrange(Date)

#* Sentinel-2 --------------------------------------------------------------

s2.raw<-readRDS(s2.data) %>% 
  filter(OP2=="NDVI") %>% 
  filter(!is.na(Value)) %>% 
  mutate(Station=as.character(Station)) %>% 
  mutate(OP1=as.character(OP1))

sentinel<-s2.raw %>%
  filter(is.na(Date)) %>% 
  dplyr::select(-Date) %>% 
  distinct() %>% 
  filter(OP3!="Sd") %>%
  mutate(Value=Value/10000) %>% 
  mutate(Date=as_date(NA)) %>% 
  full_join(.,loclink_all,by=c("Date","Station","OP1")) %>% 
  mutate(Date=Date.Sen) %>% 
  mutate(Plot=ROI) %>% 
  filter(OP1!="Station" & OP1!="Buffer") %>% 
  mutate(Plot=pmap_chr(list(Station,Plot,OP1),function(x,y,z)ifelse(z=="Site",paste0(x,"_",z),y))) %>% 
  mutate(Scale2="Sentinel2_MSI") %>% 
  filter(!is.na(Plot)) %>% 
  filter(!is.na(Value)) %>% 
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)

sentinel2<- ml.ndvi %>% 
  filter(OP3!="Sd") %>%
  mutate(Value=Value/10000) %>% 
  full_join(.,loclink_all,by=c("Date","Station","OP1")) %>% 
  mutate(Date_fin=map2_dbl(Date,Date.Sen,function(x,y) ifelse(is.na(x),y,x))) %>% 
  mutate(Date_fin=as_date(Date_fin)) %>% 
  mutate(Date=Date_fin) %>% 
  mutate(Plot=ROI) %>% 
  filter(OP1!="Station" & OP1!="Buffer") %>% 
  mutate(Plot=pmap_chr(list(Station,Plot,OP1),function(x,y,z)ifelse(z=="Site",paste0(x,"_",z),y))) %>% 
  mutate(Scale2="Sentinel2_MSI") %>% 
  filter(!is.na(Plot)) %>% 
  filter(!is.na(Value)) %>% 
  dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)


# Data Combination ----------------------------------------------------------
db<-bind_rows(insitu,decagon,phenocam,sentinel) %>% 
  .[complete.cases(.$Plot),] %>% 
  dplyr::rename(Sensor=Scale2) %>% 
  filter(Station!="P2")

saveRDS(db,file = paste0(outdir,"AllSensorData",suffix,".rds"))

db.all<-db  %>% 
  filter(!is.na(Sensor))%>% 
  arrange(Date) %>% 
  filter(Date>="2017-03-01") %>% 
  filter(Date<"2017-11-01")

saveRDS(db.all,file = paste0(outdir,"AllSensorData",suffix,"_filtered.rds"))

db.mean<-db %>% 
  filter(OP2=="NDVI") %>% 
  group_by(Date,Station,Scale1,Sensor,OP2,OP3) %>% 
  dplyr::summarise(Value=mean(Value)) %>% 
  ungroup %>% filter(!is.na(Value) & !is.na(Sensor))

saveRDS(db.mean,file = paste0(outdir,"AllSensorData",suffix,"_meanDay.rds"))
