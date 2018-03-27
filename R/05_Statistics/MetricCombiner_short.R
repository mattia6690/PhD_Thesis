# 1. Initialization ----
source("R/BaseFunctions.R")

# 2. Input ----

station<-"vimes1500"

# Read the Metrics
# InSitu
insitu_raw<-readRDS(paste0(MetricsDir,"CombinedInSituMetricsSOS.rds"))

# Sentinel2
suffix<-"151217"
sentinel2_raw<-readRDS(paste0(MetricsDir,"S2_Filtered_NDVI_",suffix,".rds"))

# Monalisa
monalisa_raw<- readRDS(paste0(MetricsDir,"Monalisa_NDVI.rds"))
monalisa_raw<- monalisa_raw %>% filter(Value!="NaN") %>% 
  mutate(Value=as.numeric.factor(Value)) %>% 
  filter(Value>0)

# Managements
Manage<-readRDS(Manage,file = paste0(DataDir,"/Management/",station,"_Management_2017.rds"))

dir<-paste0(MetricsDir,station,"/")

#* 2.1. Select the Data ----

mnls<-monalisa_raw %>% 
  filter(grepl("10:30",Date)) %>% 
  filter(Station==station)
mnls$Date<-mnls$Date %>% as_date
mnls$Value<-mnls$Value %>% as.character %>% as.numeric

alltab<-rbind(insitu_raw,sentinel2_raw,mnls)
alltabstat<- alltab %>% filter(Station==station)

s2<-sentinel2_raw %>% 
  filter(Station==station) %>% 
  filter(OP2=="NDVI") %>% 
  filter(OP3=="Mean") %>% 
  filter(Value!="NaN")
s2$Date<-s2$Date %>% as_date
s2$Value<-s2$Value %>% as.character %>% as.numeric %>% `/`(10000)

groundN<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP2=="NDVI") %>% 
  group_by(Date,OP2) %>% summarise(Value=mean(Value))
groundN$Date<-groundN$Date %>% as.Date(format="%d%m%y")