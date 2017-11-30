# 1. Initialization ----
source("R/BaseFunctions.R")

# 2. Input ----

#Read the Metrics
#InSitu
insitu_raw<-read.csv(paste0(MetricsDir,"InSituMetrics.csv"))

#Sentinel2
sentinel2_raw<-readRDS(paste0(MetricsDir,"Sentinel2_NDVI_metrics.rds"))

vimes1500cuts<-c(as.Date("2017-07-03"),as.Date("2017-09-04"))
# 3. Combine ----
#* 3.1 Sentinel and InSitu Data

stations1<-sentinel2_raw$mnls_stations %>% tolower %>% unique %>% sort
stations2<-insitu_raw$FOI %>% tolower %>% unique %>% sort

stations<-stringinlist(stations1,stations2)


insfilter<-insitu_raw %>% select(c(FOI,Date,NDVI1))
insfilter$Date<-as.Date(insfilter$Date)
S2filter<-filter(sentinel2_raw ,mnls_stations %in% stations)

S2filter[["Date"]]<-as.Date(S2_dir2date(S2filter$dir.x))

for(i in stations){
  
  ins1<-insfilter %>% filter(FOI==simpleCap(i))
  sen1<-S2filter %>% filter(mnls_stations==i) %>% filter(!is.na(Weigted_Mean)) %>% filter(Original_Nas==0)
  g1<-ggplot(ins1,aes(x=Date,y=NDVI1))+
    geom_boxplot(aes(group=Date),fill="chartreuse3")+
    geom_vline(aes(xintercept=as.Date("2017-07-03")),color="darkgreen")+
    geom_vline(aes(xintercept=as.Date("2017-09-04")),color="darkgreen")+
    geom_point(data=sen1,aes(x=Date,y=(as.numeric(Weigted_Mean)/10000)),color="gray40",size=4)+
    scale_x_date(limits = c(as.Date("2017-04-01"),as.Date("2017-08-01")),date_breaks = "1 month")+
    ggtitle("Sentinel 2 MSI and InSitu Measurements of Grassland")+
    ylab("NDVI")
  
  ggsave(plot=g1, filename = paste0(MetricsDir,"/InSituS2/S2_Insitu_test2.png"),device = "png",height=7,width = 14)
  
}


