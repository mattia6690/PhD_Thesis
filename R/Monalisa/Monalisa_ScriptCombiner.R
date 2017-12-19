
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")

writeCSV=T

ndvidir<-paste0(MonalisaDir,"NDVI & PRI Stations/")

lf<-list.files(ndvidir,full.names = T)
lf_short<-list.files(ndvidir,full.names = F)

stations<-substr(lf_short,1,9)

names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")

lf2<-list()
lf3<-list()
for (i in 1:length(lf)){
  
  lf1<-read.csv(lf[i])
  
  lf2[[1]] <-lf1[-(1:3),] %>% select(1) %>% as.data.frame
  lf2[[2]] <-rep(stations[i],nrow(lf2[[1]])) %>% as.data.frame()
  lf2[[3]] <-rep("MONALISA",nrow(lf2[[1]]))%>% as.data.frame()
  lf2[[4]] <-rep("DECAGON",nrow(lf2[[1]]))%>% as.data.frame()
  lf2[[5]] <-rep("Point",nrow(lf2[[1]]))%>% as.data.frame()
  lf2[[6]] <-rep("NDVI",nrow(lf2[[1]]))%>% as.data.frame()
  lf2[[7]] <-rep("Avg",nrow(lf2[[1]]))%>% as.data.frame()
  lf2[[8]] <-lf1[-(1:3),] %>% select(which(lf1[1,]=="NDVI_Avg"))%>% as.data.frame()
  
  lf3[[i]]<-do.call(cbind, lf2) %>% setNames(names)
  
}


lf4<-do.call(rbind, lf3)

saveRDS(lf4,paste0(MetricsDir,"Monalisa_NDVI.rds"))
if(writeCSV==T) write.csv(lf4,paste0(MetricsDir,"Monalisa_NDVI_2017.csv"))



stat.unq<-lf4 %>% select(Station) %>% unique %>% as.matrix

for(i in 1:nrow(stat.unq)){
  
  stat<-stat.unq[i]
  
  lf5<-lf4 %>% filter(Station==stat)
  
  ggplot(lf5,aes(x=as.Date(Date,format="%Y-%m-%d"),y=as.numeric(Value)))+
    geom_point()+
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %y"))
  
}



