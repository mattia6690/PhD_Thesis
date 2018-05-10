###
# 1. Initialization ----
###

#* Data Import ----
source("R/BaseFunctions.R")

ndvidir<-paste0(MonalisaDir,"01_Data/Monalisa/")

lf<-list.files(ndvidir,full.names = T)
lf_short<-list.files(ndvidir,full.names = F)

writeCSV=T
savePlot=T


stations<-str_split(lf_short,"_") %>% 
  map(function(i) i[1]) %>% 
  unlist

names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")

#* Create Tidy Table ----
lf.t1<-lf.t2<-list()
for (i in 1:length(lf)){
  
  tab        <-suppressMessages(read_csv(lf[i],col_names = F))
  lf.t1[[1]] <-tab %>% slice(-(1:4)) %>% select(1) %>% as.tibble
  lf.t1[[2]] <-rep(stations[i],nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[3]] <-rep("MONALISA",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[4]] <-rep("DECAGON",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[5]] <-rep("Point",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[6]] <-rep("NDVI",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[7]] <-rep("Avg",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[8]] <-tab[-(1:4),] %>% select(which(tab[2,]=="NDVI_Avg"))
  lf.t2[[i]] <-do.call(cbind.data.frame, lf.t1) %>% setNames(names)
  
}

mnls.tidy <-lf.t2 %>% 
  do.call(rbind.data.frame,.) %>% 
  as.tibble %>%
  mutate(Date=as.POSIXct(Date,format="%Y-%m-%d %H:%M")) %>% 
  separate(Date,c("Date","Time")," ") %>% 
  mutate(Date=as_date(Date)) %>%
  mutate(Time=as.hms(Time)) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  filter(!is.nan(Value))

saveRDS(mnls.tidy,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_raw.rds"))
if(writeCSV==T) write.csv(mnls.tidy,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_raw.csv"))

# 2. Filter ----

#* INPUT ----

timerange<-c("10:30:00","15:00:00")
station<-"vimes1500"
dir<-paste0(MonalisaDir,"02_Tables/")

start<-"2017-01-01" %>% as_date
end  <-"2017-12-31" %>% as_date
movingWinL<-7

#* Time Filter ----

time<-as.hms(timerange)
mnls.filter1<-mnls.tidy %>% 
  filter(Time>=time[1]) %>% 
  filter(Time<=time[2])

saveRDS(mnls.filter1,paste0(dir,"Monalisa_NDVI_filter1.rds"))
if(writeCSV==T) write.csv(mnls.filter1,paste0(dir,"Monalisa_NDVI_filter1.csv"))

#* Outlier Filter ----
temp<-mnls.filter1 %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  nest

temp2<- temp %>% mutate(data=map(data,function(i){
  
  gDens <-.getDensityLimits(i$Value)
  lof   <- i %>% select(Value) %>% lofactor(., k=3)

  dat1   <- i %>% 
    add_column(.,Lower=gDens[1]) %>% 
    add_column(.,Upper=gDens[4]) %>% 
    add_column(.,LOF=lof)
  
  return(cbind(i,dat1))

}))

mnls.filter2<-temp2 %>% 
  unnest %>% 
  filter(Value>Lower) %>% 
  filter(Value<Upper) %>% 
  filter(LOF<2)

saveRDS(mnls.filter2,paste0(dir,"Monalisa_NDVI_filter2.rds"))
if(writeCSV==T) write.csv(mnls.filter2,paste0(dir,"Monalisa_NDVI_filter2.csv"))

#* Max Value Filter ----

temp3<-filter.data2 %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  nest %>% 
  mutate(filtered = map(data, ~ filter(., Value==max(Value))))

mnls.filter3<-temp3 %>% select(-data) %>% unnest 

saveRDS(mnls.filter3,paste0(dir,"Monalisa_NDVI_filter3.rds"))
if(writeCSV==T) write.csv(mnls.filter3,paste0(dir,"Monalisa_NDVI_filter3.csv"))

#* Moving Window Filter ----

temp.station<-mnls.filter3 %>% filter(Station==station)
date.range<-seq(start,end,1)
tbl<-list()

for(i in 1:length(date.range)){
  
  date1<-date.range[i]
  date2<-seq(date1,date1+movingWinL-1,1)
  
  if(length(which(is.element(date2,temp.station$Date)==T))>=2){
    
    summary4<- temp.station %>% 
      filter(is.element(Date,date2)) %>% 
      group_by(Date,Station) %>% nest %>% 
      mutate(new=map(data,function(i) i[1,])) %>% 
      select(-data) %>% 
      unnest
    
    if(nrow(summary4)>2){
      mp<-map(2:5,function(i) lofactor(summary4$Value, k=i)) %>% do.call(cbind,.) %>% rowMeans()
      tbl[[i]]<-as.tibble(summary4$Date) %>% add_column(LOF=mp)
      
    }
  }
}

mp2<-map(tbl, ~ filter(.,LOF>2)) 
wh<- do.call(rbind.data.frame,mp2) %>% select(value) %>% table %>% {which(.>3)} %>% names

filter.data3<-summary3 %>% filter(!is.element(Date,as_date(wh)))




gall<-ggplot(filter.data3,aes(Date,Value))+geom_point()+geom_line()+
  ggtitle(paste0("Decagon NDVI of 2017 Filtered 3 - MovingWindow"))+
  ylim(c(-1,1))

saveRDS(filter.data3,paste0(MetricsDir,"Monalisa_NDVI_filtered_MovingWindow.rds"))
ggsave(gall,filename = paste0(Monalisa17Dir,"FilterComplete.png"),device = "png",width = 7,height=7)
  
# 3. Plotting ----

#* MinMaxMeanPlot ----
summary<-filter.data2 %>% 
  group_by(Date,Station) %>% 
  dplyr::summarize(maxNDVI=max(Value,na.rm=T),
                   minNDVI=min(Value,na.rm=T),
                   meanNDVI=mean(Value,na.rm=T))

summary2<-melt(summary,id.vars = c("Date","Station"),value.name = "Value") %>% as.tibble()
summary2$Date<-as_date(summary2$Date)

g6<-ggplot(summary2,aes(x=Date,y=value,color=variable))+
  geom_point()+
  ggtitle(paste0("Decagon NDVI of 2017 Filtered - MinMeanMax"))+
  ylim(c(-1,1))+
  facet_grid(Station~.)
ggsave(g6,filename = paste0(Monalisa17Dir,"FilterCompleteMinMaxMean.png"),device = "png",width = 7,height=11)

#* FilterPlot ----
summary3<-summary2 %>% filter(variable=="maxNDVI")
g7<-ggplot(summary3,aes(x=Date,y=value))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  facet_grid(Station~.)

ggsave(g7,filename = paste0(Monalisa17Dir,"FilterComplete.png"),device = "png",width = 7,height=11)
