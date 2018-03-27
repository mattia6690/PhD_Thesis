###
# 1. Initialization ----
###

#** Data Import ----
source("R/BaseFunctions.R")

lf<-list.files(ndvidir,full.names = T)
lf_short<-list.files(ndvidir,full.names = F)

writeCSV=T
savePlot=T

ndvidir<-paste0(MonalisaDir,"NDVI & PRI Stations/")
stations<-substr(lf_short,1,9)

names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")

#** Create Tidy Table ----
lf.t1<-list()
lf.t2<-list()
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

lf_deca       <-do.call(rbind.data.frame, lf.t2) %>% as.tibble
lf_deca$Date  <-as.POSIXct(lf_deca$Date,format="%Y-%m-%d %H:%M") %>% as_datetime
lf_deca$Value <-lf5$Value %>% as.numeric
lf_deca       <-lf_deca %>% filter(!is.nan(Value))

saveRDS(lf_deca,paste0(MetricsDir,"Monalisa_NDVI.rds"))
if(writeCSV==T) write.csv(lf_deca,paste0(MetricsDir,"Monalisa_NDVI_2017.csv"))

#** Plot the Decagon NDVIs ----

g1<-ggplot(lf_deca,aes(x=Date,y=Value))+
  geom_point()+
  ggtitle("Decagon NDVI of 2017 - Unfiltered")+
  facet_grid(Station~.)

name.g1<-paste0(Monalisa17Dir,"Decagon_NDVI_2017_Unfiltered_All.png")
if(savePlot==T) ggsave(g1, filename = name.g1, device = "png", width = 7, height=11)

# 2. Filter ----

#* Time Filter ----
time<-c("1030","1500")
time.intable<-paste0(hour(lf_deca$Date),minute(lf_deca$Date))
wh<-which(time.intable>=time[1] & time.intable<=time[2])
filter.data1<-lf_deca %>% slice(wh) 
filterInp1<-filter.data1 %>% separate(.,Date,c("Date","Time")," ") %>% group_by(Date,Station)

#* Outlier Filter ----
test<-filterInp1 %>% nest %>% mutate(error2=map(data,function(dat){
  
  gDens <-.getDensityLimits(dat$Value)
  lof   <- dat %>% select(Value) %>% lofactor(., k=3)

  dat1   <- dat %>% 
    add_column(.,Lower=gDens[1]) %>% 
    add_column(.,Upper=gDens[4]) %>% 
    add_column(.,LOF=lof)
  
  return(dat1)

}))

test.new<-test %>% select(-data) %>% unnest

filter.data2<-test.new %>% 
  filter(Value>Lower) %>% 
  filter(Value<Upper) %>% 
  filter(LOF<2)

step1<-filter.data1 %>% separate(.,Date,c("Date","Time")," ")
step1$Time<-as.hms(step1$Time)
step2<-filter.data2

step11<-step1 %>%  group_by(Date) %>% nest %>% slice(dates.day[[2]]) %>% unnest
step21<-step1 %>%  group_by(Date) %>% nest %>% slice(dates.day[[2]]) %>% unnest

f<-min(step11$Date) %>% str_replace_all(.,"-","")
t<-max(step11$Date) %>% str_replace_all(.,"-","")

g4<-ggplot(step11,aes(x=Time,y=Value,color=Date))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  scale_x_time(limits=c(3*3600,18*3600))+
  facet_grid(Station~.)

g5<-ggplot(step21,aes(x=Time,y=Value,color=Date))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  scale_x_time(limits=c(3*3600,18*3600))+
  facet_grid(Station~.)

ggsave(g4,filename = paste0(Monalisa17Dir,"Decagon_NDVI_",f,"_to_",t,"Filter1.png"),device = "png",width = 7,height=11)
ggsave(g5,filename = paste0(Monalisa17Dir,"Decagon_NDVI_",f,"_to_",t,"Filter2.png"),device = "png",width = 7,height=11)

#* Max Value Filter ----
station<-"vimes1500"

summary<-filter.data2 %>% 
  group_by(Date,Station) %>% 
  nest %>% 
  mutate(filtered = map(data, ~ filter(., Value==max(Value))))
summary$Date<-as_date(summary$Date)

summary2<-summary %>% select(-data) %>% unnest 
summary3<-summary2 %>% filter(Station==station)

#* Moving Window Filter ----
start<-"2017-01-01" %>% as_date
end  <-max(summary3$Date)
date.range<-seq(start,end,1)
movingWinL<-7
tbl<-list()

for(i in 1:length(date.range)){

    date1<-date.range[i]
    date2<-seq(date1,date1+movingWinL-1,1)
    
    summary4<- summary3 %>% 
      filter(is.element(Date,date2)) %>% 
      group_by(Date,Station) %>% nest %>% 
      mutate(new=map(data,function(i) i[1,])) %>% 
      select(-data) %>% unnest

    if(nrow(summary4)>2){
      mp<-map(2:5,function(i) lofactor(summary4$Value, k=i)) %>% do.call(cbind,.) %>% rowMeans()
      tbl[[i]]<-as.tibble(summary4$Date) %>% add_column(LOF=mp)
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
  
# 2. Filter ----

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
