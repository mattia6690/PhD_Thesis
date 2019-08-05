
readSWCAlpEnv<-function(table,Station="",headRows,headNames,TimeCol,StartCol){
  
  Time    <- table[headRows+1:length(table$X1),TimeCol]
  TimeCHR <- map(Time,function(x) as_datetime(as.character(x),format="%Y-%m-%d %H:%M"))
  
  data.seqC<-c(1+StartCol:ncol(table)-1)
  data.seqR<-c(headRows+1:nrow(table))
  
  head<- table[1:headRows,startcol:ncol(table)]%>% 
    t %>% 
    as_tibble %>% 
    setNames(headNames) %>% 
    add_column(ID=c(1:nrow(.)),.before = T)
  
  
  data<- head %>% 
    mutate(Data=map(ID,function(x,t=table,hr=data.seqR,sc=data.seqC,time=TimeCHR){
      
      ret <- t[hr,sc[x]]
      ret <- as_tibble(cbind(time,ret))
      ret <- setNames(ret,c("TimeStamp","Value"))
      ret <- mutate(ret,Value=as.numeric(Value))
      return(ret)
      
    }))
  
  meta<- data %>% 
    add_column(Station=Station,.before = T) %>% 
    unnest
  
  return(meta)
  
}

headRows<-3
timecol <-1
startcol<-2
headNames<-c("Key","Unit","Statistic")

b2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/B2_2017-2018.csv",skip = 1,col_names = F)
p2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/P2_2017-2018.csv",skip = 1,col_names = F)

t1<-readSWCAlpEnv(b2,Station = "Vimes1500",
                  headRows = 3, headNames = c("Key","Unit","Statistic"),
                  TimeCol = 1, StartCol = 2)

t2<-readSWCAlpEnv(p2,Station = "P2",
                  headRows = 3, headNames = c("Key","Unit","Statistic"),
                  TimeCol = 1, StartCol = 2)


complete<-rbind(t1,t2)

atib<-complete %>% 
  mutate(Date=date(TimeStamp)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Day=day(Date)) %>% 
  mutate(Value=as.numeric(Value))

atib.gr1<- atib %>% 
  group_by(Date,Station,Key,Statistic, Unit) %>% 
  summarize(Mean=mean(Value),
            Max=max(Value),
            Min=min(Value),
            Stdev=sd(Value),
            Sum=sum(Value)) %>% 
  ungroup

atib.all<- atib.gr1 %>% 
  mutate(MetricS=lapply(Key,function(x){
    
    sp <- strsplit(x,"_")[[1]]
    bind_cols(MetricS = sp[1], Plot = sp[3], Depth = paste(sp[4],"cm"), Statistic = sp[5])
    
  })) %>% 
  unnest %>% 
  mutate(MetricL=sapply(MetricS,function(x) ifelse(x=="SWC","Soil Water Content","Soil Water Potential")))


atib2.avg<-atib.all %>% filter(Statistic=="Avg")
ggplot(atib2.avg,aes(Date,Mean,pch=Station,linetype=Station))+ theme_bw()+
  geom_point(alpha=.5)+
  geom_line()+
  facet_wrap(.~Key,scales = "free")

saveRDS(object = atib.all,file = "C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/SWPSWC_Vinschgau_2017-2018_Daily.rds")

