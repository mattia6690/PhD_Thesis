# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")
#source("R/RemSen/S2_ExtractMetrics2_Iteration.R")
writeCSV=T
suffix<-"050718"

ml1<-readRDS(paste0(MetricsDir,"Sentinel2_metrics_",suffix,".rds")) %>% add_column(Tdiff=0)
ml2<-readRDS(paste0(MetricsDir,"Sentinel2_metrics_points_",suffix,".rds"))
ml<-rbind(ml1,ml2) %>% arrange(Date,Station) %>% add_column(Date2={.$Date+.$Tdiff},.after="Date")

ml<-readRDS(paste0(Workspacedir,"07_Products/Metrics_S2_Station_Selection_",suffix,".rds"))


ml.mask<-ml %>% filter(OP2=="Mask") %>% filter(OP3=="MaskedPercent") 
ml.mask.good<-ml.mask %>% group_by(Station,Date.Sen) %>% dplyr::summarize(mean=100-mean(Value,na.rm=T)) %>% filter(mean>90)


g1<-ggplot(ml.mask,aes(Date.Sen,mean))+geom_bar(stat="identity")+facet_wrap(.~Station)


ml.ndvi<-ml %>% filter(OP2=="NDVI") %>% filter(!is.na(Value))
ml.ndvi.Nosd<- ml.ndvi %>% filter(OP3!="Sd")
ml.ndvi.Date<- ml.ndvi.Nosd %>% filter(!is.na(Date)) %>% mutate(Tdiff=abs(Date-Date.Sen)) %>% 
  group_by(Station,Date,OP1) %>% nest

x<-ml.ndvi.Date$data[[1]]

ml.ndvi.min<-ml.ndvi.Date %>% 
  mutate(MinDiff=map(data,function(x){return(x %>% arrange(Tdiff) %>% slice(1))})) %>% 
  select(-data) %>% 
  unnest


g1<-ggplot(ml.ndvi.min,aes(Date.Sen,Value))+geom_point()+facet_wrap(.~Station)

# Digit the Treshold for Masking & The Local outlier Function
tres.mask<-25
tres.lof<-2

# 2. Filter the Images ----
# Unique Inputs
ml.unq<-ml$Station %>% unique
dat.unq<-ml$Date %>% unique
ml_out1<-list()
ml_out2<-list()

for(i in 1:length(ml.unq)){
  
  #* 2.1. Masked Data by Station ----
  stat<-ml.unq[i]
  ml1.stat<- ml %>% filter(Station==stat)
  ml2.mask<- ml1.stat %>% filter(OP3=="MaskedPercent")
  ml3.sele<-ml2.mask %>% filter(Value<tres.mask) %>% select(Date,Station)
  ml4.filt<-ml1.stat %>% slice({which(!is.element(Date2,ml3.sele$Date))})
  ml5.ndvi.all<- ml4.filt %>% filter(OP2=="NDVI")
  ml5.ndvi.val<- ml5.ndvi.all %>% filter(OP3!="Sd")
  
  test<-ml5.ndvi.val %>% group_by(Date) %>% nest
  
  t<-test %>% mutate(cluster=map(data,function(k){
    
    val<-select(k,Value) %>% unlist
    if(length(val)>2){ 
      
      km<-kmeans(val,centers = 2)$cluster 
      
    } else { km<-rep(2,length(val))}
    return(km)
    
  })) %>% unnest
  
  t1<- t %>% filter(cluster==2)
  ggplot(t1,aes(Date2,Value,group=Date2))+geom_boxplot()
  
  
  t2<-t1 %>% select(-cluster) %>% group_by(Date,Date2,Station,Scale1,Scale2,OP1,OP2,OP3) %>% slice(which.min(Tdiff))
  ggplot(t2,aes(Date2,Value,group=Date2))+geom_boxplot()
  g2<-ggplot(t2,aes(Date2,Value,color=OP1))+geom_point()+geom_line()
  
  tileml<-ml %>% filter(Station=="Tile") %>% filter(OP3=="MaskedPercent")
  ml_out1[[i]]<-rbind(tileml,ml3)
  
  #* 2.2. Delete by Masked Pixel ----
  wh<-ml3$Date[which(ml3$Value>tres.mask)] %>% table
  wh<-which(wh>=1) %>% as.data.frame() %>% rownames()
  wh<-which(is.element(ml2$Date,as.Date(wh)))
  ml4<-ml2[-wh,]
  
  #* 2.3. Delete by Stdev ----
  ml5<-ml4 %>% 
    filter(OP1=="Shapefile",OP2=="NDVI",OP3=="Stdev") %>% 
    filter(!is.na(Value))
  
  ml5_lof<-ml5 %>% 
    select(Value) %>% 
    lof(.,c(2:5)) %>% rowMeans(.)
  
  ml5_date<-ml5 %>% 
    .[which(ml5_lof<tres.lof),] %>% 
    .[["Date"]]
  
  wh<-which(is.element(ml4$Date,ml5_date))
  ml5<-ml4[wh,]
  
  #* 2.4. Filtered List ----
  ml6<-ml5 %>% filter(OP2=="NDVI") %>% filter(OP3=="Absolute"|OP3=="Mean") %>% na.omit
  ml_out2[[i]]<-ml6
  
}

out1<-do.call(rbind,ml_out1)
saveRDS(out1, paste0(MetricsDir,"S2_MaskingValues_",suffix,".rds"))
if(writeCSV==T) write.csv(out1, paste0(MetricsDir,"S2_MaskingValues_",suffix,".csv"))
  
  

out2<-do.call(rbind,ml_out2)
saveRDS(out2, paste0(MetricsDir,"S2_Filtered_NDVI_",suffix,".rds"))
if(writeCSV==T) write.csv(out2, paste0(MetricsDir,"S2_Filtered_NDVI_",suffix,".csv"))

closeAllConnections() # Lof Function


ggplot(ml6,aes(Date,Value/10000))+
  geom_point(aes(color="Filtered Time Series"))+
  geom_line(aes(color="Filtered Time Series"))+
  ylim(c(-1,1))+
  labs(y="NDVI",color="S2 Images")+
  ggtitle("Filtered NDVI Time Series by Sentinel 2")+
  geom_point(data=ml3,aes(Date,0,color="Available Images"))+
  theme(legend.position = c(0.8, 0.1))
