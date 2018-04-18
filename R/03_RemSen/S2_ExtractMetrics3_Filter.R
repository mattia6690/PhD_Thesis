# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")
#source("R/RemSen/S2_ExtractMetrics2_Iteration.R")
writeCSV=T
suffix<-"151217"
ml<-readRDS(paste0(MetricsDir,"Sentinel2_metrics_",suffix,".rds"))
ml$Value<-ml$Value %>% as.numeric()
ml$Date<-ml$Date %>% as.Date(.,format="%Y%m%d")

# Digit the Treshold for Masking & The Local outlier Function
tres.mask<-50
tres.lof<-2

# 2. Filter the Images ----
# Unique Inputs
ml.unq<-ml$Station %>% unique %>% .[-1] # Tile Deleted
dat.unq<-ml$Date %>% unique
ml_out1<-list()
ml_out2<-list()

for(i in 1:length(ml.unq)){
  
  #* 2.1. Masked Data by Station ----
  stat<-ml.unq[i]
  ml2<- ml %>% filter(Station==stat)
  ml3<- ml2 %>% filter(OP3=="MaskedPercent") %>% filter(OP1=="Buffer"|OP1=="Shapefile"|OP1=="Point")
  
  ml2.ndvi<- ml2 %>% filter(OP1=="Shapefile",OP2=="NDVI",OP3=="Mean")
  
  
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
