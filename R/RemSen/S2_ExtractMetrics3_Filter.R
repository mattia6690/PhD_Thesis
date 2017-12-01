# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")
source("R/RemSen/S2_ExtractMetrics1_Initialization.R")
source("R/RemSen/S2_ExtractMetrics2_Iteration.R")


ml<-readRDS(paste0(MetricsDir,"Sentinel2_NDVI_metrics.rds"))

# Digit the Treshold for Masking & The Local outlier Function
tres.mask<-75
tres.lof<-1.5

# Allow the Plotting? 1= Cloud, 2= Filtered Image
plot1=F
plot2=F

# 2. Tidy Metrics ----

# Unique Inputs
ml.unq<-ml$Station %>% unique %>% .[-1] # Tile Deleted
dat.unq<-ml$Date %>% unique

i=ml.unq[10]

for(i in ml.unq){
  
  #* 2.1. Data by Station ----
  ml2<- ml %>% filter(Station==i)
  ml3<- ml2 %>% filter(OP3=="MakedPercent") %>% filter(OP1=="Buffer"|OP1=="Shapefile")
  
  tileml<-ml %>% filter(Station=="Tile") %>% filter(OP3=="MakedPercent")
  ml3_tile<-rbind(tileml,ml3_mask_B)
  
  #* 2.2. Cloud Coverage Plots ----
  # Per Date and Station
  if(plot1==T){
    g1<-ggplot(ml3_tile,aes(x=as.Date(Date,format="%Y%m%d"),y=as.numeric(Value)))+
    geom_point()+
    geom_vline(aes(xintercept=as.Date(Date,format="%Y%m%d")),linetype=2,color="grey")+
    geom_point(aes(color=OP1),size=2)+
    xlab("Date")+ylab("Percentage")+
    ggtitle(paste("Percentage of Masked Pixels in Sentinel2 MSI calculated NDVI maps for",i,"site"))+
    scale_color_manual("Scale",values=c("dodgerblue2","dodgerblue4","deepskyblue"))+
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b-%y"))
    
    ggsave(g1,filename = paste0(sentineldir,"Clouds/CloudCoverage_",i,".png"),device="png",width=10,height=6)
  }
  
  #* 2.3. Delete by Masked Pixel ----
  wh<-ml3$Date[which(ml3$Value %>% as.numeric >tres.mask)] %>% table
  wh<-which(wh>=2) %>% as.data.frame() %>% rownames()
  wh<-which(is.element(ml2$Date,wh))
  ml4<-ml2[-wh,]
  
  #* 2.4. Delete by Stdev ----
  ml5<-ml4 %>% 
    filter(OP1=="Shapefile",OP2=="NDVI",OP3=="Stdev") %>% 
    filter(!is.na(Value))
  
  ml5_lof<-ml5 %>% 
    select(Value) %>% 
    as.matrix %>% as.numeric() %>% 
    lof(.,c(2:(length(.)-2)),cores=1) %>% rowMeans(.)
  
  ml5_date<-ml5 %>% 
    .[which(ml5_lof<tres.lof),] %>% 
    .[["Date"]]
  
  wh<-which(is.element(ml4$Date,ml5_date))
  ml5<-ml4[wh,]
  
  #* 2.4. Plot Filtered Image ----
  ml6<-ml5 %>% filter(OP2=="NDVI") %>% filter(OP3=="Absolute"|OP3=="Mean") %>% na.omit
  
  if(plot2==T){
    ggplot(ml6,aes(x=as.Date(Date,format="%Y%m%d"),y=as.numeric(Value)))+
    geom_point(aes(color=OP1))+
    ylab("NDVI value")+xlab("Date")+ ylim(0,10000)+
    labs(title=paste0("Sentinel 2 NDVI Mean, 2017 in Station ",i))+
    scale_x_date(breaks = date_breaks("months"), labels = date_format("%b %y"))
    
    ggsave(g1,filename = paste0(sentineldir,"Metrics/S2_NDVI_2017_",i,"_nopoint.png"),device="png",height = 7,width=14)
  }  
}