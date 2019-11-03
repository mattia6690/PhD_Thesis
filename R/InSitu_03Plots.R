###########################'
#' 00. In Situ Plot 1 Script
#'
#' This script allows plot the outputs of the InSituReader.R script in different ways:
#' * Linear plots of the time series
#' * Boxplot representations of those time series
#' * Combined plot of PRI and NDVI in different wavelengths
#' 
###########################'
source("R/BaseFunctions.R")

### 1. Read Data ----

aggr2<-read_csv(file = paste0(MetricsDir,"InSituMetrics.csv"))
stats<-aggr2 %>% select(FOI) %>% unique %>% unlist %>% as.character

# Plot configs
startdsipdate <-as.Date("2017-05-01")
enddispdate   <-as.Date("2017-10-15")

### 2. Plot Data ----
for(j in stats){
  
  #Denominate
  str1 <-"LAI"
  str2 <-"Biomass"
  str3 <-"Soil Water"
  str4 <-"NDVI"
  str4b<-"PRI"
  
  #Aggregate...
  All   <- aggr2  %>% group_by(Date,SubID)%>% filter(FOI==j)
  LAI   <- All %>% summarize(Mean=mean(LAI,na.rm=T),Se=se(LAI)) 
  SW    <- All %>% summarize(Mean=mean(SW_perc,na.rm=T),Se=se(SW_perc))
  Hyper <- All %>% summarize(
    Mean_NDVI=mean(NDVI2,na.rm=T),
    Mean_PRI=mean(PRI1,na.rm=T),
    Se_NDVI=se(NDVI2),
    Se_PRI=se(PRI1))
  
  #* 2.1. General LinePlot of all Metrics ----
  # LAI
  LAIp<-ggplot(LAI,aes(x=Date,y=Mean,color=SubID))+ ylab(str1)+
    geom_line(size=1.2)+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-Se,ymax=Mean+Se),width=.3)+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str1,"over time of",j,"Station - 2017"))
  
  # SW
  SWp<- ggplot(SW,aes(x=Date,y=Mean,color=SubID))+
    geom_line(size=1.2)+
    geom_point()+
    geom_errorbar(aes(ymin=Mean-Se,ymax=Mean+Se),width=.3)+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str3,"over time of",j,"Station - 2017"))
  
  # Hyper
  Hyperp<- ggplot(Hyper,aes(x=Date,y=Mean_NDVI,color=SubID))+
    geom_line(size=1.2)+
    geom_point()+
    geom_errorbar(aes(ymin=Mean_NDVI-Se_NDVI,ymax=Mean_NDVI+Se_NDVI),width=.3)+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    scale_y_continuous(limits=c(.3,1))+
    ggtitle(paste(str4,"over time of",j,"Station - 2017"))  
  
  # Biomass
  BIOp<-ggplot(data=subset(All,!is.na(BioWet)),aes(x=Date,y=BioWet,color=SubID))+
    geom_line(size=1.2)+ geom_point()+
    geom_errorbar(aes(ymin=BioWet-MeanErr,ymax=BioWet+MeanErr),width=.3)+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str2,"over time of",j,"Station - 2017"))

  #* 2.2. General BoxPlot of all Metrics ----
  
  LAIbox<-ggplot(All,aes(x=Date,y=LAI,group=Date))+ ylab(str1)+
    geom_boxplot()+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str1,"over time of",j,"Station - 2017"))
  SWbox<-ggplot(All,aes(x=Date,y=SW_perc,group=Date))+ ylab(str3)+
    geom_boxplot()+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str3,"over time of",j,"Station - 2017"))
  NDVIbox<-ggplot(All,aes(x=Date,y=NDVI1,group=Date))+ ylab(str4)+
    geom_boxplot()+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str4,"over time of",j,"Station - 2017"))
  BIObox<-ggplot(All,aes(x=Date,y=BioWet,group=Date))+ ylab(str2)+
    geom_boxplot()+
    scale_x_date(limits=c(startdsipdate,enddispdate))+
    ggtitle(paste(str2,"over time of",j,"Station - 2017"))
  
  
  # Save
  ggsave(paste0("09_Visualization/Station_Metrics/",j,"_",str1,".png"),plot=LAIp,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Boxplots/",j,"_",str1,".png"),plot=LAIbox,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Metrics/",j,"_",str3,".png"),plot=SWp,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Boxplots/",j,"_",str3,".png"),plot=SWbox,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Metrics/",j,"_",str4,".png"),plot=Hyperp,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Boxplots/",j,"_",str4,".png"),plot=NDVIbox,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Metrics/",j,"_",str2,".png"),plot=BIOp,device="png",width = 15.2,height = 7.71)
  ggsave(paste0("09_Visualization/Station_Boxplots/",j,"_",str2,".png"),plot=BIObox,device="png",width = 15.2,height = 7.71)

}

#* 2.3 Comparison of PRI and NDVI ----

g0<-ggplot(oi,aes(x=PRI2,y=PRI1))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral PRI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("PRI +- 2nm, DECAGON1")+xlab("PRI +-12nm, DECAGON2")
g1<-ggplot(oi,aes(x=NDVI2,y=NDVI1))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral NDVI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("NDVI +- 2nm, DECAGON1")+xlab("NDVI +- 12nm, DECAGON2")
g2<-ggplot(oi,aes(x=NDVI4,y=NDVI1))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral NDVI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("NDVI +- 2nm, DECAGON1")+xlab("NDVI +- 30nm & 115nm, SENTINEL2")
g3<-ggplot(oi,aes(x=NDVI4,y=NDVI2))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral NDVI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("NDVI +- 12nm, DECAGON2")+xlab("NDVI +- 30nm & 115nm, SENTINEL2")

ga1<-grid.arrange(g0,g1,g2,g3, nrow=2, ncol=2,top="Comparison of the different PRI/NDVI Computations")
ggsave(plot = ga1,paste0(InSitu_dir,"09_Visualization/PRI_NDVI_Comparison.png"),device = "png",width = 10,height = 7.71)

####* 2.4 Heatmap plot of the Pearson Correlation (overall) ----
aggr3 <-aggr2 %>% dplyr::select(.,c(PRI1:SW_perc)) %>% select(c(PRI1,NDVI1,LAI,BioWet,BioWatRawPerc,SW_perc))
aggr3cor<-aggr3 %>% cor(.,use="pairwise.complete.obs") %>% round(.,2) %>% get_upper_tri %>% melt(.,na.rm=T)

corplot<-ggplot(data = aggr3cor, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value))+
  scale_fill_gradient2(low = "blue", high = "green", mid = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  ggtitle("Pearson Correlation Heatmap in 2017 Field Campaign")

ggsave(paste0(InSitu_dir,"09_Visualization/Correlation_heatmap_all.png"),width = 10,height = 7.71)

## 2.3 Heatmap plot of the Pearson Correlation (byStation)
fois<-unique(aggr2$FOI)
for(foi1 in fois){
  
  aggr3 <-aggr2 %>% filter(FOI==foi1) %>% select(.,c(PRI1:SW_perc)) %>% select(c(PRI1,NDVI1,LAI,BioWet,BioWatRawPerc,SW_perc))
  aggr3cor<-aggr3 %>% cor(.,use="pairwise.complete.obs") %>% round(.,2) %>% get_upper_tri %>%  melt(.,na.rm=T)
  
  corplot<-ggplot(data = aggr3cor, aes(x=Var2, y=Var1, fill=value)) + 
    geom_tile(color = "white")+
    geom_text(aes(Var2, Var1, label = value))+
    scale_fill_gradient2(low = "blue", high = "green", mid = "red", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()+
    ggtitle(paste0("Pearson Correlation Heatmap in 2017 Field Campaign ",foi1))
  
  ggsave(paste0(InSitu_dir,"09_Visualization/Correlations/Correlation_heatmap_",foi1,".png"),width = 10,height = 7.71)
  
}

## 3. Boxplot with all Stations combined ----

#aggr_alpenv<-aggr2 %>% filter(FOI=="Vimes1500"| FOI=="P2" | FOI=="Vimef2000")
aggr_alpenv<-aggr2 %>% filter(FOI=="Domef1500"|FOI=="Domef2000")
alpOUT<-paste0(InSitu_dir,"09_Visualization/Station_Combo/")

str<-list("LAI","NDVI","Wet biomass", "Biomass Water percentage","Soil water content")
column<-list("LAI","NDVI1","BioWet","BioWatRawPerc","SW_perc")
ylims<-list(c(0,7),c(0,1),c(0,700),c(0,100),c(0,100))
list(str,column,ylims)

pmap(list(str,column,ylims),function(j,k,v,dat=aggr_alpenv,dir=alpOUT){
  
  tst<-ggplot(dat,aes(x=Date,y=get(k),group=interaction(Date,FOI),fill = FOI))+
    geom_boxplot()+
    ylab(j)+
    ylim(c(v[1],v[2]))+
    ggtitle(paste0("Field Campaign 2017 - ",j))
  ggsave(plot=tst,filename = paste0(dir,k,".png"),device="png",width = 15.2,height = 7.71)
  
})

#* 3.1. Boxplot for AlpEnv ----

aggr_alpenv<-aggr %>% filter(FOI=="Vimes1500"| FOI=="P2")

str<-"LAI over time"
tst<-ggplot()+
  geom_boxplot(data=aggr_alpenv,aes(x=Date,y=LAI,group=interaction(Date,FOI),fill = FOI))+ 
  scale_fill_manual(values = c("goldenrod", "forestgreen"))+
  scale_x_date()+
  ylim(c(0,7))+
  ggtitle(paste0(str," - Field Campaign 2017"))+
  xlim(startdsipdate,enddispdate)
ggsave(paste0(InSitu_dir,"09_Visualization/Station_Combo/",str,".png"),
       plot=tst,device="png",width = 15.2,height = 7.71)

str<-"NDVI over time"
tst<-ggplot()+
  geom_boxplot(data=aggr_alpenv,aes(x=Date,y=NDVI1,group=interaction(Date,FOI),fill = FOI))+ 
  scale_fill_manual(values = c("goldenrod", "forestgreen"))+
  scale_x_date()+
  ylim(c(0,1))+
  ggtitle(paste0(str," - Field Campaign 2017"))+
  xlim(startdsipdate,enddispdate)

ggsave(paste0(InSitu_dir,"09_Visualization/Station_Combo/",str,".png"),
       plot=tst,device="png",width = 15.2,height = 7.71)

str<-"Biomass Water Percentage over Time"
tst<-ggplot()+
  geom_boxplot(data=aggr_alpenv,aes(x=Date,y=BioWatRawPerc,group=interaction(Date,FOI),fill = FOI))+ 
  scale_fill_manual(values = c("goldenrod", "forestgreen"))+
  scale_x_date()+
  ylim(c(0,100))+
  ggtitle(paste0(str," - Field Campaign 2017"))
ggsave(paste0(InSitu_dir,"09_Visualization/Station_Combo/",str,".png"),
       plot=tst,device="png",width = 15.2,height = 7.71)

str<-"Wet Biomass Extraction over time"
tst<-ggplot()+
  geom_boxplot(data=aggr_alpenv,aes(x=Date,y=BioWet,group=interaction(Date,FOI),fill = FOI))+ 
  scale_fill_manual(values = c("goldenrod", "forestgreen"))+
  scale_x_date()+
  ylim(c(0,800))+
  ggtitle(paste0(str," - Field Campaign 2017"))
ggsave(paste0(InSitu_dir,"09_Visualization/Station_Combo/",str,".png"),
       plot=tst,device="png",width = 15.2,height = 7.71)


# 4. Per Station Statistics ----

alpOUT<-paste0(InSitu_dir,"09_Visualization/Station_AllYear/")

#* 4.1 Per Station ----

dates<- aggr2 %>% select(Date,FOI) %>% distinct %>% arrange(Date)
dates$Date<-as.character(dates$Date)
table.png(dates,"Dates_and_Stations",dir=alpOUT,res=200)

bS<-aggr2 %>% filter(!is.na(NDVI1)) %>% select(FOI) %>% 
  table %>% as.tibble %>% setNames(.,c("FOI","Spectrometer"))

bS<-aggr2 %>% filter(!is.na(LAI)) %>% select(FOI) %>% 
  table %>% as.tibble %>% setNames(.,c("FOI","LAI2200")) %>% right_join(bS,.,"FOI")

bS<-aggr2 %>% filter(!is.na(BioWet)) %>% select(FOI) %>% 
  table %>% as.tibble %>% setNames(.,c("FOI","Phytomass Samples")) %>% right_join(bS,.,"FOI")

bS<-aggr2 %>% filter(!is.na(SW_perc)) %>% select(FOI) %>%
  table %>% as.tibble %>% setNames(.,c("FOI","Soil Water Content")) %>% right_join(bS,.,"FOI")

newrow<-c("ALL",sum(bS$Spectrometer),sum(bS$LAI2200),sum(bS$`Phytomass Samples`),sum(bS$`Soil Water Content`))
bS<-rbind.data.frame(bS,newrow)
table.png(bS,"Station_Measurements_Approach_2017",dir=alpOUT,res=200)

#* 4.2 Per Station AND Parameter  ----
Stationtab<-aggr2 %>% 
  group_by(FOI) %>% 
  dplyr::summarise(
    MeanWaterPerc=mean(BioWatRawPerc,na.rm=T),StdevWaterPerc=sd(BioWatRawPerc,na.rm=T),
    MeanBiomass=mean(BioWet,na.rm=T),StdevBiomass=sd(BioWet,na.rm=T),
    MeanNDVI=mean(NDVI1,na.rm=T),StdevNDVI=sd(NDVI1,na.rm=T),
    MeanLAI=mean(LAI,na.rm=T),StdevLAI=sd(LAI,na.rm=T),
    MeanSWC=mean(SW_perc,na.rm=T),StdevSWC=sd(SW_perc,na.rm=T))

table.png(Stationtab,"Station_metrics_2017_mean",dir=alpOUT,res=200)

