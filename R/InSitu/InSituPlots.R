#################
# InSituPlotter
#################

### 01. Read ###

lf<-list.files(paste0(getwd(),"/07_FieldCampaign17/02_Station/"),pattern=".csv",full.names=T)
lf_short<-list.files(paste0(getwd(),"/07_FieldCampaign17/02_Station/"),pattern=".csv")

stats<-do.call(rbind,str_split(lf_short,"_"))[,1]
oi<-do.call(rbind,lapply(lf,read_csv))
oi$Date<-as.Date(sprintf("%06s",oi$Date),"%d%m%y")

aggr<-oi %>% group_by(FOI,Date,SubID)

# Plot configs
startdsipdate <-as.Date("2017-05-01")
enddispdate   <-as.Date("2017-08-01")

### 02. Plots 1 ####
## 2.1. GEneral Plot of all Metrics ##
j=stats[2]
for(j in stats){
  
  #Denominate
  str1<-"LAI"
  str2<-"Biomass"
  str3<-"Soil Water"
  str4<-"NDVI"
  str4b<-"PRI"
  
  #Aggregate...
  All   <- oi  %>% group_by(Date,SubID)%>% filter(FOI==j)
  LAI   <- All %>% summarize(Mean=mean(LAI,na.rm=T),Se=se(LAI)) 
  SW    <- All %>% summarize(Mean=mean(SW_perc,na.rm=T),Se=se(SW_perc))
  Hyper <- All %>% summarize(
    Mean_NDVI=mean(NDVI2,na.rm=T),
    Mean_PRI=mean(PRI1,na.rm=T),
    Se_NDVI=se(NDVI2),
    Se_PRI=se(PRI1))
  
  ###... and plot
  # LAI
  LAIp<-ggplot(LAI,aes(x=Date,y=Mean,color=SubID))+ ylab(str1)+
    geom_line(size=1.2)+geom_point()+
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

  # Boxplots
  
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


ggplot(aggr,aes(x=Date,y=LAI,group=FOI))+
  geom_boxplot()


## 2.2 Comparison of PRI and NDVI
g0<-ggplot(oi,aes(x=PRI2,y=PRI1))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral PRI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("PRI +- 2nm, DECAGON1")+xlab("PRI +-12nm, DECAGON2")
g1<-ggplot(oi,aes(x=NDVI2,y=NDVI1))+geom_point(alpha=.3)+
  ggtitle("Comparison of Hyperspectral NDVI")+
  geom_abline(color="brown",linetype="dotted")+
  geom_smooth(method="lm",se=F,linetype="dotted")+
  ylab("NDVI +- 2nm, DECAGON1")+xlab("PRI +- 12nm, DECAGON2")
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

ggsave(paste0("09_Visualization/PRI_NDVI_Comparison.png"),plot=ga1)


1


### 03. Plots 2 GPS ####

load("07_FieldCampaign17/02_Station/GPS.RData") # gps

gps %>% filter(Stat=="Domef1500")

