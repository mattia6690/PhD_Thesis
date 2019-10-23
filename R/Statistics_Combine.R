

# Input -------------------------------------------------------------------
source("R/BaseFunctions.R")
library(readxl)
library(broom)
library(forecast)

stations<-c("Domef1500","Domef2000","Vimef2000","Vimes1500")
suffix<-"161019"

db<-readRDS(paste0(MetricsDir,"AllSensorData",suffix,".rds"))
db.all<-readRDS(paste0(MetricsDir,"AllSensorData",suffix,"_filtered.rds"))
db.mean<-readRDS(paste0(MetricsDir,"AllSensorData",suffix,"_meanDay.rds"))
gapfill.db<-readRDS(paste0(MetricsDir,"02_Gapfill/AllSensorData",suffix,"_gapFill.rds"))

Manage.Site<-readRDS(paste0(DataDir,"/Management/Management_2017_complete.rds"))
Manage.Plot<-readRDS(paste0(DataDir,"/Management/Management_2017_plot.rds"))


# Descriptive Statistics ---------------------------------------------------

site.stats<-db %>% 
  separate(Plot,c("Sti","Plot"),"_") %>% 
  filter(Plot=="Site") %>% 
  arrange(Date) %>% 
  group_by(Station,Sensor) %>% 
  dplyr::summarise(Mean=mean(Value),Min=min(Value),Max=max(Value)) %>% ungroup() %>% 
  gather(key=Statistics, value=Value,Mean,Max,Min) %>% 
  spread(.,key=Sensor,value=Value)

# Correlations DOY --------------------------------------------------------

names<-list(c("Spectrometer","Sentinel-2 MSI"),
            c("Spectrometer","SRS"),
            c("Spectrometer","Phenocam"),
            c("Phenocam","Sentinel-2 MSI"),
            c("Phenocam","SRS"),
            c("SRS","Sentinel-2 MSI"))

names2<-map(names,function(i) paste(i,collapse=" & ")) %>% unlist %>% c("Station","Plot",.)


db2<-gapfill.db %>% distinct %>% spread(Sensor,Value) %>% 
  filter(Date>="2017-03-01" & Date<"2017-11-01")

db3<-db2 %>% 
  group_by(Date,Station,Plot) %>% 
  dplyr::summarise('SRS' = mean(Decagon_SRS,na.rm=T),
                   'Sentinel-2 MSI' = mean(Sentinel2_MSI,na.rm=T),
                   'Phenocam' = mean(Phenocam,na.rm=T),
                   'Spectrometer' = mean(Spectrometer,na.rm=T)) %>% 
  group_by(Station,Plot) %>% 
  nest

d.nest<- db3 %>% 
  mutate(t1=map(data,function(x,y=names[[1]]) allbind(x,y[1],y[2]))) %>% 
  mutate(t2=map(data,function(x,y=names[[2]]) allbind(x,y[1],y[2]))) %>% 
  mutate(t3=map(data,function(x,y=names[[3]]) allbind(x,y[1],y[2]))) %>% 
  mutate(t4=map(data,function(x,y=names[[4]]) allbind(x,y[1],y[2]))) %>% 
  mutate(t5=map(data,function(x,y=names[[5]]) allbind(x,y[1],y[2]))) %>% 
  mutate(t6=map(data,function(x,y=names[[6]]) allbind(x,y[1],y[2]))) %>% 
  select(-data) %>% 
  setNames(names2) %>% 
  gather(.,key=Scales,value = Value,-c(Station,Plot)) %>% 
  unnest %>%  
  mutate(DOY=map_dbl(Date,yday)) %>% 
  separate(Plot,c("Sti","Plot"),"_")

saveRDS(d.nest,file = paste0(MetricsDir,"AllSensorData",suffix,"_CorrelationDOY.rds"))


# Correlations All ------------------------------------------------------------

d.nest<- db3 %>% 
  mutate(t1=map(data,function(x,y=names[[1]]) mod_fun(x,y[1],y[2]))) %>% 
  mutate(t2=map(data,function(x,y=names[[2]]) mod_fun(x,y[1],y[2]))) %>% 
  mutate(t3=map(data,function(x,y=names[[3]]) mod_fun(x,y[1],y[2]))) %>% 
  mutate(t4=map(data,function(x,y=names[[4]]) mod_fun(x,y[1],y[2]))) %>% 
  mutate(t5=map(data,function(x,y=names[[5]]) mod_fun(x,y[1],y[2]))) %>% 
  mutate(t6=map(data,function(x,y=names[[6]]) mod_fun(x,y[1],y[2]))) %>% 
  dplyr::select(-data) %>% 
  setNames(names2) %>% 
  gather(.,key=Scales,value = Value,-c(Station,Plot)) %>%
  mutate(Select=map_dbl(Value,function(x) ifelse(any(is.na(x)),0,1))) %>% 
  filter(Select==1) %>% 
  unnest %>% 
  tidyr::separate(.,Scales,into=c("Scale1","Scale2"),sep=" & ") %>% 
  dplyr::select(-Select)

cors<-d.nest  %>% 
  mutate(Stars=map_chr(p.value,function(x) {
    if(x>.1) {y<-"   "}
    if(x<.1) {y<-"*  "}
    if(x<.05){y<-"** "}
    if(x<.01){y<-"***"}
    return(y)
  })) %>% 
  mutate(R.squared=pmap_chr(list(r.squared,Stars,df.residual),function(x,y,z) paste0(round(x,2),y)))

plottab1<-cors %>% 
  rename(N=df.residual) %>% 
  dplyr::select(Station,Plot,Scale1,Scale2,N,R.squared)

plottab2<-plottab1 %>% 
  separate(.,col=Plot,into=c("Stat","ID"),sep="_") %>% 
  select(-Stat) %>% 
  spread(.,key=ID,value=R.squared)

saveRDS(plottab2,file = paste0(MetricsDir,"AllSensorData",suffix,"_CorrelationAll01.rds"))


# Cross Correlations ------------------------------------------------------

ccf.raw<- db3 %>% 
  mutate(t1=map(data,function(x,y=names[[1]]) ccf_fun(x,y[1],y[2]))) %>% 
  mutate(t2=map(data,function(x,y=names[[2]]) ccf_fun(x,y[1],y[2]))) %>% 
  mutate(t3=map(data,function(x,y=names[[3]]) ccf_fun(x,y[1],y[2]))) %>% 
  mutate(t4=map(data,function(x,y=names[[4]]) ccf_fun(x,y[1],y[2]))) %>% 
  mutate(t5=map(data,function(x,y=names[[5]]) ccf_fun(x,y[1],y[2]))) %>% 
  mutate(t6=map(data,function(x,y=names[[6]]) ccf_fun(x,y[1],y[2]))) %>% 
  select(-data) %>% 
  setNames(names2)

ccf<- ccf.raw %>% 
  gather(.,key=Scales,value = Value,-c(Station,Plot)) %>% 
  mutate(Select=map_dbl(Value,function(x) ifelse(any(is.null(x)),0,1))) %>% 
  filter(Select==1) %>% 
  unnest %>% 
  separate(.,Plot,into=c("Site","ROI")) %>% select(-Site) %>% 
  filter(ROI=="Site")

ccf.subset<-ccf %>% filter(!grepl("Spectrometer",Scales))

gg.ccf<-ggplot(ccf.subset,aes(Lags,R))+ theme_bw()+
  geom_bar(stat="identity",width = .2)+
  geom_point()+
  geom_vline(xintercept=0,linetype="dashed")+
  facet_grid(vars(Station),vars(Scales))+
  ylab("Correlation Coefficient (R)")+
  xlab("Lag (in Days)")

ggsave(gg.ccf,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_CCF_nospec.png",device = "png",width=12,height=8)
ggsave(gg.ccf,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_CCF_nospec.jpg",device = "jpeg",width=12,height=8)


# Plot: Differences ---------------------------------------------------

db.d1 <- db.all %>% 
  separate(.,Plot,into=c("Site","ROI")) %>% select(-Site) %>% 
  mutate(RCat=map_chr(ROI,function(x) {ifelse(!grepl("Site",x),"ROI","Site")})) %>% 
  group_by(Date,Station,Sensor) %>% nest

db.d2<-db.d1 %>% 
  mutate(Difference=map2(1:nrow(.),data,function(w,x){
    
    st<-x %>% filter(ROI=="Site") %>% select(Value) %>% unlist(use.names = F) %>% as.numeric %>% mean(.)
    pl.a<-x %>% filter(ROI!="Site") 
    
    if(nrow(pl.a)>1 & is.numeric(st)){
      
      c<-pl.a %>% mutate(Difference=map_dbl(Value,function(x,s=st) abs(x-st)))
      
    }else{c<-NA}
    return(c)
    
  }))

db.diff<-db.d2 %>% 
  mutate(Filt=map_lgl(Difference,is.tibble)) %>% 
  filter(Filt==T) %>% 
  select(-c(data,Filt)) %>% 
  unnest %>% 
  arrange(Station,Sensor,Date,ROI) %>% 
  mutate(DateDiff=map2(Date,Station,function(d1,y,md=Manage.Plot){
    
    f1<-filter(md,Station==y) %>% select(Date)
    f2<-f1 %>% 
      map(.,function(y,w=d1) difftime(w,y,units="days")) %>% 
      unlist %>% abs %>% min %>% as.numeric
    
  }))%>% 
  unnest

db.diff %>% group_by(Station,Sensor) %>% 
  dplyr::summarise(.,Mean=mean(Difference),Sd=sd(Difference),Min=min(Difference),Max=max(Difference))

gbp<-ggplot(db.diff,aes(Date,Difference,group=Date))+theme_bw()+
  geom_boxplot(aes(color=DateDiff))+
  facet_grid(vars(Sensor),vars(Station))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b %d")+
  ylab("Absolute NDVI Difference per Day")+
  scale_color_gradient2("Days from last Event",low="green",mid = "gold",high="brown")+
  theme(legend.position = "bottom")


ggsave(gbp,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/BoxplotDiffs_Stations.png",device = "png",width=10,height=7)


# Plot: Histograms ---------------------------------------------------

d<-db %>% group_by(Station,Sensor) %>% nest %>% 
  mutate(Dens=map_dbl(data,function(x) { 
    
    d<-density(x$Value)
    u<-cbind(d$x,d$y) %>% as.tibble() %>% 
       arrange(desc(V2)) %>% .$V1 %>% .[1]
    
    })) %>% select(-data)


h1<-ggplot(db,aes(Value))+theme_bw()+
  geom_histogram(aes(y=..density..), alpha=0.5,position="identity",bins = 100)+
  geom_density()+
  facet_grid(vars(Sensor),vars(Station),scales = "free")+
  ylab("Count")+xlab("NDVI")+
  scale_x_continuous(breaks=seq(-.5,1,.2))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_vline(data=d,aes(xintercept=Dens),linetype="dotted")


ggsave(h1,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/HistogramSites.png",device = "png",width=7,height=7)


# Plot: Combination ---------------------------------------------------

textcex<-17
colors1=c("red","green2","blue","black")
# * By Station -------------------------------------------------------------------

db.all2 <- db.all %>% separate(.,Plot,into=c("Site","Plot")) %>% filter(Plot=="Site")

g<-ggplot(db.all2,aes(Date,Value,color=Sensor))+ylim(c(-.5,1))+ theme_bw()+
  geom_point(alpha=.5,aes(pch=Sensor,size=Sensor))+
  geom_line(linetype=4,alpha=.5)+
  scale_x_date(limits=as.Date(c("2017-03-01","2017-11-01")),date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  geom_vline(data=Manage.Plot,aes(xintercept=Date,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  scale_shape_manual(values=c(0,1,2,18))+
  scale_size_manual(values=c(2,2,2,4))+
  scale_color_manual(values=colors1)+
  ylab("NDVI")

g1 <- g + facet_wrap(Station~.)+
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text  = element_text(size = textcex),
        strip.text.x = element_text(size = textcex),
        axis.text.x  = element_text(size = textcex),
        axis.text.y  = element_text(size = textcex),
        legend.title = element_text(size = textcex+3),
        axis.title = element_text(size = textcex+3))

ggsave(g1,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_Station.png",device = "png",width=14,height=10,limitsize=FALSE)

g2 <- g + facet_wrap(Station~.,ncol = 1)
ggsave(g2,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_Station_long.png",device = "png",width=5,height=15)

# Single Station

station<-"Domef2000"
dball_stat<-db.all2 %>% filter(Station==station)
Manage_stat<-Manage.Plot %>% filter(Station==station)

g.stat<-ggplot(dball_stat,aes(Date,Value,color=Sensor))+ylim(c(-.5,1))+ theme_bw()+
  geom_point(alpha=.5,aes(pch=Sensor,size=Sensor))+
  geom_line(linetype=4,alpha=.5)+
  scale_x_date(limits=as.Date(c("2017-03-01","2017-11-01")),date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position="bottom")+
  geom_vline(data=Manage_stat,aes(xintercept=Date,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  scale_shape_manual(values=c(0,1,2,18))+
  scale_size_manual(values=c(2,2,2,4))+
  scale_color_manual(values=colors1)+
  ylab("NDVI")

fname<-paste0("C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_Station_",station,".png")
ggsave(g.stat,filename = fname,device = "png",width=10,height=7,limitsize=FALSE)


# * By Plot -------------------------------------------------------------------

db.all2 <- db.all %>% separate(.,Plot,into=c("Site","Plot")) %>% select(-Site) %>% filter(Plot!="Site")

g.p<-ggplot(db.all2,aes(Date,Value,color=Sensor))+ theme_bw() +
  geom_point(alpha=.5,aes(shape=Sensor,size=Sensor))+
  geom_line(linetype=4,alpha=.5)+
  ylim(c(-.5,1))+ 
  facet_grid(vars(Station),vars(Plot)) + 
  scale_x_date(date_breaks = "2 months",date_labels = "%b %d")+
  scale_shape_manual(values=c(0,1,18))+
  scale_size_manual(values=c(2,2,4))+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  scale_color_manual(values=colors1[c(2,3,4)])+
  ylab("Normalized Difference Vegetation Index (NDVI) Signal")

g1.p <- g.p + 
  theme(legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.text  = element_text(size = textcex),
        strip.text.x = element_text(size = textcex),
        axis.text.x  = element_text(size = textcex),
        axis.text.y  = element_text(size = textcex),
        legend.title = element_text(size = textcex+3),
        axis.title = element_text(size = textcex+3))

ggsave(g1.p,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVI_Point.png",device = "png",width=14,height=10,limitsize=FALSE)

# * Combined -------------------------------------------------------------------

db.all2 <- db.all %>% separate(.,Plot,into=c("Site","Plot")) %>% select(-Site)

g1<-ggplot(db.all,aes(Date,Value,color=Sensor))+ theme_bw() +
  geom_point(alpha=.5,aes(shape=Sensor,size=Sensor))+
  geom_line(linetype=4,alpha=.5)+
  ylim(c(-.5,1))+ 
  facet_wrap(.~Plot) + 
  scale_x_date(date_breaks = "1 month")+
  scale_shape_manual(values=c(0,1,2,18))+
  scale_size_manual(values=c(2,2,2,4))+
  scale_color_manual(values=colors1)+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  theme(legend.position = "bottom")+
  
  ylab("Normalized Difference Vegetation Index (NDVI) Signal")


g2<- g1 + geom_vline(data=Manage.Plot,aes(xintercept=Date,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))
  
ggsave(g1,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVIall_Station_Point.png",device = "png",width=15,height=10)
ggsave(g2,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVIall_Station_Point_Harvest.png",device = "png",width=15,height=10)


# Plot: Correlation DOY ----------------------------------------------------

gcorr<-ggplot(d.nest,aes(Y,X))+ theme_bw()+
  geom_point(aes(pch=Plot,color=DOY))+
  facet_grid(vars(Station),vars(Scales),scales="free")+
  scale_color_gradientn(colors=terrain.colors(100))+
  ylab("NDVI")+xlab("NDVI")+
  geom_smooth(method="lm")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position="bottom")

ggsave(gcorr,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/NDVIall_byStat.png",device = "png",width=15,height=10)

# Acquisition Table -------------------------------------------------------

Acq<-c(16740,4283,70,5,16895,4556,122,7,16190,4681,70,2,16615,4587,70,12)

db1<-db %>% 
  filter(Date>="2017-03-01" & Date<="2017-11-01") %>% 
  arrange(Plot) %>% 
  tidyr::separate(.,Plot,c("Stat","ID"),"_") %>% 
  group_by(Station,ID,Sensor) %>% nest %>% 
  mutate(N=map_dbl(data,nrow)) %>% 
  select(-data) %>% 
  tidyr::spread(.,ID,value=N) %>% 
  add_column(Acq,.after="Sensor") %>% 
  rename('Raw Acquisitions'=Acq)

saveRDS(db1,file = paste0(MetricsDir,"AllData_byPlot.rds"))

# ACF ---------------------------------------------------------------------
maxgap<-50
lag<-c(1:maxgap)

gp1<-gapfill.db %>% group_by(Station,Sensor) %>% nest

Manage2<-Manage.Plot %>% 
  mutate(Start=map(Date,function(x,l=lag) x-l)) %>% 
  mutate(End=map(Date,function(x,l=lag) x+l)) %>% 
  mutate(Lag=rep(list(c(1:maxgap)),nrow(.))) %>% 
  unnest

Manage.Bef<-Manage2 %>% 
  select(-End) %>% 
  add_column(Period="Before") %>% 
  add_column(To=.$Date) %>% 
  rename(From=Start) %>% 
  select(Date,Period,From,To,Station,Event,Lag)

Manage.Aft<-Manage2 %>% 
  select(-Start) %>% 
  add_column(Period="After") %>% 
  add_column(From=.$Date) %>% 
  rename(To=End) %>% 
  select(Date,Period,From,To,Station,Event,Lag)

Manage.all<-rbind(Manage.Bef,Manage.Aft) %>% 
  arrange(desc(Station,Date)) %>% 
  mutate(Data=pmap(list(Station,From,To),function(x,y,z,g=gapfill.db) {
    f1<-g %>% filter(Date>y & Date<z & Station==x)
  }))

Manage.all.nest<-Manage.all %>% 
  unnest %>% 
  group_by(Date,Lag,Station,Event,Sensor) %>% 
  nest 

Manage.all.coefs<-Manage.all.nest %>% 
  mutate(coefs=map(data,function(x){
    
    x.bef<-x %>% 
      filter(Period=="Before") %>% 
      add_column(Iter=as.numeric(rownames(.)))
    
    x.aft<-x %>% 
      filter(Period=="After") %>% 
      add_column(Iter=as.numeric(rownames(.)))
    
    if(nrow(x.aft)>0 & nrow(x.bef)>0){
      
      # Slope
      if(nrow(x.bef)>1){
        lm1<-lm(Value~Iter,x.bef)
        interlm1<-lm1$coefficients[1] 
        slopelm1<-lm1$coefficients[2] 
      } else { interlm1<-slopelm1<-NA }
      
      if(nrow(x.aft)>1){
        lm2<-lm(Value~Iter,x.aft)
        interlm2<-lm2$coefficients[1]
        slopelm2<-lm2$coefficients[2]
      } else { interlm2<-slopelm2<-NA }
      
      if(!is.na(slopelm1) & !is.na(slopelm2)){
        
        p1<-mean(diff(predict(lm1)))
        p2<-mean(diff(predict(lm2)))
        slopecoef<-diff(c(p1,p2))
        
      } else { slopecoef=NA }
      
      if(!is.na(interlm1) & !is.na(interlm2)){
        
        intercoef<-diff(c(interlm1,interlm2))
        
      } else { intercoef=NA }
      
      # N
      n.b<-nrow(x.bef)
      n.a<-nrow(x.aft)
      n.all<-sum(n.b,n.a)
      
      # Mean and Standard Deviation Before
      if(nrow(x.bef)>0){
        
        mn.b<-mean(x.bef$Value)
        sd.b<-sd(x.bef$Value)
        
      } else { mn.b<-sd.b<- NA }
      
      # Mean and Standard Deviation After
      if(nrow(x.aft)>0){
        
        mn.a<-mean(x.aft$Value)
        sd.a<-sd(x.aft$Value)
        
      } else { mn.a<-sd.a<- NA }
      
      # Mean Differences
      if(!is.na(mn.b) & !is.na(mn.a)){
        
        diffmean<- diff(c(mn.b,mn.a)) %>% abs
        
      } else { diffmean<-NA }
      
      # Stdev Differences
      if(!is.na(sd.b) & !is.na(sd.a)){
        
        diffsd<- diff(c(sd.b,sd.a)) %>% abs
        
      } else { diffsd<-NA }
      
      # Combination
      lista<-tibble(
        N1=n.b,N2=n.a,
        Mean1=round(mn.b,4), Mean2=round(mn.a,4),MeanDiff=round(diffmean,4),
        Sd1=round(sd.b,4), Sd2=round(sd.a,4), SdDiff=round(diffsd,4),
        Slope1=round(slopelm1,4),Slope2=round(slopelm2,4),SlopeCoef=round(slopecoef,4),
        Inter1=round(interlm1,4),Inter2=round(interlm2,4), InterCoef=round(intercoef,4))
      
      return(lista)
      
    }
  }))

ManageList<-Manage.all.coefs %>% select(-data)
wh<-map_dbl(Manage.all.coefs$coefs,length) %>% {which(.==0)}
ManageList2<-ManageList[-wh,] %>% unnest %>% mutate(Difference=abs(SlopeCoef)*abs(MeanDiff))

ggplot(ManageList2,aes(x=Date,y=Difference,color=as.integer(Date)))+
  geom_point(aes(pch=Sensor),alpha=.5)+
  facet_grid(Station~.,scales="free")+
  scale_colour_gradientn("Dates",colours=br, labels=as.Date)+
  ggtitle("Difference in linear Slope and Mean NDVI by")

br<-brewer.pal(n = 11, name = "Paired")

library(viridis)
br<-colorRampPalette(c("darkgreen","green","orange","brown"))(100)


# ACF2 ---------------------------------------------------------------------
maxgap<-6
gp1<-gapfill.db %>% filter(Date>="2017-02-01"& Date<="2017-12-01")

# Test
# x <-gp1$Date[10]
# y <-gp1$Station[1]
# yy<-gp1$Plot[1]
# z <-gp1$Sensor[1]
# g=gp1
# l=maxgap


Manage.all.coefs<-gp1 %>% 
  mutate(coefs=pmap(list(Date,Station,Plot,Sensor),function(x,y,yy,z,g=gp1,l=maxgap){
    
    min<-x-l
    max<-x+l
    
    x.bef<-g %>%  
      filter(Date>=min & Date<=x) %>% 
      filter(Station==y) %>% 
      filter(Plot==yy) %>% 
      filter(Sensor==z) %>%
      add_column(Iter=as.numeric(rownames(.)))
    
    x.aft<-g %>% 
      filter(Date>=x & Date<=max) %>% 
      filter(Station==y) %>% 
      filter(Plot==yy) %>% 
      filter(Sensor==z) %>%
      add_column(Iter=as.numeric(rownames(.)))
    
    if(nrow(x.aft)>0 & nrow(x.bef)>0){
      
      # Slope
      if(nrow(x.bef)>1){
        lm1<-lm(Value~Iter,x.bef)
        interlm1<-lm1$coefficients[1] 
        slopelm1<-lm1$coefficients[2] 
      } else { interlm1<-slopelm1<-NA }
      
      if(nrow(x.aft)>1){
        lm2<-lm(Value~Iter,x.aft)
        interlm2<-lm2$coefficients[1]
        slopelm2<-lm2$coefficients[2]
      } else { interlm2<-slopelm2<-NA }
      
      if(!is.na(slopelm1) & !is.na(slopelm2)){
        
        p1<-mean(diff(predict(lm1)))
        p2<-mean(diff(predict(lm2)))
        slopecoef<-diff(c(p1,p2))
        
      } else { slopecoef=NA }
      
      if(!is.na(interlm1) & !is.na(interlm2)){
        
        intercoef<-diff(c(interlm1,interlm2))
        
      } else { intercoef=NA }
      
      # N
      n.b<-nrow(x.bef)
      n.a<-nrow(x.aft)
      n.all<-sum(n.b,n.a)
      
      # Mean and Standard Deviation Before
      if(nrow(x.bef)>0){
        
        mn.b<-mean(x.bef$Value)
        sd.b<-sd(x.bef$Value)
        
      } else { mn.b<-sd.b<- NA }
      
      # Mean and Standard Deviation After
      if(nrow(x.aft)>0){
        
        mn.a<-mean(x.aft$Value)
        sd.a<-sd(x.aft$Value)
        
      } else { mn.a<-sd.a<- NA }
      
      # Mean Differences
      if(!is.na(mn.b) & !is.na(mn.a)){
        
        diffmean<- diff(c(mn.b,mn.a)) %>% abs
        
      } else { diffmean<-NA }
      
      # Stdev Differences
      if(!is.na(sd.b) & !is.na(sd.a)){
        
        diffsd<- diff(c(sd.b,sd.a)) %>% abs
        
      } else { diffsd<-NA }
      
      # Combination
      lista<-tibble(
        N1=n.b,N2=n.a,
        Mean1=round(mn.b,4), Mean2=round(mn.a,4),MeanDiff=round(diffmean,4),
        Sd1=round(sd.b,4), Sd2=round(sd.a,4), SdDiff=round(diffsd,4),
        Slope1=round(slopelm1,4),Slope2=round(slopelm2,4),SlopeCoef=round(slopecoef,4),
        Inter1=round(interlm1,4),Inter2=round(interlm2,4), InterCoef=round(intercoef,4))
      
      return(lista)
      
    }
  }))

ManageList<-Manage.all.coefs %>% filter(Date>as.Date("2017-03-15")) %>% filter(Date<as.Date("2017-11-01"))
ManageList2<- ManageList %>% 
  unnest %>% 
  mutate(Difference=abs(SlopeCoef)*abs(MeanDiff)) %>% 
  mutate(DOY=yday(Date)) %>% 
  group_by(Date,DOY,Station,Scale1,Sensor) %>% 
  nest

ManageList2 <- ManageList2 %>% 
  mutate(Difference=map(data,function(x) mean(x$Difference))) %>% 
  select(-data) %>% unnest

ManageList2.norm<-ManageList2 %>% 
  group_by(Station,Scale1,Sensor) %>% 
  nest %>% 
  mutate(Normalized=map(data,function(d){
    
    x<- d$Difference
    n <- (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
    
    return(n)
    
  })) %>% unnest

br<-colorRampPalette(c("darkgreen","green","orange","brown"))(100)

Managements<-Manage.Plot %>% mutate(DOY=yday(Date)) %>% select(Station,DOY,Event)

# Facet by Sensor
g1<-ggplot(ManageList2,aes(x=DOY,y=Difference))+ theme_bw()+
  geom_smooth()+
  geom_point(alpha=.1)+
  facet_grid(vars(Sensor),vars(Station),scales="free")+
  scale_colour_gradientn("Dates",colours=br, labels=as.Date)+
  ggtitle(paste("Difference in linear Slope and Mean NDVI by",maxgap,"days"))+
  geom_vline(data=Managements,aes(xintercept=DOY,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  theme(legend.position="none")+
  ylab("abs(diff(SlopeB,SlopeA)*mean(NDVI))")

# Facet by Station
  
g2<-ggplot(ManageList2,aes(x=DOY,y=Difference,color=Sensor))+ theme_bw()+
  geom_smooth()+
  facet_grid(Station~.)

ggsave(g1,filename = "C:/Users/MRossi/Documents/03_Data/06_Metrics/HarvestCoefficiaents.png",device = "png",width=9,height=7)


# Test of the Spline Function

ManageList3 <- ManageList2.norm %>% filter(Sensor!="Spectrometer")
ManageList.tmp<-ManageList3 %>% mutate(Sensor="COMBINED")
ManageList3 <- bind_rows(ManageList3, ManageList.tmp)
  

grp<-ManageList3 %>% 
  group_by(Station,Sensor) %>% 
  filter(!is.na(Normalized)) %>% 
  nest

freedom=50
grp2<-grp %>%
  mutate(Spline=map(data,function(x,d=freedom){
    
    if(nrow(x)<d) {d<-(nrow(x)-5)}
    
    Spline<-smooth.spline(x$DOY,x$Normalized,df=d) %>% 
      predict() %>% 
      as.data.frame() %>% 
      setNames(c("DOY","Normalized"))
    return(Spline)
    
  })) %>% 
  select(-data) %>% 
  unnest

g1<-ggplot(ManageList3,aes(x=DOY,y=Normalized))+ theme_bw()+
  geom_point(pch=1,col="grey")+
  geom_line(data=grp2,aes(x=DOY,y=Normalized),col="blue")+
  facet_grid(vars(Sensor),vars(Station),scales="free")+
  scale_colour_gradientn("Dates",colours=br, labels=as.Date)+
  ggtitle(paste("NDVI Dynamics in a",maxgap,"days moving Window for 2017"))+
  geom_vline(data=Managements,aes(xintercept=DOY,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  theme(legend.position="none")+
  ylab("NDVI Dynamics")+
  xlim(c(90,300))

ggsave(g1,filename = "Images/ManagementDetection_normalized2.png",device = "png")



# GetPeaks ----------------------------------------------------------------

quantiles<-20
seq<-seq(0,1,1/quantiles)
br<-colorRampPalette(c("darkgreen","green","orange","brown"))(quantiles)

times<-grp2 %>% 
  group_by(Station,Sensor) %>% 
  nest %>% 
  mutate(Stat=map(data,function(x){
    
    q<-quantile(x$Normalized,seq)
    
    ind<- map_dbl(x$Normalized,function(m) {
      r<-min(which(m<q))
      if(is.infinite(r)) r<-length(q)
      return(r)
      
      })
    x$quantile<-ind
    
    return(x)
    
    
   }))


grp2.quan<- times %>% select(-data) %>% unnest
grp2.quan2<-grp2.quan %>% mutate(Q2=map_dbl(quantile,function(x,s=seq) s[x]))

g1<-ggplot(ManageList3,aes(x=DOY,y=Normalized))+ theme_bw()+
  geom_point(pch=1,col="grey")+
  geom_line(data=grp2.quan2,aes(x=DOY,y=Normalized,col=Q2))+
  facet_grid(vars(Sensor),vars(Station),scales="free")+
  scale_colour_gradientn("Distibution \n Quantiles",colours=br)+
  ggtitle(paste("NDVI Dynamics in a",maxgap,"days moving Window for 2017"))+
  geom_vline(data=Managements,aes(xintercept=DOY,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  ylab("NDVI Dynamics")+
  xlim(c(90,300))

ggsave(g1,filename = "Images/ManagementDetection_normalized_quantiles2.png",device = "png",height=8,width=10)


# Join them 

uno <-dplyr::left_join(Managements,grp2.quan2,by=c("Station","DOY"))



uno2<-dplyr::filter(uno,!is.na(Q2))


uno3<-uno2 %>% select(Station,DOY,Event,Sensor,Q2)
uno.spread<- uno3 %>%  tidyr::spread(Sensor,Q2)

saveRDS(uno.spread,file = paste("rds/uno.spread.rds"))

g1<-ggplot(uno2,aes(DOY,Q2,pch=Sensor,lty=Station))+
  geom_point()+
  geom_line()+
  facet_wrap(.~Event)

# 4 domef

gr.domef<-grp2.quan %>% filter(Station=="Domef1500")
ManageList.domef<-ManageList3 %>% filter(Station=="Domef1500")
Managements.domef<-Managements %>% filter(Station=="Domef1500")

g1<-ggplot(ManageList.domef,aes(x=DOY,y=Normalized))+ theme_bw()+
  geom_point(pch=1,col="grey")+
  geom_line(data=gr.domef,aes(x=DOY,y=Normalized,col=quantile))+
  facet_wrap(.~Sensor)+
  scale_colour_gradientn("Distibution \n Quantiles",colours=br)+
  ggtitle(paste("Normalized Changes in NDVI signal: ",maxgap,"days moving Window"))+
  geom_vline(data=Managements.domef,aes(xintercept=DOY,linetype=Event))+
  scale_linetype_manual("Events",values=c("Harvest"="solid","Snow"="dotted"))+
  ylab("Normalized Difference in linear coefficients and mean NDVI")+
  xlim(c(90,300))

ggsave(g1,filename = "Images/ManagementDetection_normalized_quantiles_domef.png",device = "png")

# PCA --------------------------------------------------------

library("FactoMineR")
library("FactoInvestigate")

db4<-db3 %>% filter(!is.nan(Sentinel2)) %>% addNear(db3,.,col="Phenocam")

ggplot(db2,aes(Date,Value,color=Scale2))+
  geom_point(aes(pch=Plot2))+
  facet_grid(Station~.)+
  scale_x_date(limits=as.Date(c("2017-03-01","2017-11-01")),date_breaks = "1 month")

db4.plot<-db4 %>% 
  gather("Scale",value=NDVI,Decagon,Sentinel2,Phenocam,Spectrometer) %>% separate(Plot,c("Plot1","Plot"),sep="_")

g2<-ggplot(db4.plot,aes(Date,NDVI))+
  geom_point(aes(pch=Scale))+
  geom_line(aes(linetype=Scale,color=Plot))+
  facet_grid(Station~.)+
  scale_x_date(limits=as.Date(c("2017-05-01","2017-11-01")),
               date_breaks = "1 month")

loadandinstall("factoextra")

dbpca<-db4 %>% select(Station,Decagon,Sentinel2,Phenocam,Spectrometer)
res.pca<- PCA(dbpca,quali.sup=c(1:1),graph=F)

plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage="none", col.ind="grey", 
         col.ind.sup="blue", col.quali="red", label=c( "ind.sup", "quali"),
         new.plot=TRUE)

plot.PCA(res.pca, axes=c(1, 2), choix="var",
         new.plot=TRUE)

fviz_pca_var(res.pca ,repel=T,col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

fviz_pca_biplot(res.pca, repel = TRUE,label = c( "ind.sup", "quali"),col.quali="red")



# ///////////// -----------------------------------------------------------------------
### TEST----
### OLD Management----
# Manage<-readRDS(file = paste0(DataDir,"/Management/Complete_Management_2017.rds")) %>% 
#   as.tibble %>% 
#   filter(Value=="Harvest") %>% 
#   mutate(Station=as.character(Station)) %>% 
#   mutate(Value=as.character(Value)) 

### OLD Sentinel---- 
# sentinel1<-readRDS(paste0(MetricsDir,"Sentinel2_metrics_230618.rds")) %>% 
#   filter(OP2=="NDVI") %>% 
#   filter(OP3=="Mean") %>% 
#   rename(Plot=OP1) %>% 
#   select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)
# 
# 
# sentinel2<-readRDS(paste0(MetricsDir,"Sentinel2_metrics_points_230618.rds"))
# 
# tst <-sentinel2 %>% mutate(Tdiff=abs(Tdiff)) 
# tst2<-tst %>% group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% slice(which.min(Tdiff))
# 
# tst %>% mutate(error=purrr::map(data, dplyr::filter(Tdiff==min(Tdiff))))


# 
# # OLD Correlations --------------------------------------------------------
# library("broom")
# 
# db2<-db %>% select(-Scale1) %>% spread(Scale2,Value) %>% filter(OP3!="max")
# db3<-db2 %>% group_by(Date,Station,Plot) %>% 
#   dplyr::summarise(Decagon=mean(DECAGON,na.rm=T),
#                    Sentinel2=mean(MSI,na.rm=T),
#                    Phenocam=mean(Phenocam,na.rm=T),
#                    Spectrometer=mean(Spectrometer,na.rm=T)) %>% ungroup
# 
# db3.nest <- db3 %>% group_by(Station) %>% nest
# 
# names<-list(c("Spectrometer","Sentinel2"),
#             c("Spectrometer","Decagon"),
#             c("Spectrometer","Phenocam"),
#             c("Phenocam","Spectrometer"),
#             c("Phenocam","Decagon"),
#             c("Decagon","Spectrometer"))
# names2<-map(names,function(i) paste(i,collapse="_")) %>% unlist %>% c("Station",.)
# 
# mod_fun<-function(df,y1,y2) fit<-lm(get(y1) ~ get(y2),data=df,na.action = na.omit)
# d.nest<- db3.nest %>% 
#   mutate(t1=map(data,function(x,y=names[[1]]) glance(mod_fun(x,y[1],y[2])))) %>% 
#   mutate(t2=map(data,function(x,y=names[[2]]) glance(mod_fun(x,y[1],y[2])))) %>% 
#   mutate(t3=map(data,function(x,y=names[[3]]) glance(mod_fun(x,y[1],y[2])))) %>% 
#   mutate(t4=map(data,function(x,y=names[[4]]) glance(mod_fun(x,y[1],y[2])))) %>% 
#   mutate(t5=map(data,function(x,y=names[[5]]) glance(mod_fun(x,y[1],y[2])))) %>% 
#   mutate(t6=map(data,function(x,y=names[[6]]) glance(mod_fun(x,y[1],y[2]))))%>% 
#   select(-data) %>% 
#   setNames(names2) %>% 
#   gather(.,key=Scales,value = Value,-Station) %>%
#   unnest %>% 
#   tidyr::separate(.,Scales,into=c("Scale1","Scale2"),sep="_")
# 
# 
# file<-paste0(paste0(MNLS_Phenodir,"00_Combination/Filter/02_MovingWindow_byday.rds")) %>% readRDS()
# 
# a<-map(dirs,function(i) list.files(i,pattern="F7.rds",full.names = T)) %>% unlist
# stations<-dirname(dirname(a)) %>% basename
# 
# phenocam1<-map2(a,stations,function(i,j) {
#   u<-readRDS(i)
#   u$Station<-j
#   return(u)
# })
# 
# phenocam<-phenocam1 %>% 
#   do.call(rbind,.) %>% 
#   mutate(Plot=as.character(ROI)) %>% 
#   filter(ROI!="Far" & ROI!="Close" & ROI!="Mid") %>% 
#   filter(ndvitype=="mean") %>% 
#   add_column(Scale1="Proximal") %>% 
#   add_column(Scale2="Phenocam") %>%
#   add_column(OP2="NDVI") %>% 
#   rename(OP3=ndvitype) %>% 
#   rename(Value=ndvi) %>% 
#   select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)


# OLD SENTINEL 

# ml.ndvi.Date<- ml.ndvi.Nosd %>% filter(!is.na(Date)) %>% mutate(Tdiff=abs(Date-Date.Sen)) %>% 
#   group_by(Station,Date,OP1) %>% nest
# 
# sentinel1<-ml.ndvi.Date %>% 
#   mutate(MinDiff=map(data,function(x){return(x %>% arrange(Tdiff) %>% slice(1))})) %>% 
#   dplyr::select(-data) %>% 
#   unnest  %>% 
#   dplyr::select(-Tdiff)
# 
# sentinel2<-sentinel1 %>% 
#   full_join(.,loclink,by=c("Date","Station","OP1")) %>% 
#   filter(!is.na(ROI)) %>% 
#   mutate(Plot=as.character(ROI)) %>% 
#   dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value)
# 
# sentinel.all<-ml.ndvi.Nosd %>% 
#   dplyr::select(-Date) %>% 
#   filter(OP1=="Site") %>% 
#   rename(Plot=OP1) %>%
#   rename(Date=Date.Sen) %>% 
#   dplyr::select(Date,Station,Plot,Scale1,Scale2,OP2,OP3,Value) %>% 
#   mutate(Scale2="Sentinel2")
# 
# g.sen<-ggplot(sentinel.all,aes(Date,Value,pch="Site"))+
#   geom_line(linetype=1)+geom_point()+
#   geom_point(data=sentinel1,aes(Date.Sen,Value,color="Plot"))+
#   facet_wrap(Station~.)+
#   ggtitle("Cloud Free Sentinel-2 Acquisitions per Site")+ylab("NDVI")+xlab("Date")+
#   ylim(c(0,1)) + 
#   scale_colour_discrete(name  ="Payer",breaks=c("Female", "Male"),labels=c("Woman", "Man")) +
#   scale_shape_discrete(name  ="Payer",breaks=c("Female", "Male"),labels=c("Woman", "Man"))

