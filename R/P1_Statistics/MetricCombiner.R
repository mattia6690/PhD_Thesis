# 1. Initialization ----
source("R/BaseFunctions.R")

# 2. Input ----

station<-"vimes1500"
as.numeric.factor <- function(x) {as.numeric(as.character(x))}
# Read the Metrics
# InSitu
insitu_raw<-readRDS(paste0(MetricsDir,"CombinedInSituMetricsSOS.rds"))

# Sentinel2
suffix<-"151217"
sentinel2_raw<-readRDS(paste0(MetricsDir,"S2_Filtered_NDVI_",suffix,".rds"))

# Monalisa
monalisa_raw<- readRDS(paste0(MetricsDir,"Monalisa_NDVI.rds"))
monalisa_raw<- monalisa_raw %>% filter(Value!="NaN") %>% 
  mutate(Value=as.numeric.factor(Value)) %>% 
  filter(Value>0)

# Managements
Manage<-readRDS(Manage,file = paste0(DataDir,"/Management/",station,"_Management_2017.rds"))


# 3. Plots of Metrics ----
#* 3.1. Select the Data ----

mnls<-monalisa_raw %>% 
  filter(grepl("10:30",Date)) %>% 
  filter(Station==station)
mnls$Date<-mnls$Date %>% as_date
mnls$Value<-mnls$Value %>% as.character %>% as.numeric

alltab<-rbind(insitu_raw,sentinel2_raw,mnls)
alltabstat<- alltab %>% filter(Station==station)

s2<-sentinel2_raw %>% 
  filter(Station==station) %>% 
  filter(OP2=="NDVI") %>% 
  filter(OP3=="Mean") %>% 
  filter(Value!="NaN")
s2$Date<-s2$Date %>% as_date
s2$Value<-s2$Value %>% as.character %>% as.numeric %>% `/`(10000)

groundN<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP2=="NDVI") %>% 
  group_by(Date,OP2) %>% summarise(Value=mean(Value))
groundN$Date<-groundN$Date %>% as.Date(format="%d%m%y")


#* 3.2. All scales together ----

ggplot(mnls,aes(Date,Value,color="MONALISA Station NDVI"))+theme_light()+
  geom_point(shape=0)+
  geom_point(data=s2,aes(Date,Value,color="Sentinel-2 NDVI"),shape=6)+
  geom_point(data=groundN,aes(Date,Value,color="Spectrometer NDVI"),shape=4)+
  scale_color_manual("Legend",values = c("blue","darkgreen","green","chocolate1"),
                     guide=guide_legend(override.aes = list(shape = c(0,6,4))))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ggtitle("NDVI derived from three different scales")

ggplot(mnls,aes(Date,Value,color="MONALISA Station NDVI"))+theme_light()+
  geom_point(shape=0)+
  geom_point(data=s2,aes(Date,Value,color="Sentinel-2 NDVI"),shape=6)+
  geom_point(data=groundN,aes(Date,Value,color="Spectrometer NDVI"),shape=4)+
  geom_vline(data=Manage,aes(xintercept=Date,color=Value),linetype="dashed")+
  scale_color_manual("Legend",values = c("blue","darkgreen","green","chocolate1"),
                     guide=guide_legend(override.aes = list(
                       linetype = c("dashed",rep("blank", 3)),
                       shape = c(NA,0,6,4)
                     )))+
  ggtitle("NDVI derived from three different scales")

#* 3.3. Metrics together with management ----

#** 3.3.1 MONALISA ----
g1<-ggplot(mnls,aes(Date,Value,color="MONALISA Station NDVI"))+theme_light()+
  geom_point(shape=0)+
  geom_vline(data=Manage,aes(xintercept=Date,color=Value),linetype="dashed")+
  scale_color_manual("Legend",values = c("chocolate1","darkorange3","gold3","blue","darkseagreen1","darkgreen"),
                     guide=guide_legend(override.aes = list(
                       linetype = c("dashed","dashed","dashed","dashed","dashed","blank"),
                       shape = c(NA,NA,NA,NA,NA,0)
                     )))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ylim(0,1)+
  ggtitle("NDVI from the Decagon Sensor")
ggsave(g1,filename = paste0(MetricsDir,"/",station,"/",station,"_NDVI_MONALISA.png"),device="png",width=10,height=7)

#** 3.3.2 Sentinel-2 ----
g2<-ggplot(data=s2,aes(Date,Value,color="Sentinel-2 NDVI"))+theme_light()+
  geom_point(shape=0)+
  geom_vline(data=Manage,aes(xintercept=Date,color=Value),linetype="dashed")+
  scale_color_manual("Legend",values = c("chocolate1","darkorange3","gold3","blue","darkseagreen1","darkgreen"),
                     guide=guide_legend(override.aes = list(
                       linetype = c("dashed","dashed","dashed","dashed","dashed","blank"),
                       shape = c(NA,NA,NA,NA,NA,0)
                     )))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ylim(0,1)+
  ggtitle("NDVI from Sentinel-2 NDVI")
ggsave(g2,filename = paste0(MetricsDir,"/",station,"/",station,"_NDVI_Sentinel.png"),device="png",width=10,height=7)

#** 3.3.3 Spectroradiometer ----
g3<-ggplot(data=groundN,aes(Date,Value,color="Spectrometer NDVI"))+theme_light()+
  geom_point(shape=0)+
  geom_vline(data=Manage,aes(xintercept=Date,color=Value),linetype="dashed")+
  scale_color_manual("Legend",values = c("chocolate1","darkorange3","gold3","blue","darkseagreen1","darkgreen"),
                     guide=guide_legend(override.aes = list(
                       linetype = c("dashed","dashed","dashed","dashed","dashed","blank"),
                       shape = c(NA,NA,NA,NA,NA,0)
                     )))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ylim(0,1)+
  ggtitle("NDVI from Spectrometer measurements")
ggsave(g3,filename = paste0(MetricsDir,"/",station,"/",station,"_NDVI_Ground.png"),device="png",width=10,height=7)



diffs<-mnls$Value %>% diff() %>% cbind.data.frame(Date=mnls$Date[-1],Value=.)
ggplot(diffs,aes(x=Date,y=Value))+
  geom_bar(stat="identity")+
  geom_point(data=mnls,aes(Date,Value))

diffs<-s2$Value %>% diff() %>% cbind.data.frame(Date=s2$Date[-1],Value=.)
ggplot(diffs,aes(x=Date,y=Value))+
  geom_bar(stat="identity")+
  geom_point(data=s2,aes(Date,Value))

diffs<-groundN$Value %>% diff() %>% cbind.data.frame(Date=groundN$Date[-1],Value=.)
ggplot(diffs,aes(x=Date,y=Value))+
  geom_bar(stat="identity")+
  geom_point(data=groundN,aes(Date,Value))


#* 3.3. Scale Correlations ----
#** 3.3.1 MNLS/Sentinel ----

s21<-is.element(mnls$Date,s2$Date) %>% filter(mnls,.)

joins<-right_join(s21,s2,by="Date",suffix=c("_Decagon","_Sentinel")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

rmse(lm(joins),joins)

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

ggplot(joins,aes(x=Value_Decagon,y=Value_Sentinel))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Sentinel-2 MSI")+xlab("Decagon_NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Sentinel and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))


#** 3.3.2 MNLS/Ground ----
s21<-is.element(mnls$Date,groundN$Date) %>% filter(mnls,.)

joins<-right_join(s21,groundN,by="Date",suffix=c("_Decagon","_Spectrometer")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

joins<-joins[-4,] # One outlier

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

ggplot(joins,aes(x=Value_Decagon,y=Value_Spectrometer))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Spectrometer NDVI")+xlab("Decagon_NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Spectrometer and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

#** 3.3.3 Sentinel/Ground ----
maxdays<-2

nmat<-nearDate(s2,groundN,maxdays=3)
colnames(nmat)<-c("Value_Sentinel","Value_Spectrometer","Diff")
joins<-nmat[,c(1,2)]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

joins$Diff<-nmat[,3]

ggplot(joins,aes(x=Value_Sentinel,y=Value_Spectrometer,color=Diff))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Spectrometer NDVI")+xlab("Sentinel NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

#* 3.4. Biomass Correlations ----

groundB<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP3=="BioWet") %>% 
  group_by(Date) %>% summarise(Value=mean(Value)) %>% ungroup
groundB$Date<-groundB$Date %>% as.Date(format="%d%m%y")

mnls2<-mnls %>% select(Date,Value)

#** 3.4.1 Monalisa ----

joins<-left_join(groundB,mnls2,by="Date") %>% arrange(Date) %>% select(-Date)
colnames(joins)<-c("Biomass","Monalisa_NDVI")
joins<-joins[-1,] # Wrong NDVI Detection
joins<-joins[,c(2,1)]


rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

ggplot(joins,aes(y=Biomass,x=Monalisa_NDVI))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))

#** 3.4.2 Ground ----

groundN2<-groundN %>% select(Date,Value)

joins<-left_join(groundB,groundN2,by="Date") %>% arrange(Date) %>% select(-Date)
joins<-joins[complete.cases(joins),]
colnames(joins)<-c("Biomass","Spectrometer_NDVI")
joins<-joins[,c(2,1)]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

ggplot(joins,aes(y=Biomass,x=Spectrometer_NDVI))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))

#** 3.4.3 Sentinel ----

nmat<-nearDate(s2,groundB,maxdays=3)
colnames(nmat)<-c("Sentinel_NDVI","Biomass","Diff")
joins<-nmat[,c(1,2)]


rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

ggplot(nmat,aes(x=Sentinel_NDVI,y=Biomass,color=Diff))+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))



# #* 3.1 Sentinel and InSitu Data
# 
# stations1<-sentinel2_raw$mnls_stations %>% tolower %>% unique %>% sort
# stations2<-insitu_raw$FOI %>% tolower %>% unique %>% sort
# 
# stations<-stringinlist(stations1,stations2)
# 
# 
# insfilter<-insitu_raw %>% select(c(FOI,Date,NDVI1))
# insfilter$Date<-as.Date(insfilter$Date)
# S2filter<-filter(sentinel2_raw ,mnls_stations %in% stations)
# 
# S2filter[["Date"]]<-as.Date(S2_dir2date(S2filter$dir.x))
# 
# for(i in stations){
#   
#   ins1<-insfilter %>% filter(FOI==simpleCap(i))
#   sen1<-S2filter %>% filter(mnls_stations==i) %>% filter(!is.na(Weigted_Mean)) %>% filter(Original_Nas==0)
#   g1<-ggplot(ins1,aes(x=Date,y=NDVI1))+
#     geom_boxplot(aes(group=Date),fill="chartreuse3")+
#     geom_vline(aes(xintercept=as.Date("2017-07-03")),color="darkgreen")+
#     geom_vline(aes(xintercept=as.Date("2017-09-04")),color="darkgreen")+
#     geom_point(data=sen1,aes(x=Date,y=(as.numeric(Weigted_Mean)/10000)),color="gray40",size=4)+
#     scale_x_date(limits = c(as.Date("2017-04-01"),as.Date("2017-08-01")),date_breaks = "1 month")+
#     ggtitle("Sentinel 2 MSI and InSitu Measurements of Grassland")+
#     ylab("NDVI")
#   
#   ggsave(plot=g1, filename = paste0(MetricsDir,"/InSituS2/S2_Insitu_test2.png"),device = "png",height=7,width = 14)
#   
# }
# 

