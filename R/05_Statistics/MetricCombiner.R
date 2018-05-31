# 1. Initialization ----
source("R/BaseFunctions.R")

# 2. Input ----

station<-"vimes1500"
dir<-paste0(MetricsDir,station,"/")

# Read the Metrics
# InSitu
insitu_raw<-readRDS(paste0(MetricsDir,"CombinedInSituMetricsSOS.rds"))

# Sentinel2
suffix<-"151217"
sentinel2_raw<-readRDS(paste0(MetricsDir,"S2_Filtered_NDVI_",suffix,".rds"))

# Monalisa
monalisa_raw<- readRDS(paste0(MetricsDir,"Monalisa_NDVI_filtered_MovingWindow.rds"))
monalisa_raw<- monalisa_raw %>% filter(Value!="NaN") %>% 
  mutate(Value=as.numeric.factor(Value)) %>% 
  filter(Value>0)

# PhenoCam
phenodat<-readRDS(paste0(PhenocamDir,"phenodat2.rds"))

# Managements
Manage<-readRDS(file = paste0(DataDir,"/Management/",station,"_Management_2017.rds")) %>% 
  dplyr::filter(Station==station)

#* 2.1. Select the Data ----

# Monalisa
mnls<-monalisa_raw %>% filter(Station=="vimes1500")

s2<-sentinel2_raw %>% 
  filter(Station==station) %>% 
  filter(OP2=="NDVI") %>% 
  filter(OP3=="Mean") %>% 
  filter(Value!="NaN") %>% 
  mutate(Value=Value/10000)

groundN<-insitu_raw %>% 
  filter(Station==simpleCap(station)) %>% 
  filter(OP2=="NDVI") %>% 
  mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
  group_by(Date,OP2) %>%
  dplyr::summarise(.,Value=mean(Value))

phenodat<-phenodat %>% setNames(.,c("Date","Value"))

ggplot(groundN,aes(Date,Value))+
  geom_point()+
  geom_line()+
  ggtitle("Smoothed NDVI InSitu for Station Vimes1500 in 2017")+
  ylab("NDVI")+
  ylim(c(.5,1))

m.ndvi<-mnls %>% select(Date,Value) %>% setNames(c("Date","SRS"))
s.ndvi<-s2 %>% select(Date,Value) %>% setNames(c("Date","Sentinel2"))
p.ndvi<-phenodat %>% select(Date,Value) %>% setNames(c("Date","Phenocam"))
g.ndvi<-groundN %>% select(Date,Value) %>% setNames(c("Date","Spectrometer"))

ndvi.tab<-full_join(m.ndvi,s.ndvi) %>% 
  full_join(.,p.ndvi) %>% 
  full_join(.,g.ndvi) %>% 
  add_column(Period=3) %>% 
  arrange(Date) 

Harvest<-Manage %>% filter(Value=="Harvest")

ndvi.tab$Period[which(ndvi.tab$Date<Harvest$Date[2])]<-2
ndvi.tab$Period[which(ndvi.tab$Date<Harvest$Date[1])]<-1

saveRDS(ndvi.tab,file = paste0(MetricsDir,"NDVI_Table_vimes1500_2017.rds"))


# 3. Plots of Metrics ----
ggplot(phenodat,aes(Date,NDVI2))+ geom_point()


#* 3.1. NDVI across scales ----

g1<-ggplot(mnls,aes(Date,Value,color="Decagon"))+theme_light()+ylab("NDVI")+
  geom_point(shape=0)+
  geom_point(data=s2,aes(Date,Value,color="Sentinel-2 MSI"),shape=6)+
  geom_point(data=groundN,aes(Date,Value,color="Spectrometer"),shape=4)+
  scale_color_manual("Legend",values = c("darkgreen","green","chocolate1"),
                     guide=guide_legend(override.aes = list(shape = c(0,6,4))))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ggtitle(paste0("Sensor specific NDVI for Station ",station))

colors<-c("darkgreen","blue","green","chocolate1","brown")
Harvest2<-Manage %>% filter(Value=="Harvest") %>% slice(1:2)
g2<-ggplot(mnls,aes(Date,Value,color="Decagon"))+theme_light()+ylab("NDVI")+
  geom_point(shape=16)+
  geom_point(data=s2,aes(Date,Value,color="Sentinel-2 MSI"),shape=16)+
  geom_point(data=groundN,aes(Date,Value,color="Spectrometer"),shape=16)+
  geom_point(data=phenodat,aes(Date,Value,color="PhenoCam"),shape=16)+
  geom_vline(data=Harvest2,aes(xintercept=Date,color=Value),linetype="dashed")+
  scale_color_manual("Legend",values = c("darkgreen","blue","green","chocolate1","brown"),
                     guide=guide_legend(override.aes = list(
                       linetype = c("blank","dashed","blank","blank","blank"),
                       shape = c(16,NA,16,16,16)
                     )))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-11-01")))+
  ggtitle(paste0("Sensor specific NDVI for Station ",station," - with Harvest"))

ggsave(g1,file=paste0(dir,"Combined_NDVI_",station,".png"),device="png",width=10,height=7)
ggsave(g2,file=paste0(dir,"Combined_NDVI_Management_",station,".png"),device="png",width=10,height=6)

# Facet grid for pubblication

ndvi.tab2<-ndvi.tab %>% 
  distinct %>% 
  setNames(c("Date","Decagon SRS","Sentinel-2 MSI","Phenocam","Spectrometer","Growing Period")) %>% 
  gather(key=Sensor,value= NDVI,"Decagon SRS","Sentinel-2 MSI","Phenocam","Spectrometer")
  
g.fac<-ggplot(ndvi.tab2,aes(Date,NDVI))+ theme_bw()+
  facet_wrap(.~Sensor)+
  geom_point()+
  geom_vline(data=Harvest2,aes(xintercept=Date),linetype="dashed")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))+
  ylim(c(0,1))+
  scale_x_date(date_breaks = "2 month")

ggsave(g.fac,file=paste0(dir,"Combined_NDVI_",station,"FacetGrid.png"),device="png",width=10,height=7)

#* 3.2. NDVI & Management ----

dsets<-list(mnls,s2,groundN)
names<-c("Decagon","Sentinel-2 MSI","Spectrometer")
ltypes12<-c("dotted","longdash","dotdash","dashed","solid","blank")
Manage2<-Manage
Manage2$ltypes<-NA
Manage2$ltypes[which(Manage$Value=="Irrigation")]<-"dotted"
Manage2$ltypes[which(Manage$Value=="Harvest")]<-"longdash"
Manage2$ltypes[which(Manage$Value=="Livestock")]<-"dotdash"
Manage2$ltypes[which(Manage$Value=="Hay")]<-"dashed"
Manage2$ltypes[which(Manage$Value=="Fertilization")]<-"solid"
ltypes2<-Manage2$ltypes

for(i in 1:length(dsets)){

  g1<-ggplot(dsets[[i]],aes(Date,Value))+theme_light()+ylab("NDVI")+
    geom_point(aes(color="NDVI"),shape=0)+
    geom_vline(data=Manage2,aes(xintercept=Date,color=Value,linetype=Value))+
    scale_color_manual("Value",values = c("Fertilization"="darkorchid1",
                                     "Harvest"="darkorange3",
                                     "Hay"="gold3",
                                     "Irrigation"="blue",
                                     "Livestock"="gray32",
                                     "NDVI"="darkgreen"),
                       guide=guide_legend(override.aes = list(
                         linetype = ltypes,
                         shape = c(NA,NA,NA,NA,NA,0))))+
    scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
    ylim(0,1)+
    ggtitle(paste0(names[i]," NDVI & Management for Station ",station))
  ggsave(g1,filename = paste0(dir,"NDVI_",names[i],"_Management_",station,".png"),device="png",width=9,height=7)

}

# IGARSS2018

sensor<-"SRS"
ManageDiff<-Manage[-c(13,14),]
igarssset<-ndvi.tab %>% select(Date,eval(sensor)) %>% mutate(Value=SRS)

g1<-ggplot(igarssset,aes(Date,Value))+theme_light()+ylab("NDVI")+
  geom_point(aes(color="NDVI"),shape=0)+
  geom_vline(data=ManageDiff,aes(xintercept=Date,linetype=Value))+
  scale_linetype_manual("",values=c("Fertilization"="dotted",
                                    "Harvest"="solid",
                                    "Irrigation"="dotdash",
                                    "Livestock"="longdash"))+
  scale_color_manual("",values = c("NDVI"="darkgreen"))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-31")),date_breaks="1 month")+
  ylim(0,1)+
  theme(legend.position="bottom",legend.text=element_text(size=12),axis.text=element_text(size=12))

g1<- ggplot(igarssset,aes(Date,Value))+theme_light()+ylab("NDVI")+
  geom_point(aes(color="NDVI"),shape=0)+
  facet_wrap(~.ManageDiff)

  
ggsave(g1,filename = paste0(dir,"NDVI_",sensor,"_IGARSSManagement_",station,".png"),
       device="png",width=9,height=7)
  
#* 3.3. Scale Correlations ----

cordf<-matrix(nrow=6,ncol=6)
colnames(cordf)<-c("Scale1","Scale2","R.squared","p-value","RMSE","N")
cordf[,1]<-c("Spectrometer","Spectrometer","Spectrometer","Decagon","Decagon","Phenocam")
cordf[,2]<-c("Decagon","Phenocam","Sentinel2","Phenocam","Sentinel2","Sentinel2")
options(scipen=10)


#** Ground/MNLS ----

s21<-is.element(mnls$Date,groundN$Date) %>% filter(mnls,.)

joins<-right_join(s21,groundN,by="Date",suffix=c("_Decagon","_Spectrometer")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

joins<-joins[-4,] # One outlier

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(x=Value_Decagon,y=Value_Spectrometer))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Spectrometer NDVI")+xlab("Decagon_NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Spectrometer and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Spectrometer_Decagon_",station,".png"),device="png",width=10,height=7)

cordf[1,3:6]<-round(c(r2,pval,rmse,nrow(joins)),5)


#** Ground/Phenocam ----

s21<-is.element(phenodat$Date,groundN$Date) %>% filter(phenodat,.)

joins<-right_join(s21,groundN,by="Date",suffix=c("_Phenocam","_Spectrometer")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]


rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(x=Value_Phenocam,y=Value_Spectrometer))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Spectrometer NDVI")+xlab("Phenocam_NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Spectrometer and Phenocam NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Spectrometer_Phenocam_",station,".png"),device="png",width=10,height=7)

cordf[2,3:6]<-round(c(r2,pval,rmse,nrow(joins)),5)


#** Ground/Sentinel ----
maxdays<-1

nmat<-nearDate(s2,groundN,maxdays=maxdays)
colnames(nmat)<-c("Value_Sentinel","Value_Spectrometer","Diff")
joins<-nmat[,c(1,2)]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

joins$Diff<-nmat[,3]

g1<-ggplot(joins,aes(x=Value_Sentinel,y=Value_Spectrometer,color=Diff))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Spectrometer NDVI")+xlab("Sentinel NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))
ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Spectrometer_Sentinel_",station,"_mxD",maxdays,".png"),device="png",width=10,height=7)

cordf[3,3:6]<-round(c(r2,pval,rmse,nrow(joins)),5)


#** MNLS/Phenocam ----

s21<-is.element(mnls$Date,phenodat$Date) %>% filter(mnls,.)

joins<-right_join(s21,phenodat,by="Date",suffix=c("_Decagon","_Phenocam")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(x=Value_Decagon,y=Value_Phenocam))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Phenocam NDVI")+xlab("Decagon NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Phenocam and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Phenocam_Decagon_",station,".png"),device="png",width=10,height=7)

cordf[4,3:6]<-round(c(r2,pval,rmse,nrow(joins)),6)


#** MNLS/Sentinel ----

s21<-is.element(mnls$Date,s2$Date) %>% filter(mnls,.)

joins<-right_join(s21,s2,by="Date",suffix=c("_Decagon","_Sentinel")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(x=Value_Decagon,y=Value_Sentinel))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Sentinel-2 MSI")+xlab("Decagon_NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Sentinel and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Sentinel_Decagon_",station,".png"),device="png",width=10,height=7)

cordf[5,3:6]<-round(c(r2,pval,rmse,nrow(joins)),5)

#** Phenocam/Sentinel ----

s21<-is.element(phenodat$Date,s2$Date) %>% filter(phenodat,.)

joins<-right_join(s21,s2,by="Date",suffix=c("_Phenocam","_Sentinel")) %>% select(contains("Value")) %>% 
  .[complete.cases(.),]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
pval<-joins %>% lm %>% summary %>% .[["coefficients"]] %>% .[8]
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(x=Value_Phenocam,y=Value_Sentinel))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("Phenocam NDVI")+xlab("Decagon NDVI")+
  ylim(0,1)+xlim(0,1)+
  ggtitle(paste0("Correlation between Phenocam and Decagon NDVI 2017 - ", station))+
  annotate("text",x=.9,y=.1,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.9,y=.05,label=paste0("RMSE = ",rmse))

ggsave(g1,filename = paste0(dir,"NDVI_Correlation_Phenocam_Decagon_",station,".png"),device="png",width=10,height=7)

cordf[6,3:6]<-round(c(r2,pval,rmse,nrow(joins)),5)

table.png(cordf,name="Correlation Table",dir=MetricsDir,res=200)

#* 3.4. Biomass Correlations ----

groundB<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP3=="BioWet") %>% 
  group_by(Date) %>% summarise(Value=mean(Value)) %>% ungroup
groundB$Date<-groundB$Date %>% as.Date(format="%d%m%y")

#** 3.4.1 Monalisa ----

mnls2<-mnls %>% select(Date,Value)
joins<-left_join(groundB,mnls2,by="Date") %>% arrange(Date) %>% select(-Date)
colnames(joins)<-c("Biomass","Monalisa_NDVI")
joins<-joins[-1,] # Wrong NDVI Detection
joins<-joins[,c(2,1)]


rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(y=Biomass,x=Monalisa_NDVI))+ ylab("Phytomass (g)")+xlab("NDVI")+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Phytomass and Decagon NDVI - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))
ggsave(g1,filename = paste0(dir,"Decagon_NDVI_Biomass_Correlation_",station,".png"),device="png",width=10,height=7)


#** 3.4.2 Ground ----

groundN2<-groundN %>% select(Date,Value)

joins<-left_join(groundB,groundN2,by="Date") %>% arrange(Date) %>% select(-Date)
joins<-joins[complete.cases(joins),]
colnames(joins)<-c("Biomass","Spectrometer_NDVI")
joins<-joins[,c(2,1)]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(joins,aes(y=Biomass,x=Spectrometer_NDVI))+ ylab("Phytomass (g)")+xlab("NDVI")+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))
ggsave(g1,filename = paste0(dir,"Spectrometer_NDVI_Biomass_Correlation_",station,".png"),device="png",width=10,height=7)


#** 3.4.3 Sentinel ----
maxdays<-3

nmat<-nearDate(s2,groundB,maxdays=maxdays)
colnames(nmat)<-c("Sentinel_NDVI","Biomass","Diff")
joins<-nmat[,c(1,2)]

rmse<-joins %>% rmse(lm(.),.) %>% round(.,2)
r2<-joins %>% rsquare(lm(.),.)%>% round(.,2)

g1<-ggplot(nmat,aes(x=Sentinel_NDVI,y=Biomass,color=Diff))+ ylab("Phytomass (g)")+xlab("NDVI")+
  geom_point()+
  geom_smooth(method="lm")+
  ylim(c(0,750))+xlim(c(0,1))+
  ggtitle(paste0("Correlation between Spectrometer and Sentinel NDVI 2017 - ", station))+
  scale_color_gradient("Days apart",low="green",high="red")+
  annotate("text",x=.1,y=0,label=paste0("paste(italic(R) ^ 2, \" = ",r2,"\")"),parse=T)+
  annotate("text",x=.1,y=50,label=paste0("RMSE = ",rmse))
ggsave(g1,filename = paste0(dir,"Sentinel_NDVI_Biomass_Correlation_",station,".png"),device="png",width=10,height=7)

#* 3.5 Differences ----

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


### 4. Management ----

data<-ndvi.tab %>% 
  distinct %>% 
  gather(key="Sensor",value=Value,SRS,Sentinel2,Phenocam,Spectrometer,-Period) %>% 
  select(-Period) 

ndays<-30
sensorsall<-data$Sensor %>% unique

mvallist<-list()
for(s in 1:length(sensorsall)){
  
  sensor<-sensorsall[s]
  
  data2<- data %>% 
    filter(Sensor==sensor) %>% 
    filter(!is.na(Value))
  
  ManageDiff<-Manage[-c(13,14),]
  
  events<-ManageDiff$Value %>% unique %>% as.character
  
  lista<-list()
  for(j in 1:nrow(ManageDiff)){
    
    if(j==1) counter=0
    
    ManageSub<-ManageDiff[j,]
    subname<-as.character(ManageSub$Value)
    date1<-as.character(ManageSub$Date)
    
    for(i in ndays:3){
      
      ManageSubDateB<-ManageSub$Date-i
      ManageSubDate<-ManageSub$Date
      ManageSubDateA<-ManageSub$Date+i
      
      mnlssub1<-data2 %>% filter(Date>=ManageSubDateB & Date<ManageSubDate) %>% add_column(Action="Before")
      mnlssub1$Iter<-mnlssub1 %>% rownames %>% as.numeric()
      mnlssub2<-data2 %>% filter(Date>ManageSubDate   & Date<=ManageSubDateA) %>% add_column(Action="After")
      mnlssub2$Iter<-mnlssub2 %>% rownames %>% as.numeric()
      mnlssub<-rbind(mnlssub1,mnlssub2)
      
      if(length(mnlssub1)==0|length(mnlssub2)==0) next
      
      if(nrow(mnlssub)>1){
        
        counter<-counter+1
        
        # Slope
        if(nrow(mnlssub1)>1){
          lm1<-lm(Value~Iter,mnlssub1)
          slopelm1<-lm1$coefficients[2] 
        } else { slopelm1=NA }
        
        if(nrow(mnlssub2)>1){
        lm2<-lm(Value~Iter,mnlssub2)
        slopelm2<-lm2$coefficients[2]
        } else { slopelm2=NA }
        
        if(!is.na(slopelm1) & !is.na(slopelm2)){
          
          p1<-mean(diff(predict(lm1)))
          p2<-mean(diff(predict(lm2)))
          slopecoef<-diff(c(p1,p2))
          
        } else { slopecoef=NA }
        
        # Intercept
        if(nrow(mnlssub1)>1){
          lm1<-lm(Value~Iter,mnlssub1)
          interlm1<-lm1$coefficients[1] 
        } else { interlm1=NA }
        
        if(nrow(mnlssub2)>1){
          lm2<-lm(Value~Iter,mnlssub2)
          interlm2<-lm2$coefficients[1]
        } else { interlm2=NA }
        
        if(!is.na(interlm1) & !is.na(interlm2)){
          
          intercoef<-diff(c(interlm1,interlm2))
          
        } else { intercoef=NA }
        
        # N
        n.b<-nrow(mnlssub1)
        n.a<-nrow(mnlssub2)
        n.all<-sum(n.b,n.a)
        
        # Mean and Standard Deviation Before
        if(nrow(mnlssub1)>0){
          
          mn.b<-mean(mnlssub1$Value)
          sd.b<-sd(mnlssub1$Value)
          
        } else { mn.b<-sd.b<- NA }
        
        # Mean and Standard Deviation After
        if(nrow(mnlssub2)>0){
          
          mn.a<-mean(mnlssub2$Value)
          sd.a<-sd(mnlssub2$Value)
          
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
        lista[[counter]]<-c(
          subname,date1,i,
          n.b,n.a,n.all,
          round(mn.b,4), round(mn.a,4), round(diffmean,4),
          round(sd.b,4), round(sd.a,4), round(diffsd,4),
          round(slopelm1,4),round(slopelm2,4), round(slopecoef,4),
          round(interlm1,4),round(interlm2,4), round(intercoef,4))
        
      }
    }
  }
  
  mvals<-do.call(rbind,lista) %>% .[complete.cases(.),] %>% as.tibble
  colnames(mvals)=c("Event","Date","Days",
                    "NobsB","NobsA","NobsTotal",
                    "MeanB","MeanA","MeanDiff",
                    "SdB","SdA","SdDiff",
                    "LMslopeB","LMslopeA","LMslopeDiff",
                    "LMinterB","LMinterA","LMinterDiff")
  
  mvals<-mvals %>% mutate(SlopeMean=as.numeric(MeanDiff)*abs(as.numeric(LMslopeDiff)))
  
  mvallist[[s]]<-mvals
  
  mvals2<- mvals %>% select(Event,Date,Days,MeanDiff,LMslopeDiff,SlopeMean) %>% 
    gather(key="Variation",value=Difference,MeanDiff,LMslopeDiff,SlopeMean) %>% 
    mutate(Difference=as.numeric(as.character(Difference))) %>% 
    mutate(Days=as.numeric(as.character(Days)))
  
  tit.short<-paste0(sensor,"_ChangeMeanNDVI_byEvent_",station)
  tit<-paste(sensor,"Change of Mean NDVI by Management -",station)
  g11<-ggplot(mvals2 %>% filter(Variation=="MeanDiff"),aes(Days,Difference,color=Date))+ theme_bw()+
    geom_line()+
    geom_point()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g11,filename = paste0(ManagementDir,station,"/",tit.short,".png"),device="png",width=12,height = 8)
  
  g12<-ggplot(mvals2 %>% filter(Variation=="MeanDiff"),aes(Days,Difference,group=Days))+ 
    geom_boxplot()+theme_bw()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g12,filename = paste0(ManagementDir,station,"/",tit.short,"_box.png"),device="png",width=12,height = 8)
  
  tit.short<-paste0(sensor,"_ChangeLMSlope_byEvent_",station)
  tit <- paste(sensor,"Change of Linear Regression Slope by Management -",station)
  g21<-ggplot(mvals2 %>% filter(Variation=="LMslopeDiff"),aes(Days,Difference,color=Date))+ theme_bw()+
    geom_line()+
    geom_point()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g21,filename = paste0(ManagementDir,station,"/",tit.short,".png"),device="png",width=12,height = 8)
  
  g22<-ggplot(mvals2 %>% filter(Variation=="LMslopeDiff"),aes(Days,Difference,group=Days))+
    geom_boxplot()+theme_bw()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g22,filename = paste0(ManagementDir,station,"/",tit.short,"_box.png"),device="png",width=12,height = 8)
  
  tit.short<-paste0(sensor,"_SlopeMean_byEvent_",station)
  tit <- paste(sensor,"Difference in Slope * Mean by Management -",station)
  g31<-ggplot(mvals2 %>% filter(Variation=="SlopeMean"),aes(Days,Difference,color=Date))+ theme_bw()+
    geom_line()+
    geom_point()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ylab("SlopeLM * Mean")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g31,filename = paste0(ManagementDir,station,"/",tit.short,".png"),device="png",width=12,height = 8)
  
  
  g32<-ggplot(mvals2 %>% filter(Variation=="SlopeMean"),aes(Days,Difference))+
    geom_smooth(method = "lm", formula = y ~ x + I(x^2)+I(x^3), size = 1,se=F,color="grey")+
    geom_boxplot(aes(group=Days))+theme_bw()+
    facet_wrap(.~Event)+
    xlab("Days Before and After Event")+
    ylab("SlopeLM * Mean")+
    ggtitle(tit)+
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))
  ggsave(g32,filename = paste0(ManagementDir,station,"/",tit.short,"_box.png"),device="png",width=12,height = 8)
  
  mvals3<-mvals2 %>% filter(Event=="Harvest")
  mvals3$Variation<-factor(
    mvals3$Variation,
    labels=c("Difference in LM Slope","Difference in Mean NDVI","Difference in LM Slope * Difference in Mean NDVI"))
  
  tit.short<-paste0(sensor,"_SlopeMeanDiff_Harvest_",station)
  tit <- paste(sensor,"Differences in Harvest -",station)
  g41<-ggplot(mvals3,aes(Days,Difference))+
    geom_boxplot(aes(group=Days)) +
    facet_wrap(.~Variation,ncol=1,scales = "free_y") +
    scale_x_continuous(breaks=seq(0,ndays,2),limits=c(0,ndays))+
    theme(strip.text.x = element_text(size = 12))+
    xlab("Days before and after")
  ggsave(g41,filename = paste0(ManagementDir,station,"/",tit.short,"_box.png"),device="png",width=8,height = 8)
  
  
  for(e in events){
    
    ev<- e
    mval2<-mvals %>% select(Event,Date,Days,MeanDiff,LMslopeDiff) %>% 
      gather(key="Variation",value=Difference,MeanDiff,LMslopeDiff) %>% 
      mutate(Difference=as.numeric(as.character(Difference))) %>% 
      mutate(Days=as.numeric(as.character(Days)))
    
    mval3<- mval2 %>% filter(Event==ev)
    tit<-paste(sensor,"NDVI Signal Before and After",ev,"event on",station)
    
    if(nrow(mval3)>0){
      
      g1<-ggplot(mval3,aes(Days,Difference,fill=Date))+
        geom_col(position="dodge")+
        ggtitle(tit)+
        ylim(c(0,.5))+
        facet_wrap(.~Variation)+
        scale_x_continuous(breaks=seq(2,ndays,2),limits=c(2,ndays))
      
      tit.short<-paste0(sensor,"_NDVI_Signal_",ev,"_",station)
      ggsave(g1,filename = paste0(ManagementDir,station,"/",tit.short,".png"),device="png",width=12,height = 8)
      
    }
  }
}

mvallist.bound<-do.call(rbind,mvallist) %>% as.tibble




ggplot(mnlssub,aes(Iter,Value))+
  geom_point()+
  geom_smooth(data=mnlssub1,method="lm")+
  geom_smooth(data=mnlssub2,method="lm")

ggplot(mvals,aes(x=Before,y=After))+geom_point()+ylim(-.1,.1)+xlim(-.1,.1)

g1<-mnlssub %>% 
  ggplot(.,aes(x=Date,y=Value,color=Action))+geom_point()+
  geom_vline(data=ManageSub,aes(xintercept=Date))+
  geom_smooth(aes(color=Action),method="lm",se=F)


mvals2<-mvals %>% gather
mvals2[["Diff"]]<-c(rownames(mvals),rownames(mvals)) %>% as.numeric
mvals2<-arrange(mvals2,Diff)

ggplot(mvals2,aes(x=Diff,y=value,fill=key))+
  geom_bar(stat="identity",position = "dodge")


out=cpt.mean(mnlssub$Value, pen.value = c(1,20000),penalty = "CROPS",method = "PELT")
out=cpt.meanvar(mnlssub$Value,method="BinSeg",penalty = "BIC")
plot(out,type="p",cpt.col="blue",xlab="index",cpt.width=4)

out2<-mnlssub %>% slice(cpts(out))

ggplot(mnlssub,aes(x=Date,y=Value))+
  geom_point()+
  geom_vline(data=out2,aes(xintercept=Date))

par(mfrow=c(1,2))
oi<-mnlssub$Value %>% diff()
plot(oi)
acf(oi, lag.max=20)

Harvests<-Manage %>% 
  filter(Value=="Harvest") %>% 
  select(Date) %>% 
  add_column("BioWet"=NA) %>% 
  add_column("LAI"=NA)

laiwettab<-insitu_raw %>% 
  filter(Station==simpleCap(station)) %>% 
  filter(OP3=="LAI"|OP3=="BioWet") %>% 
  mutate(Date= .$Date %>% as.Date(format="%d%m%y")) %>% 
  group_by(Date,OP3) %>% 
  dplyr::summarize(Mean=mean(Value)) %>% 
  spread(OP3,Mean) %>% 
  ungroup


ggtab<-bind_rows(laiwettab,Harvests) %>% arrange(Date)

ggplot(ggtab,aes(x=Date))+
  geom_line(aes(y=BioWet/100,color="Wet Biomass"))+
  geom_point(aes(y=BioWet/100,color="Wet Biomass"))+
  geom_line(aes(y=LAI,color="Leaf Area Index"))+ 
  geom_point(aes(y=LAI,color="Leaf Area Index"))+ 
  ylim(c(0,7))+
  labs(y="Leaf Area Index",color="Variable")+
  scale_y_continuous(sec.axis = sec_axis(~.*100, name = "Wet Biomass (g)"))+
  theme(legend.position = c(0.9, 0.9))+
  ggtitle("In Situ collected LAI and Wet Biomass in 2017 - Vimes1500")



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

