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
Manage<-readRDS(file = paste0(DataDir,"/Management/",station,"_Management_2017.rds"))

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
  filter(Station=="Vimes1500") %>% 
  filter(OP2=="NDVI") %>% 
  group_by(Date,OP2) %>% summarise(Value=mean(Value)) %>% ungroup
groundN$Date<-groundN$Date %>% as.Date(format="%d%m%y")

phenodat<-phenodat %>% setNames(.,c("Date","Value"))

ggplot(groundN,aes(Date,Value))+
  geom_point()+
  geom_line()+
  ggtitle("Smoothed NDVI InSitu for Station Vimes1500 in 2017")+
  ylab("NDVI")+
  ylim(c(.5,1))

m.ndvi<-mnls %>% select(Date,Value) %>% setNames(c("Date","Monalisa"))
s.ndvi<-s2 %>% select(Date,Value) %>% setNames(c("Date","Sentinel"))
p.ndvi<-phenodat %>% select(Date,Value) %>% setNames(c("Date","Phenocam"))
g.ndvi<-groundN %>% select(Date,Value) %>% setNames(c("Date","Ground"))

ndvi.tab<-full_join(m.ndvi,s.ndvi) %>% 
  full_join(.,p.ndvi) %>% 
  full_join(.,g.ndvi) %>% 
  add_column(Period=3) %>% 
  arrange(Date) 

ndvi.tab$Period[which(ndvi.tab$Date<Harvest$Date[2])]<-2
ndvi.tab$Period[which(ndvi.tab$Date<Harvest$Date[1])]<-1

saveRDS(ndvi.tab,file = paste0(MetricsDir,"NDVI_Table_vimes1500_2017.rds"))


# 3. Plots of Metrics ----

Harvest<-Manage %>% filter(Value=="Harvest")
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
g1<-ggplot(dsets[[i]],aes(Date,Value))+theme_light()+ylab("NDVI")+
  geom_point(aes(color="NDVI"),shape=0)+
  geom_vline(data=Manage2,aes(xintercept=Date,linetype=Value))+
  scale_linetype_manual("",values=c("Fertilization"="solid",
                                              "Harvest"="longdash",
                                              "Hay"="1F",
                                              "Irrigation"="dotted",
                                              "Livestock"="dotdash"))+
  scale_color_manual("",values = c("NDVI"="darkgreen"))+
  scale_x_date(limits=c(as.Date("2017-03-15"),as.Date("2017-10-15")))+
  ylim(0,1)+
  theme(legend.position="bottom",legend.text=element_text(size=12),axis.text=element_text(size=12))
  
ggsave(g1,filename = paste0(dir,"NDVI_",names[i],"_IGARSSManagement_",station,".png"),device="png",width=9,height=7)
  
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


# Look at management

ManageSub<-Manage[11,]

lista<-list()
for(i in 14:1){
  
  ManageSubDateB<-ManageSub$Date-i
  ManageSubDate<-ManageSub$Date
  ManageSubDateA<-ManageSub$Date+i
  
  mnlssub1<-mnls %>% filter(Date>=ManageSubDateB & Date<ManageSubDate) %>% add_column(Action="Before")
  mnlssub2<-mnls %>% filter(Date>ManageSubDate   & Date<=ManageSubDateA) %>% add_column(Action="After")
  mnlssub<-rbind.data.frame(mnlssub1,mnlssub2)
  
  if(nrow(mnlssub)>1){
    lm1<-lm(Value~Date,mnlssub1)
    lm2<-lm(Value~Date,mnlssub2)
    
    lista[[i]]<-c(lm1$coefficients[2],lm2$coefficients[2])
    
  }
}

mvals<-do.call(rbind.data.frame,lista) %>% .[complete.cases(.),]
colnames(mvals)=c("Before","After")


ggplot(mvals,aes(x=Before,y=After))+geom_point()+ylim(-.1,.1)+xlim(-.1,.1)

g1<-mnlssub %>% 
  ggplot(.,aes(x=Date,y=Value,color=Action))+geom_point()+
  geom_vline(data=ManageSub,aes(xintercept=Date))+
  geom_smooth(aes(color=Action),method="lm",se=F)

# glm poarameters: family = binomial,formula = y ~ poly(x,3

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

