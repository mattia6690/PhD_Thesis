
loadandinstall("ggplot2")
loadandinstall("tidyverse")
# In and output directory
indir<-"02_Download/"

# Load the RDAta Output from Program 01
pattern<-"Download_20170405_1547"
load(paste0(indir,pattern,".RData")) # finlist
finlist1<-finlist
myinfo1<-read.table(paste0(indir,pattern,".txt"),sep="")


pattern2<-"Download_20170405_1601"
load(paste0(indir,pattern2,".RData")) # finlist
finlist2<-finlist
myinfo2<-read.table(paste0(indir,pattern2,".txt"),sep="")

# NDVI/PRI Plot
for(i in 1:length(finlist2)){
  colns<-c("Time","FOI","value","Property")
  u1<-finlist1[[i+1]]
  u1[["Property"]]<-"NDVI"
  colnames(u1)<-colns
  
  u2<-finlist2[[i]]
  u2[["Property"]]<-"PRI"
  colnames(u2)<-colns
  
  u<-rbind(u1,u2)
  u$value[which(u$value<(-1))]<-NA
  u$value[which(u$value>(1))]<-NA
  
  ai<-as.data.frame(table(c(unique(u1$Time),unique(u2$Time))))
  aj<-ai$Var1[which(ai$Freq==2)]
  
  u1r<-u1[which(is.element(as.character(u1$Time),as.character(aj))),]
  u2r<-u2[which(is.element(as.character(u2$Time),as.character(aj))),]
  
  lll<-lm(u1r$value~u2r$value)
  rsq<-format(summary(lll)$r.squared,digits=3)
  
  ggplot(u,aes(Time,value,group=Property))+
    geom_point(aes(color=Property,shape=Property),na.rm=T)+
    ggtitle(u$FOI)+
    scale_colour_manual(values=c("green", "blue"))+
    scale_y_continuous(limits=c(-1.1,1))+
    annotate(geom="text",x=as.POSIXct(sort(aj)[length(aj)/2]),y=(-1.1),label=paste0("rsq= ",rsq))
  
  ggsave(filename = paste0("03_Images/PRI_NDVI/",u$FOI,".jpeg"),device="jpeg",dpi=300,width=15,height=10)
}

# PRI Plot
for(i in 1:length(finlist2)){
  
  u2<-finlist2[[i]]
  u2[["Property"]]<-"PRI"
  colnames(u2)<-c("Time","FOI","value","Property")
  u2$value[which(u2$value<(-1))]<-NA
  
  tst<-lapply(strsplit(as.character(u2$Time),split="\\s"),function(l) l[[1]])
  u2[["Date"]]<-as.Date(unlist(tst))
  
  u3<-u2 %>% group_by(Date) %>% summarise(mean=mean(value),sd=sd(value))
  
  ggplot(u3,aes(x=Date))+ theme_bw()+
    geom_line(aes(y=mean))+
    ylim(c(-.5,.5))+
    ggtitle(paste0(u2$FOI,": Mean Daily PRI"))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_date(date_breaks="1 month",date_labels = "%d-%m-%y",
                 limits = c(as.Date("2015-03-01"),as.Date("2017-01-01")))
  
  ggsave(filename = paste0("03_Images/PRI/Daily_Mean_PRI_",u2$FOI,".jpeg"),device="jpeg",dpi=300,width=15,height=10)
}


# LAI Plots
mnlsdir<-"C:/Users/MRossi/Documents/03_Data/03_InSitu/01_MONALISA/"

LAI2015<-read.csv(paste0(mnlsdir,"LAI2015_NaNsreplaced_PhenoCorrected.csv"))
LAI2015_cor<-LAI2015[which(.multigrepl("cor",colnames(LAI2015)))]
LAI2015_cor[["Date"]]<-as.Date(LAI2015$Date,format="%d/%m/%Y")

LAI2016<-read.csv(paste0(mnlsdir,"LAI2016_NaNsreplaced_PhenoCorrected.csv"))
LAI2016_cor<-LAI2016[which(.multigrepl("cor",colnames(LAI2016)))]
LAI2016_cor[["Date"]]<-as.Date(LAI2016$Date,format="%d/%m/%Y")

szp<-read.csv(paste0(mnlsdir,"Schnittzeitpunkte 2016.csv"))
szp<-szp[-which(szp$Beprobung=="nicht durchgeführt"),]

inpLAI<-LAI2015_cor
for(name in colnames(inpLAI)){
  
  if(name=="Date") break
  d<-diff(inpLAI[[name]])
  dd<-inpLAI[wh1<-which(d<(-1)),]$Date
  dd1<-as.data.frame(cbind(dd,rep("Havesting",length(dd))))
  colnames(dd1)<-c("Date","Harvest")
  dd1[["Date"]]<-as.Date(dd)
  
  year<-as.numeric(unique(format(inpLAI$Date,"%Y")))
  stat<-gsub("_cor","",name)
  start<-as.Date(paste0(year,"-01-01"))
  end<-as.Date(paste0(year+1,"-01-01"))
  
  tmp<-gsub(" ","",as.character(szp[,1]))
  szp1<-szp[.multigrepl(stat,tmp),]
  szp1[["Date"]]<-as.Date(szp1$Beprobung,format("%d.%m.%y"))
  
  if(nrow(szp1)>0){
    
    ggplot(inpLAI,aes(x=Date,color="LAI"))+
      geom_point(aes_string(y=name))+
      geom_vline(data=dd1,aes(xintercept = as.numeric(Date),color=Harvest))+
      annotate("text",x=dd+15,y=5,label=dd)+
      ggtitle(paste("Modelled LAI Curves for",stat,"in",year))+
      ylab("LAI")+
      scale_x_date(limits = c(as.Date(start),as.Date(end)))+
      geom_vline(data=szp1,aes(xintercept=as.numeric(Date),color="Observations"),linetype="dashed")+
      scale_colour_manual(name="Legend",values=c("darkred","darkgreen","black"))+
      ylim(c(0,6))
    
  }else{
    
    ggplot(inpLAI,aes(x=Date,color="LAI"))+
      geom_point(aes_string(y=name))+
      geom_vline(data=dd1,aes(xintercept = as.numeric(Date),color=Harvest))+
      annotate("text",x=dd+15,y=5,label=dd)+
      ggtitle(paste("Modelled LAI Curves for",stat,"in",year))+
      ylab("LAI")+
      scale_x_date(limits = c(as.Date(start),as.Date(end)))+
      scale_colour_manual(name="Legend",values=c("darkred","darkgreen"))+
      ylim(c(0,6))
  }
  ggsave(filename = paste0(mnlsdir,"LAI_",stat,"_",year,".jpg"),device="jpeg",height=10,width=15)
}
