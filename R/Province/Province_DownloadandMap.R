# 1. Initialization ----

source("R/BaseFunctions.R")
install.packages("C:/Users/MRossi/Documents/07_Codes/MonalisR",repos=NULL,type="source")
library("MonalisR")


# 2. Input ----

oi<-readOGR(paste0(WorkspaceDir,"/01_Data/KML"),"MONALISA Grassland")
ST<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol")
oi_mat<-readOGR(paste0(WorkspaceDir,"/01_Data/KML"),"Mattia_Stations_PhD")
oi_mat_c<-coordinates(oi_mat)


# 3. Tidy ----

stat<-buffmeteo(oi,buffer=10000,dist=T)
stat1<- stat$Province%>% as.character %>% unique
scode<-getMeteoSensor(onlySensor = T)

stats<-getMeteoStat()
stats_spatial<-getMeteoStat(format="spatial")
stats<-stats[which(is.element(stats$SCODE,stat1)),]

names(stat)<-c("SCODE","Shape","Distance")
u<-right_join(stat,stats,by="SCODE")
u$ALTdiff<- abs(u$ALT- (substr(u$Shape,6,9) %>% as.numeric))
u1<-u %>% group_by(Shape) %>% summarize(min=min(ALTdiff))

perStat<-data.frame()

for(i in 1:nrow(u1)){
  
  wh1<-which(u$Shape==u1$Shape[i])
  wh2<-which(u$ALTdiff==u1$min[i])
  wh <-wh1[is.element(wh1,wh2)]
  perStat<-perStat %>% rbind(.,u[wh,]) 
  
}


# 4. Plot in Leaflet ----

ico1<-awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "darkblue")
ico2<-awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "orange")

m<-plotMeteoLeaflet(addPoints=oi,addBuff=T,widthBuff=10000)
m<-m %>% addAwesomeMarkers(perStat$LONG,perStat$LAT,icon=ico1,
                           popup=paste("Code:",perStat$SCODE,"<br>",
                                       "Name GER:",perStat$NAME_D,"<br>",
                                       "Name ITA:",perStat$NAME_I,"<br>",
                                       "Altitude:",perStat$ALT))
m<-m %>% addAwesomeMarkers(oi_mat_c[,1],oi_mat_c[,2],ico=ico2)
m<-m %>% addPolygons(data=ST,fill=NA,col="black") 
m<-m %>% addLegend(position = 'topright',colors = c("dodgerblue","blue","red","orange"),
                   labels=c("Province Stations","Selected Province Stations",
                            "MONALISA Stations","MONALISA Stations + Fieldwork"),opacity=1) 
m %>% addScaleBar() 


# 5. Download the Data ----

datestart = "2015-01-01 00:00"
dateend = "2017-08-31 00:00"
path<-paste0(ProviceDir,"Download/")

stat1<-perStat$SCODE

scode<-c("LT","N")

for(i in 1:length(stat1)){
  for(j in 1:length(scode)){
    
    pvcdw<-downloadMeteo(station_code = stat1[i],
                             sensor_code = scode[j],
                             datestart = datestart,
                             dateend = dateend,
                             path = path)
    
    #lst[[length(lst)+1]]<-pvcdw
    print(paste(i,"of",length(stat1),"stations & ",j,"of",length(scode),"Sensors"))
  }
}


# 6. Visualize the Timesteps ----

stats<- as.character(perStat$Shape)
lf2<-listdownload(path)

for(i in stats){
  
  stat<-perStat %>% filter(Shape==i)
  lf2<- lf2 %>% filter(CODE==stat$SCODE)
  
  for(j in 1:nrow(lf2)){
    
    load(lf2$Path[j]) #DAT
    
    DAT[["Date"]]<- as.Date(DAT$TimeStamp)
    sensor<-DAT$Sensor %>% unique
    
    if(sensor=="LT"){
      
      DAT2 <- DAT %>% group_by(Date) %>% 
        summarize(min=min(Value),mean=mean(Value),max=max(Value))
      
      g1<-ggplot(DAT2,aes(x=Date,y=mean))+
        geom_line(col="red")+
        geom_ribbon(aes(ymin=min,ymax=max),alpha=.2)+
        ggtitle(paste("Temperature at Station",unique(DAT$Station),"in",year(DAT2$Date) %>% unique))
      
    }
    
    if(sensor=="N"){
      
      DAT2 <- DAT %>% group_by(Date) %>% 
        summarize(prec=sum(Value))
      
      g1<-ggplot(DAT2,aes(x=Date,y=prec))+
        geom_bar(stat="identity",color=NA,fill="blue")+
        ggtitle(paste("Precipitation at Station",unique(DAT$Station),"in",year(DAT2$Date) %>% unique))
      
    }
    
    start<-str_replace_all(DAT$Date,"[-]","")
    ggsave(g1,filename = paste0(ProviceDir,"/Plots/",
                             DAT$Station %>% unique,"_",
                             DAT$Sensor %>% unique,"_",
                             min(start),"_",max(start),".png"),device="png")
  }
}

# 7. Harmonoie for the Metrics ----



