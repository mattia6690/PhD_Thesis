
#########################'
#### 1.Input ############
#########################'

source("R/00_BaseFunctions.R")

oi<-readOGR(paste0(Workspacedir,"/01_Data/KML"),"MONALISA Grassland")
ST<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol")

oi_mat<-readOGR(paste0(Workspacedir,"/01_Data/KML"),"Mattia_Stations_PhD")
oi_mat_c<-coordinates(oi_mat)

#########################'
#### 2.Process ##########
#########################'

stat<-.STMeteo_buffer(oi,buffer=10000,dist=T)
stat1<- stat$Province%>% as.character %>% unique
scode<-.STMeteo_getallSCODE()$TYPE

stats<-.STMeteo_getStation()
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

#########################'
#### 3.Plot #############
#########################'

ico1<-awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "darkblue")
ico2<-awesomeIcons(icon = 'ios-close',iconColor = 'black',library = 'ion',markerColor = "orange")



m<-.STMeteo_plotStation(.STMeteo_getStation(as="table"),
                        addPoints=oi,addBuff=T,widthBuff=10000)
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

#########################'
#### 4.Download #########
#########################'

datestart = "2017-01-01 00:00"
dateend = "2017-08-31 00:00"
path<-"C:/Users/MRossi/Documents/03_Data/06_Province/Download/"

for(i in 1:length(stat1)){
  for(j in 1:length(scode)){
    
    pvcdw<-.STMeteo_download(station_code = stat1[i],
                             sensor_code = scode[j],
                             datestart = datestart,
                             dateend = dateend,
                             path = path)
    
    #lst[[length(lst)+1]]<-pvcdw
    print(paste(i,"of",length(stat1),"stations & ",j,"of",length(scode),"Sensors"))
  }
}



