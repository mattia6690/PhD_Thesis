source("R/00_BaseFunctions.R")

oi<-readOGR(paste0(Workspacedir,"/01_Data/KML"),"MONALISA Grassland")
ST<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol")

stat<-.STMeteo_buffer(oi,buffer=10000)
stat1<- stat$Province%>% as.character %>% unique
scode<-.STMeteo_getallSCODE()$TYPE

m<-.STMeteo_plotStation(.STMeteo_getStation(as="table"),
                        addPoints=oi,addBuff=T,widthBuff=10000)
m<-m %>% addPolygons(data=ST,fill=NA,col="black") 
m<-m %>% addLegend(position = 'topright',colors = c("blue","red"),
                labels=c("Province Stations","MONALISA Stations"),opacity=.5) %>%
m %>% addScaleBar() 


datestart = "2017-01-01 00:00"
dateend = "2017-08-31 00:00"

path<-"C:/Users/MRossi/Documents/03_Data/06_Province/Download/"

#lst<-list()
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




