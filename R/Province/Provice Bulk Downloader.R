source("R/DependingFunctions.R")
source("R/ProvinceST_StationDownload.R")

dburl = "http://daten.buergernetz.bz.it/services/meteo/v1/timeseries"
sensor_code  = c("LT","N","GS","SD")
datestart = "2017-01-01 00:00"
dateend = Sys.Date()
outdir = "C:/Users/MRossi/Documents/03_Data/06_Province"

shp1<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/02_MONALISA","MONALISA_grasslands")
shp1<-shp1[,-(2:ncol(shp1))]

statC<-.STMeteo_buffer(shp=shp1,bufferW=10000)
statC<-unique(statC$Province)

for(i in statC){
  for(j in sensor_code){
    
  provdat<-.STMeteo_download(dburl=dburl,
                             station_code=i,
                             sensor_code=j,
                             datestart=datestart,
                             dateend=dateend,
                             path=outdir,csv=T)
    
  }
}




