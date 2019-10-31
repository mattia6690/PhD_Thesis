
# Some Tables and Graphs for the second Paper


Sensor  <-c("Sentinel-2 MSI","Phenocam","SRS")
NDVI <-c("x","x","x")
NDWI <-c("x","","")
EVI  <-c("x","","")
PRI  <-c("","","x")
GCC  <-c("x","x","")
EXG  <-c("","x","")


tab<-as_tibble(cbind(Sensor,NDVI,NDWI,EVI,PRI,GCC,EXG))

saveRDS(tab,"tables/sensors.rds")

