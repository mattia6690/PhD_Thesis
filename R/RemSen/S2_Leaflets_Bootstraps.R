
# 1. Initialization ----
source("R/00_BaseFunctions.R")

# Leaflet Plots for Rasters

# Examplary Leaflet for 
ex1<-extent(oi2_shape[6,])
rne<-crop(r1,ex1+10000)
ex_shp<-as(extent(rne),'SpatialPolygons')
crs(ex_shp)<-projection(oi2_shape)

m<-leaflet(spTransform(ex_shp,CRS(projection(oi_shape)))) %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addPolygons()

m2<-leaflet() %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addRasterImage(rne,opacity = .9)


rne<-crop(r2,ex1+1000)
ex_shp<-as(extent(rne),'SpatialPolygons')
crs(ex_shp)<-projection(oi2_shape)

m<-leaflet(spTransform(ex_shp,CRS(projection(oi_shape)))) %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addPolygons()

rne<-crop(r2,ex1+100)
ex_shp<-as(extent(rne),'SpatialPolygons')
crs(ex_shp)<-projection(oi2_shape)

m<-leaflet(spTransform(ex_shp,CRS(projection(oi_shape)))) %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addPolygons()

m2<-leaflet() %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addRasterImage(rne,opacity = .8)

m2<-leaflet(oi_shape[6,]) %>% 
  addTiles() %>% 
  addMarkers(coordinates(oi_shape[6,])[,1],coordinates(oi_shape[6,])[,2]) %>%
  addRasterImage(rne,opacity = .8) %>% 
  addPolygons(color = "black",fill = F)


