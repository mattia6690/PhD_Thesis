
# 1. Initialization ----
source("R/00_BaseFunctions.R")

# Leaflet Plots for Rasters

r2<-raster(sao_ndvi_lf[30])
r2[r2<(-10000)]<-NA


ST<-oi<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol")
proj<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
ST2<-spTransform(ST,CRS(proj))


gr<-ggR(r2,geom_raster = T,stretch = 'lin')+theme_minimal()+
  ggtitle("NDVI MAP of T32TPS on 13.06.17")+
  scale_fill_continuous("NDVI",low = "red",high = "green",na.value = "grey")+
  geom_polygon(data=fortify(ST2),aes(long,lat),fill=NA,color="black")+
  ylim()

buff<-gBuffer(ST2,width=200000)

ers<-erase(buff,ST2)

ext<-extent(r2)
gr<-ggR(r2,geom_raster = T,stretch = 'lin')+theme_minimal()+
  ggtitle("NDVI MAP of T32TPS on 13.06.17")+
  scale_fill_continuous("NDVI",low = "red",high = "green",na.value = "grey")+
  geom_polygon(data=fortify(ers),aes(long,lat),fill="white",color=NA)+
  ylim(c(ext[3]-100,ext[4]+100))+
  xlim(c(ext[1]-100,ext[2]+100))

# Examplary Leaflet for 
ex1<-extent(oi2_shape[6,])
rne<-crop(r2,ex1+10000)
rne[rne<(-10000)]<-NA
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


