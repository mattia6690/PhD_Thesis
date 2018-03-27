
### Create fishnet within polygon ###

# Load the Raster
ras_10<-raster(paste0(SAO_Vegetationdir,"/ResampleSNAP10m_Export/T32TPS/S2A_USER_MSI_L2A_10m_20150704_N02.04_s2cV2.3_T32TPS.tif"))
ras_20<-raster(paste0(SAO_Vegetationdir,"/ResampleSNAP20m_Export/S2A_USER_MSI_L2A_20m_20150704_N02.04_s2cV2.3_T32TPS.tif"))

# Load the Shapefiles
oi<-readOGR(paste0(Workspacedir,"01_Data/KML"),"MONALISA Grassland")

for(i in 1:length(oi)){
  
  nm<-as.character(oi@data$Name[i])
  # Buffer
  oi2<-spTransform(oi,CRS(projection(ras_10)))
  gg<-gBuffer(oi2[1,],width=400)
  
  # Cropping
  cr<-crop(ras_10,gg)
  poly10<-rasterToPolygons(cr)
  poly10<-spTransform(poly10,CRS("+proj=longlat +datum=WGS84"))
  cr2<-crop(ras_20,gg)
  poly20<-rasterToPolygons(cr2)
  poly20<-spTransform(poly20,CRS("+proj=longlat +datum=WGS84"))
  
  
  # Writing
  
  writeOGR(poly10, dsn=paste0("Sentinel2_NDVI_",nm,"_poly10.kml"), layer="10m NDVI", driver="KML")
  writeOGR(poly10, dsn=getwd(),layer=paste0("Sentinel2_NDVI_",nm,"_poly10.shp"), driver="ESRI Shapefile")
  writeOGR(poly20, dsn=paste0("Sentinel2_NDVI_",nm,"_poly20.kml"), layer="20m NDVI", driver="KML")
  writeOGR(poly20, dsn=getwd(),layer=paste0("Sentinel2_NDVI_",nm,"_poly20.shp"), driver="ESRI Shapefile")
  
  print(paste(i,"of",length(oi)))

  }