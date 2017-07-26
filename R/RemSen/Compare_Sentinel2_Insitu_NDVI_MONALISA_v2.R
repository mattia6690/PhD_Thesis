
install.packages("devtools");library(devtools)
install_bitbucket("mattia6690/mrfunctions")
library(MRFunctions)

loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("raster")
loadandinstall("stringr")
loadandinstall("dplyr")

oi<-readOGR("Y:/Workspaces/RosM/01_Data/KML","MONALISA Grassland")
oi_shape<-readOGR("Y:/Workspaces/RosM/01_Data/KML","MONALISA Grassland Shapefile")
nms1<-tolower(c(as.character(oi@data$Name),as.character(oi_shape@data$Name)))
nms<-unique(nms1)
nms<-nms[-which(nms=="vimef2000")]

lf<-list.files("U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps/TPS",pattern=".tif",full.names = T)
lf.short<-list.files("U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps/TPS",pattern=".tif")

stat_par<-"NDVI_Avg"
down<-list.files("C:/Users/MRossi/Documents/07_Codes/SOS4R/02_Download/Download_csv_20170405_1547")
down.full<-list.files("C:/Users/MRossi/Documents/07_Codes/SOS4R/02_Download/Download_csv_20170405_1547",full.names = T)
supersplit<-do.call(rbind,strsplit(down, "\\_|\\-| "))[,2]
supersplit<-toupper(supersplit)

dates<-do.call(rbind,str_split(lf.short,pattern="_"))[,6]
dates2<-as.Date(dates,format("%Y%m%d"))

tot<-data.frame(Station=nms)
lst<-list()
mskp<-data.frame(Date<-dates,Masked<-NA)

for(i in 1:length(nms)){

  r1<- raster(lf[i])
  t5<- as.array(values(r1))
  wh_ras<-which(t5<(-10000)|t5>10000)
  t5[wh_ras]<-NA
  r2<-setValues(r1,as.numeric(t5));rm(r1)
  msk<-round(length(wh_ras)/ncell(r2)*100,2)

  mskp[i,2]<-msk

  date<-dates2[i]
  date2<-paste(date,"10:15:00")
  print(paste0("#0 Data Loaded - Masked Pixel: ",msk,"%"))

  oi2<-spTransform(oi,CRS(projection(r2)))
  nm_pnt<-oi2@data$Name
  oi2_shape<-spTransform(oi_shape,CRS(projection(r2)))
  nm_shape<-oi2_shape@data$Name
  print("#1 Preprocessing DONE")

  t1<- cellFromXY(r2,oi2)
  r3<- t5[t1]/10000
  exer.pnt<-cbind.data.frame(tolower(nm_pnt),round(r3,3)) # Combine the Data
  names(exer.pnt)<-c("Station","S2Pixel") # Rename the colmns
  vimval<-which(exer.pnt$Station=="vimef2000")   # Adjust the Vimef problem (Vimef to back and front and then delete original)
  d1<-data.frame(Station=c("vimef2000_back","vimef2000_front"),S2Pixel=exer.pnt[vimval,2])
  exer.pnt<-rbind(exer.pnt,d1)
  exer.pnt<-exer.pnt[-which(exer.pnt$Station=="vimef2000"),]
  print("#2 Pixel Analysis DONE") # Syso

  e1<-extract2(r2,oi2_shape)
  e2<-extract2(r2,oi2_shape,weight=T)
  exer.shp<-cbind.data.frame(tolower(nm_shape),
                             round(e1$val/10000,3),round(e2$val/10000,3),
                             e1$ncells) # Combine the data
  names(exer.shp)<-c("Station","S2 Shape","S2 Shape W","S2 NCells") # Rename the Coumns
  print("#3 Polygon Analysis DONE") # Syso

  narr<-array(dim=length(nm_pnt)) # Initialization Array
  for(j in 1:length(narr)){

    nm<-nm_pnt[j]

    if(is.element(nm,supersplit)==T){

      wh2<-which(is.element(supersplit,nm))
      csv<-read.csv(down.full[wh2])
      wh3<-which(csv[,2]==date2)

      if(length(wh3)==1){

        narr[j]<-csv[wh3,stat_par]

      }else{next}
    }else{next}
  }

  exer.ins<-cbind.data.frame(tolower(nm_pnt),narr)
  names(exer.ins)=c("Station","InSitu")
  vimval<-which(exer.ins$Station=="vimef2000")
  d1<-data.frame(Station=c("vimef2000_back","vimef2000_front"),InSitu=exer.ins[vimval,2])
  exer.ins<-rbind(exer.ins,d1)
  exer.ins<-exer.ins[-which(exer.ins$Station=="vimef2000"),]
  print("#4 In Situ Analysis DONE")

  tot1<-suppressWarnings(left_join(tot,exer.pnt,by="Station"))
  tot2<-suppressWarnings(left_join(tot1,exer.shp,by="Station"))
  tot3<-suppressWarnings(left_join(tot2,exer.ins,by="Station"))
  print("#5 Combination has been done")

  lst[[i]]<-tot3
  print(paste("Iteration",i,"of",length(lf),"DONE"))

}

save(tot,paste0("Y:\Workspaces\RosM","Sentinel_Insitu_Table.RData"))
r5<-do.call(rbind,lst)
save(r5,paste0("Y:\Workspaces\RosM","Sentinel_Insitu_Table_melted.RData"))
rm(lst)


### Create fishnet within polygon ###
setwd("Y:/Workspaces/RosM")
ras_10<-raster("U:/SAO/SENTINEL-2/SentinelVegetationProducts/ResampleSNAP10m_Export/S2A_USER_MSI_L2A_10m_20150704_N02.04_s2cV2.3_T32TPS.tif")
ras_20<-raster("U:/SAO/SENTINEL-2/SentinelVegetationProducts/ResampleSNAP20m_Export/S2A_USER_MSI_L2A_20m_20150704_N02.04_s2cV2.3_T32TPS.tif")

for(i in 1:length(oi)){

  nm<-as.character(oi@data$Name[i])
  # Buffer
  oi2<-spTransform(oi,CRS(projection(ras_10)))
  gg<-gBuffer(oi2[1,],width=400)

  # 10m Crop
  cr<-crop(ras_10,gg)
  poly10<-rasterToPolygons(cr)
  poly10<-spTransform(poly10,CRS("+proj=longlat +datum=WGS84"))
  writeOGR(poly10, dsn=paste0("Sentinel2_NDVI_",nm,"_poly10.kml"), layer="10m NDVI", driver="KML")
  writeOGR(poly10, dsn=getwd(),layer=paste0("Sentinel2_NDVI_",nm,"_poly10.shp"), driver="ESRI Shapefile")

  # 20m Crop
  cr2<-crop(ras_20,gg)
  poly20<-rasterToPolygons(cr2)
  poly20<-spTransform(poly20,CRS("+proj=longlat +datum=WGS84"))
  writeOGR(poly20, dsn=paste0("Sentinel2_NDVI_",nm,"_poly20.kml"), layer="20m NDVI", driver="KML")
  writeOGR(poly20, dsn=getwd(),layer=paste0("Sentinel2_NDVI_",nm,"_poly20.shp"), driver="ESRI Shapefile")

  print(paste(i,"of",length(oi)))
}


