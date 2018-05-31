
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")
library("sf")

# 2. Input ----
#* 2.1 MONALISA Data ----

oi<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland")
oi_shape<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland Shapefile")

mnls_lf<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"),full.names = T)
mnls_lf.short<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"))
mnls_stations<-do.call(rbind,strsplit(mnls_lf.short, "\\_|\\-| ")) %>% .[,2]

#* 2.2 Sentinel-2 Data ----

sao_ndvi_lf<-list.files(SAO_NDVIdir,pattern=".tif",full.names = T,recursive = T)
sao_ndvi_lf.short<-basename(sao_ndvi_lf)

#* 2.3 GPS Dataset ----

gpsdata<-st_read(paste0(dirgps,"FilteredGPS.shp"))
gpsdata$OP2="NDVI"
gpsdata$OP3="Absolute"
gpsdata$Value=NA

#* 2.4 Global Input ----

sen2proj<-"LAEA"
NArange <-c(-10000,10000)
year<-"2017"

# 3. Tidy ----
#* 3.1 Tidy the Table ----

df<-S2_avail(sao_ndvi_lf.short,sen2names) %>% 
  add_column(dir=sao_ndvi_lf)

df<-df %>% 
  expand(df,nesting(mnls_stations)) %>% 
  filter(Projection==sen2proj)

sao_dates<-df %>%
  filter(substr(AcqDate,1,4)==year) %>%
  dplyr::select(dir)

sao_ndvi_lf<- sao_dates %>% 
  as.matrix %>% 
  unlist %>% 
  unique

sao_ndvi_lf.short<-basename(sao_ndvi_lf)
sao_ndvi_lf.tile<-basename(dirname(sao_ndvi_lf))

#* 3.2 Change Projection ----

master<-raster(sao_ndvi_lf[1])
proj<-projection(master)
oi2<-spTransform(oi,CRS(proj))
oi2_shape<-spTransform(oi_shape,CRS(proj))
oi2@data$Name<-oi2@data$Name %>% tolower
oi2_shape@data$Name<-oi2_shape@data$Name %>% tolower

#* 3.3 Generate Buffer ----
oi2buff<-gBuffer(oi2,byid=T,width=1000)

# Easy plotting
# Adjust for fitting tiles!!!

station<-"vimes1500"
station_pnt<-oi2[5,]
station_shp<-oi2_shape[6,]
station_buff<-oi2buff[5,]

recl<-rbind(c(-10001,10001,1),
            c(-21001,-20999,2),
            c(-22001,-21999,3),
            c(-23001,-22999,4),
            c(-24001,-23999,5),
            c(-25001,-24999,6),
            c(-26001,-25999,7))

list<-cbind(c("No Masking","Snow","Shadow","Refl. Error","Clouds","LandCover","NoData"),
            c("green","lightskyblue","Black","darkred","white","red","yellow")) %>% as.tibble


for(i in 1:length(sao_ndvi_lf)){

  name<-sao_ndvi_lf[i]
  date<-S2_dir2date(name)
  r<-raster(name)
  
  rct<-crop(r,station_buff)
  colnames(recl)<-c("From","To","Class")
  recl<-as.tibble(recl) %>% mutate(From=as.numeric(From)) %>% mutate(To=as.numeric(To))
  r2<-reclassify(rct,recl)
  
  vls<- unique(r2)
  list2<-list[vls,]
  items<-list2[,1] %>% as.matrix
  cols<-list2[,2] %>% as.matrix
  
  png(filename = paste0(RemsenDir,"S2_Cover/",station,"_",i,".png"),width=600,height=600)
  plot(r2,legend=F,col=cols)
  plot(station_shp,add=T)
  plot(station_pnt,add=T)
  title(paste(station,date))
  legend("topright",legend=items,fill=cols,bg = "white")
  dev.off()
}


rastest<- as(extent(r1),"SpatialPolygons") %>% st_as_sf(wkt="geom") %>% st_make_grid(., cellsize = c(5000, 5000)) %>% st_cast("MULTIPOLYGON")
st_crs(rastest)<-projection(r1)

xy.list <- split(rastest, seq(length(rastest)))
lapply(xy.list ,function(x,y=r1) crop(y,as(x,"Spatial")))



