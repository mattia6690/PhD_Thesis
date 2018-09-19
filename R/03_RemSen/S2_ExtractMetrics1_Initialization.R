
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")

# 2. Input ----
year<-2017
#* 2.1 Sentinel-2 Data ----

sao_ndvi_lf<-list.files(SAO_NDVIdir,pattern="UTM_masked.tif",full.names = T,recursive = T)

df<-sao_ndvi_lf %>% 
  basename %>% 
  S2_avail(.,sen2names) %>% 
  add_column(dir=sao_ndvi_lf) %>%
  dplyr::filter(substr(AcqDate,1,4)==year)

#* 2.2 MONALISA Data ----

proj<-df$dir[1] %>% raster %>% projection()

oi<-st_read(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland") %>% select(Name)
oi2<-oi %>% 
  st_transform(proj) %>% 
  add_column(OP3="Absolute",.after="Name") %>% 
  add_column(OP2="NDVI",.after="Name") %>% 
  add_column(OP1="Station",.after="Name") %>% 
  add_column(Scale2="MSI",.after="Name") %>% 
  add_column(Scale1="Sentinel2",.after="Name") %>% 
  mutate(Name=sapply(as.character(Name),simpleCap)) %>% 
  rename(Station=Name)

oi_shape<-st_read(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland Shapefile") %>% select(Name)
oi2_shape<-oi_shape %>% 
  st_transform(proj) %>% 
  st_zm %>% 
  add_column(OP3="Mean",.after="Name") %>% 
  add_column(OP2="NDVI",.after="Name") %>% 
  add_column(OP1="Site",.after="Name") %>% 
  add_column(Scale2="MSI",.after="Name") %>% 
  add_column(Scale1="Sentinel2",.after="Name") %>% 
  mutate(Name=sapply(as.character(Name),simpleCap)) %>% 
  rename(Station=Name)

oi2_buffer<-st_buffer(oi2_shape,dist = 300) %>% mutate(OP1="Buffer")

oi2_all<-rbind(oi2,oi2_shape,oi2_buffer) %>% 
  arrange(Station) %>% 
  add_column(Date=NA,.before=T)


#* 2.3 GPS Dataset ----

gpsdata<-st_read(paste0(dirgps,"FilteredGPS.shp"))
gpsdata<-gpsdata %>%
  st_transform(proj) %>% 
  add_column(OP3="Absolute",.after="OP1") %>% 
  add_column(OP2="NDVI",.after="OP1") %>% 
  mutate(Scale2="MSI") %>%
  mutate(Scale1="Sentinel2") %>% 
  select(-DOY) %>% 
  arrange(Station,Date)

data.all<-rbind(gpsdata,oi2_all) %>% arrange(Station,Date)

# Cut the Rasters with GDAL

gdal.trsl<-'"C:/Program Files/GDAL/bin/gdal/apps/gdal_translate.exe"'

for(i in 1:nrow(df)){
  
  name<-df$AcqDate[i]
  file<-df$dir[i]
  
  rasterlist<-list()
  for(j in 1:nrow(oi2_buffer)){
    
    station<-oi2_buffer[j,]$Station %>% as.character
    file.c<-oi2_buffer[j,] %>% st_bbox
    
    tmpfile<-tempfile(fileext = ".tif")
    
    command<-paste(gdal.trsl,"-projwin",file.c[1],file.c[4],file.c[3],file.c[2],file,tmpfile,"-q")
    system(command)
    rasterlist[[station]]<-raster(tmpfile)
    file.remove(tmpfile)
  }
}


# 3. Tidy ----
#* 3.1 Tidy the Table ----


# df<-df %>% 
#   expand(df,nesting(mnls_stations)) %>% 
#   filter(Projection==sen2proj)
# 
# sao_dates<-df %>%
#   filter(substr(AcqDate,1,4)==year) %>%
#   dplyr::select(dir)
# 
# sao_ndvi_lf<- sao_dates %>% 
#   as.matrix %>% 
#   unlist %>% 
#   unique







#* 3.3 Generate Buffer ----



# 
# 
# 
# mnls_lf<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"),full.names = T)
# mnls_lf.short<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"))
# mnls_stations<-do.call(rbind,strsplit(mnls_lf.short, "\\_|\\-| ")) %>% .[,2]
# 
# 
# 
# 
# 
# 
# 
# # Easy plotting
# # Adjust for fitting tiles!!!
# 
# station<-"vimes1500"
# station_pnt<-oi2[5,]
# station_shp<-oi2_shape[6,]
# station_buff<-oi2buff[5,]
# 
# recl<-rbind(c(-10001,10001,1),
#             c(-21001,-20999,2),
#             c(-22001,-21999,3),
#             c(-23001,-22999,4),
#             c(-24001,-23999,5),
#             c(-25001,-24999,6),
#             c(-26001,-25999,7))
# 
# list<-cbind(c("No Masking","Snow","Shadow","Refl. Error","Clouds","LandCover","NoData"),
#             c("green","lightskyblue","Black","darkred","white","red","yellow")) %>% as.tibble
# 
# 
# for(i in 1:length(sao_ndvi_lf)){
# 
#   name<-sao_ndvi_lf[i]
#   date<-S2_dir2date(name)
#   r<-raster(name)
#   
#   rct<-crop(r,station_buff)
#   colnames(recl)<-c("From","To","Class")
#   recl<-as.tibble(recl) %>% mutate(From=as.numeric(From)) %>% mutate(To=as.numeric(To))
#   r2<-reclassify(rct,recl)
#   
#   vls<- unique(r2)
#   list2<-list[vls,]
#   items<-list2[,1] %>% as.matrix
#   cols<-list2[,2] %>% as.matrix
#   
#   png(filename = paste0(RemsenDir,"S2_Cover/",station,"_",i,".png"),width=600,height=600)
#   plot(r2,legend=F,col=cols)
#   plot(station_shp,add=T)
#   plot(station_pnt,add=T)
#   title(paste(station,date))
#   legend("topright",legend=items,fill=cols,bg = "white")
#   dev.off()
# }
# 
# 
# rastest<- as(extent(r1),"SpatialPolygons") %>% st_as_sf(wkt="geom") %>% st_make_grid(., cellsize = c(5000, 5000)) %>% st_cast("MULTIPOLYGON")
# st_crs(rastest)<-projection(r1)
# 
# xy.list <- split(rastest, seq(length(rastest)))
# lapply(xy.list ,function(x,y=r1) crop(y,as(x,"Spatial")))
# 
# 
# 
