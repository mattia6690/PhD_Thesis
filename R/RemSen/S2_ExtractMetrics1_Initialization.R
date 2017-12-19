
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")

# 2. Input ----
#* 2.1 Read the MONALISA Data ----

oi<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland")
oi_shape<-readOGR(paste0(WorkspaceDir,"01_Data/KML"),"MONALISA Grassland Shapefile")

mnls_lf<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"),full.names = T)
mnls_lf.short<-list.files(paste0(MonalisaDir,"02_Download/Download_csv_20170405_1547"))
mnls_stations<-do.call(rbind,strsplit(mnls_lf.short, "\\_|\\-| ")) %>% .[,2]

#* 2.2 Read the Sentinel Data ----

sao_ndvi_lf<-list.files(SAO_NDVIdir,pattern=".tif",full.names = T,recursive = T)
sao_ndvi_lf.short<-list.files(SAO_NDVIdir,recursive=T,pattern=".tif")

#* 2.3 Define Global Input ----

sen2tile<-"T32TPS"
sen2proj<-"LAEA"
NArange <-c(-10000,10000)
year<-"2017"


# 3. Tidy ----
#* 3.1 Tidy the Table ----

df<-S2_avail(sao_ndvi_lf.short,sen2names) %>% 
  add_column(dir=sao_ndvi_lf)

df<-df %>% 
  expand(df,nesting(mnls_stations)) %>% 
  filter(Tile==sen2tile) %>% 
  filter(Projection==sen2proj)

sao_dates<-df %>%
  filter(substr(AcqDate,1,4)==year) %>%
  dplyr::select(dir)

sao_ndvi_lf<- sao_dates %>% 
  as.matrix %>% 
  unlist %>% 
  unique

#* 3.2 Change Projection ----
proj<-"+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
oi2<-spTransform(oi,CRS(proj))
oi2_shape<-spTransform(oi_shape,CRS(proj))
oi2@data$Name<-oi2@data$Name %>% tolower
oi2_shape@data$Name<-oi2_shape@data$Name %>% tolower

#* 3.3 Generate Buffer ----
oi2buff<-gBuffer(oi2,byid=T,width=1500)
