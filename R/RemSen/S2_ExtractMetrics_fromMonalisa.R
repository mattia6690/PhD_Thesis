
# 1. Initialization ----

# Source other Scripts
source("R/BaseFunctions.R")

# 2. Input ----
#* 2.1 Read the MONALISA Data ----

oi<-readOGR(paste0(Workspacedir,"01_Data/KML"),"MONALISA Grassland")
oi_shape<-readOGR(paste0(Workspacedir,"01_Data/KML"),"MONALISA Grassland Shapefile")

mnls_lf<-list.files(paste0(Monalisa_dir,"02_Download/Download_csv_20170405_1547"),full.names = T)
mnls_lf.short<-list.files(paste0(Monalisa_dir,"02_Download/Download_csv_20170405_1547"))
mnls_stations<-do.call(rbind,strsplit(mnls_lf.short, "\\_|\\-| ")) %>% .[,2]

#* 2.2 Read the Sentinel Data ----

sao_ndvi_lf<-list.files(SAO_NDVIdir,pattern=".tif",full.names = T,recursive = T)
sao_ndvi_lf.short<-list.files(SAO_NDVIdir,recursive=T,pattern=".tif")

#* 2.3 Define Global Input ----

sen2tile<-"T32TPS"
sen2proj<-"LAEA"


# 3. Tidy ----
#* 3.1 Tidy the Sentinel Data ----

df<-S2_avail(sao_ndvi_lf.short,sen2names) %>% add_column(dir=sao_ndvi_lf)

df<-df %>% expand(df,nesting(mnls_stations)) %>% 
  filter(Tile==sen2tile) %>% 
  filter(Projection==sen2proj)

sao_dates<-df %>%filter(AcqDate>2017) %>%
  dplyr::select(dir)

sao_ndvi_lf<- sao_dates %>% as.matrix %>% unlist %>% unique


# 4. Extract Metrics ----

# Central For Iteration
for(i in 1:length(sao_ndvi_lf)){

  # Load and Mask
  start<-Sys.time()
  scene<-sao_ndvi_lf[i]
  
  r2<-rasterNA(r1,c(-10000,10000))
  df_scene<-df %>% filter(dir==scene)
  df_wh<-df_scene %>% rownames %>% as.array
  
  msk<-round(length(wh_ras)/ncell(r2)*100,2)
  df[df_wh,"masked_tot"]<-msk
  print(paste0("#0 Data Loaded - Masked Pixel: ",msk,"%"))

  # Reproject the Data
  oi2<-spTransform(oi,CRS(projection(r2)))
  oi2@data$Name<-oi2@data$Name %>% tolower
  oi2_shape<-spTransform(oi_shape,CRS(projection(r2)))
  oi2_shape@data$Name<-oi2_shape@data$Name %>% tolower
  print("#1 Preprocessing DONE")

  # Processing of the Point Data
  eP<- t5[cellFromXY(r2,oi2)]
  eP<- cbind(oi2$Name,eP) %>% as.tibble
  eP$dir<-df_scene %>% select(dir) %>% unique %>% rep(times=nrow(.)) %>% as.character
  colnames(eP)<-c("mnls_stations","Points","dir")
  print("#2 Pixel Analysis DONE") # Syso

  # Processing of the Shapefiles
  eS<-extract2(r2,oi2_shape,narm = T)
  colnames(eS)<-paste0("Original_",colnames(eS))
  eS2<-extract2(r2,oi2_shape,weight=T,narm=T)
  colnames(eS2)<-paste0("Weigted_",colnames(eS2))
  
  eS3<- cbind(eS,eS2) %>% as.tibble
  eS3$dir<-df_scene %>% select(dir) %>% unique %>% rep(times=nrow(.)) %>% as.character
  eS3$mnls_stations<-oi2_shape$Name
  print("#3 Shape Analysis DONE")
  
  # Join Point and Shapefile Outcome
  end<-Sys.time()
  diff<-round(end-start,2)
  jn<-right_join(eP,eS3,by="mnls_stations")
  metricsList[[i]]<-jn
  print(paste0("Iteration ",i," of ",length(sao_ndvi_lf)," DONE [",diff," s]"))
}

# 5. Tidy Metrics ----

# Create Plottable Data Frame
ml<-do.call(rbind,metricsList)
ml2<-ml %>% filter(mnls_stations=="vimes1500")
ml2$date<-S2_dir2date(ml2$dir.x)
ml2$nas<-ml2$Original_Nas/ml2$Original_Ncells %>% `/`(100)

# 6. Plot Metrics ----

#* 6.1 GGplots ----
ggplot(ml2,aes(date,Weigted_Mean))+
  geom_point(aes(y=Original_Mean,color="Shape All"))+
  geom_point(aes(color="Shape Weighted"))+
  geom_smooth(aes(color="Shape Weighted"),linetype=3,se=F)+
  geom_point(aes(y=as.numeric(Points),color="Point"))+
  scale_x_date()+
  ylab("NDVI value")+xlab("Date")+
  labs(title="Sentinel 2 NDVI Mean in 2017")+
  scale_colour_manual(breaks=c("Shape All","Shape Weighted","Point"),
                      values=c("red","cyan","blue"))

#* 6.2 Levelplot alternative ----

# Alternative way to plot Raster with 
colr <- colorRampPalette(brewer.pal(11, 'RdYlBu'))
colr <- colorRampPalette(c("blue","chocolate","green"))
rasterVis::levelplot(r2,margin=F, colorkey=list(space='bottom'),  
          par.settings=list(axis.line=list(col='transparent')),
          scales=list(draw=T),            # suppress axis labels
          col.regions=colr,
          at=seq(-10000, 10000, len=1000),main="Test")

