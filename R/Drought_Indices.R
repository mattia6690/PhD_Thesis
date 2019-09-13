
library("devtools")
devtools::install_github("https://github.com/mattia6690/MonalisR/tree/dev")
library("raster")
library("MonalisR")
library("tibble")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")
library("purrr")
library("readr")
library("stringr")
library("GADMTools")
library("tmap")
library("tmaptools")


source("R/Drought_functions.R")

# Import  -------------------------------------------------------------
# ** Tables -----------------------------------------------------------------

data.raw<-readRDS("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/BoundData2.rds")
max.spi.year  <-2018
min.spi.year  <-1979

date.interval <-as.Date(paste(min.spi.year,"01","01",sep="-")) %--% as.Date(paste(max.spi.year,"12","31",sep="-"))


# SPI
spiSeverity<-read.table(file = "spitab.txt",header = T,sep = " ",stringsAsFactors = F) %>% 
  as_tibble %>% 
  mutate(Index=as.factor(c(1:nrow(.))))

gg.x.start<-as.Date("2016-01-01")
gg.x.end  <-as.Date("2019-01-01")

spi.rect<-tibble(x1=rep(gg.x.start,nrow(spiSeverity)),
                 x2=rep(gg.x.end,nrow(spiSeverity)),
                 y1=spiSeverity$From,
                 y2=spiSeverity$To,
                 Severity=factor(spiSeverity$Value,levels = unique(spiSeverity$Value)),
                 col=spiSeverity$Color)

# Growing Seasons

season.rect<-tibble(x1=c(as.Date("2017-04-01"),as.Date("2018-04-01")),
                    x2=c(as.Date("2017-10-01"),as.Date("2018-10-01")),
                    y1=c(-3,-3),
                    y2=c(3,3))

swc.rect<-tibble(x1=c(as.Date("2017-04-01"),as.Date("2018-04-01")),
                    x2=c(as.Date("2017-10-01"),as.Date("2018-10-01")),
                    y1=c(0,0),
                    y2=c(.5,.5))


# ** Spatial Data ----------------------------------------------------------


# MeteoStations
STstations <-MonalisR::getMeteoInfo(format = "spatial") %>% dplyr::select(SCODE,NAME_D,NAME_I,NAME_E,ALT,geometry) %>% unique

# Bozen
ItalySHP   <-gadm_sf_loadCountries("ITA", level=3)$sf
BzSHP      <-ItalySHP %>% filter(NAME_2=="Bolzano")
BzDCT      <-st_read("C:/Users/MRossi/Documents/03_Data/Shapes/00_General/Districts_polygon.shp") %>% 
  mutate(District=paste(NAME_I,NAME_D,sep=" / ")) %>% 
  st_transform(4326) %>% 
  mutate(InArea=map(NAME_D,function(x)ifelse(x=="Vinschgau","Val Venosta / Vinschgau District","Other Districts"))) %>% 
  mutate(Nome_dct=NAME_I)
BzDCT2 <- BzDCT %>% dplyr::select(BEZ,Nome_dct,District,InArea)
STstations2 <- st_join(STstations,BzDCT2)
vinschgau<-filter(BzDCT2,InArea=="Val Venosta / Vinschgau District")

MaziaSHP   <- st_read("Y:/Workspaces/RosM/Extent/Mazia.shp") %>% st_transform(4326)

# Grasslands
Grasslands <-st_read("Y:/Workspaces/RosM/03_KMLs/Paper2.kml") %>% 
  st_zm(.) %>% 
  mutate(Name2=str_replace(Name,"_"," ")) %>% 
  mutate(Name3=map(Name2,function(x) str_split(x," ")[[1]][1]))
Grasslands_box <- sfRectBuff(Grasslands,distx=.01,disty=0.008)
Grasslands_pnt <- st_collection_extract(Grasslands,type="POINT")
Grasslands_ply <- st_collection_extract(Grasslands,type="POLYGON") 
Grasslands_proj<- as.character(st_crs(Grasslands))[2]

# Data Tidying------------------------------------------------------------

data.raw2 <-data.raw %>% 
  filter(Date %within% date.interval) %>% 
  group_by(Date,SCODE,SCODE2,Sensor,Statistics) %>% 
  summarise(Value=mean(Value,na.rm=T)) %>% 
  ungroup

data.spread<-data.raw2  %>% 
  unite(New,Sensor,Statistics,sep="_") %>% 
  spread(New,Value) %>% 
  arrange(Date,SCODE) %>% 
  group_by(SCODE,SCODE2) %>% 
  nest

data.spread.year<-data.spread %>% 
  mutate(subtab=map(data,function(x){
    
    x %>%
      complete(Date=seq.Date(as.Date(paste0(min.spi.year,"-01-01")),
                             as.Date(paste0(max.spi.year,"-12-31")),by="day")) %>% 
      mutate(Month=month(Date)) %>% 
      mutate(Year=year(Date)) 
      
  }))


# SPI Calculation ----------------------------------------------------------
# ** Table Generation ------------------------------------------------------

library("spi")
library("precintcon")

# Calculate the SPI by Station
d.spi.raw<- data.spread.year %>% 
  mutate(subtab.spi=map(subtab,function(x){
    
    x %>% 
      group_by(Year,Month) %>% 
      summarize(Value=sum(N_Sum,na.rm = F)) %>% 
      ungroup
    
  }))
  
d.spi.fil<- filter(d.spi.raw,map_int(subtab.spi, function(x) length(which(is.na(x)))) < 25) 
d.spi<-d.spi.fil %>% 
  mutate(SPI1=map(subtab.spi,function(x){
    
    write.table(x,file="spi2.txt",quote=FALSE,row.names=FALSE,col.names = TRUE,sep=",")
    sdat<-precintcon::read.data("spi2.txt", header = T)
    spi2<-precintcon::spi(sdat,1)
    spi3<-mutate(spi2,Date=as.Date(paste(year,sprintf("%02d",month),"01",sep="-")))
    spi4<-mutate(spi3,Severity=map_chr(spi,function(x,v=spiSeverity){
      
      ret<-as.character(v[which(x>v$From & x<v$To),"Value"])
      
    }))
    spi5<-as_tibble(spi4)
  }))


d.spi.all<-d.spi %>% 
  dplyr::select(SCODE,SPI1) %>% 
  unnest %>% 
  mutate(Severity=factor(Severity))

spistats.inarea <- STstations2 %>% dplyr::select(SCODE,Nome_dct,ALT,District,InArea)
d.spi.all2<-left_join(d.spi.all,spistats.inarea,by="SCODE") %>% 
  unnest(InArea) %>% 
  mutate(Name2=paste(Nome_dct,"-",SCODE)) %>% 
  mutate(Name2=paste(Name2,"-",ALT,"m")) %>% 
  mutate(InArea=factor(InArea,levels=c("Val Venosta / Vinschgau District","Other Districts")))


# ** Spatial --------------------------------------------------------------

codes <- d.spi$SCODE %>% unique %>% as.character()
spistats <- STstations2 %>% dplyr::filter(is.element(SCODE,codes))
vinschgaustats <- spistats %>% filter(InArea=="Vinschgau")

# ** Plots ----------------------------------------------------------------

gg.spi<-ggplot()+ theme_bw()+
  geom_rect(data=spi.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=Severity),alpha=.2)+
  scale_fill_manual(values = spi.rect$col)+
  geom_rect(data=season.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill=NA,col="black")+
  geom_line(data=d.spi.all2,aes(Date,spi))+
  geom_point(data=d.spi.all2,aes(Date,spi))+
  facet_wrap(InArea~Name2)+
  scale_x_date(breaks = "3 months",limits=c(gg.x.start,gg.x.end),labels=date_format("%Y-%m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(c(-3.5,3.5))+
  ylab("SPI value")+
  labs(title="Standard Precipitation Index in South Tyrol from 2016 to 2018")

ggsave(gg.spi,filename = "Paper2/SPIplot_all_rev.png",device="png",width=13,height=9)

# SPEI Calculation --------------------------------------------------------

library("scPDSI")
library("SPEI")
library("Evapotranspiration")

coords<- st_coordinates(STstations2)

STstations3<-STstations2 %>% 
  add_column(LAT=st_coordinates(STstations2)[,1]) %>% 
  as_tibble %>% 
  dplyr::select(SCODE,LAT)

data<-left_join(data.spread.year,STstations3,by="SCODE") %>% 
  dplyr::select(-data)

subtab.all<-data %>% 
  mutate(Drought=map2(subtab,LAT,function(x,y){
    
    tab<- x %>% 
      group_by(Year,Month) %>% 
      summarize(LT_meanM=mean(LT_Mean,na.rm = T),
                NS_sumM=sum(N_Sum,na.rm = T)) %>% 
      ungroup %>% 
      as.data.frame()
    
    
    tab$PET   <- SPEI::thornthwaite(tab$LT_meanM,y,na.rm = T) %>% as.numeric
    tab$BAL   <- tab$NS_sumM-tab$PET
    tab$SPEI  <- SPEI::spei(tab[,'BAL'], 1,na.rm = T)$fitted %>% as.numeric
    tab$SPI   <- SPEI::spi (tab[,'NS_sumM'], 1,na.rm = T)$fitted %>% as.numeric
    tab$Date  <- as.Date(paste(tab$Year,sprintf("%02d",tab$Month),"01",sep="-"))
    
    return(tab)
    
  }))


subtab.all.fill<- filter(subtab.all,is.element(SCODE,codes)) 
d.spei<-subtab.all.fill %>% dplyr::select(SCODE,Drought) %>% unnest

d.spei2<-left_join(d.spei,spistats.inarea,by="SCODE") %>% 
  unnest(InArea) %>% 
  mutate(Name2=paste(Nome_dct,"-",SCODE)) %>% 
  mutate(Name2=paste(Name2,"-",ALT,"m")) %>% 
  mutate(InArea=factor(InArea,levels=c("Val Venosta / Vinschgau District","Other Districts"))) %>% 
  dplyr::select(-c(Year,Month,LT_meanM,NS_sumM,PET,BAL,geometry)) %>% 
  gather(key="Indices",value=Value,SPEI,SPI)

gg.x.start<-as.Date("2016-01-01")
gg.x.end  <-as.Date("2019-01-01")

gg.spei<-ggplot()+ theme_bw()+
  geom_rect(data=spi.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,fill=Severity),alpha=.2)+
  scale_fill_manual(values = spi.rect$col)+
  geom_rect(data=season.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill=NA,col="black")+
  geom_line(data=d.spei2,aes(Date,Value,lty=Indices))+
  geom_point(data=d.spei2,aes(Date,Value,pch=Indices))+
  facet_wrap(InArea~Name2)+
  scale_x_date(breaks = "3 months",limits=c(gg.x.start,gg.x.end),labels=date_format("%Y-%m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylim(c(-3.5,3.5))+
  ylab("SPI / SPEI values")+
  labs(title="Drought indices in South Tyrol from 2016 to 2018")

ggsave(gg.spei,filename = "Paper2/SPEIplot_all_rev.png",device="png",width=13,height=9)


# Thematic Maps ---------------------------------------------------------
# ** Site -----------------------------------------------------------------

dataras <- read_osm(bb(Grasslands_box),type="osm",zoom=15)
dataras.proj<- projectRaster(dataras,crs=Grasslands_proj)
col<-c("red","blue","darkmagenta")

tmsite<-tm_shape(dataras.proj) +
  tm_rgb() +
  tm_grid(ticks = T,alpha = 0)+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, .5, 1), size = .8,position = "left")+
  tm_layout(legend.bg.color = "white",legend.frame = T)+
  tm_xlab("Longitude")+
  tm_add_legend(type = "symbol",labels="Stations")+
  tm_add_legend(type = "fill",labels="Sites",col=F)+
  tm_shape(Grasslands_box)+
  tm_borders(col=col[1],lwd = 2)+
  tm_shape(Grasslands_pnt[1,])+
  tm_dots(col=col[2],size=.5)+
  tm_shape(Grasslands_ply[1,])+
  tm_borders(col=col[2],lwd = .5)+
  tm_text(text = "Name3",size=1,ymod=3,bg.color = "white")+
  tm_shape(Grasslands_pnt[2,])+
  tm_dots(col=col[3],size=.5)+
  tm_shape(Grasslands_ply[2,])+
  tm_borders(col=col[3],lwd = .5)+
  tm_text(text = "Name3",size=1,ymod=2,bg.color = "white")

tmview<-tm_shape(BzDCT)+
  tm_fill("grey95")+
  tm_shape(vinschgau)+
  tm_borders("orange")+
  tm_shape(MaziaSHP)+
  tm_borders("green",lty=2)+
  tm_shape(Grasslands_box)+
  tm_borders("red")


# ** South Tyrol ----------------------------------------------------------

tmraw<-tm_shape(BzDCT)+
  tm_grid(col = "grey90",ticks = T)+
  tm_fill("grey95")+
  tm_borders("grey")+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 20, 40), size = .8,position = "left")+
  tm_text("NAME_I",size=.5,ymod=-.5)+
  tm_xlab("Longitude")+
  tm_ylab("Latitude")+
  tm_layout(legend.bg.color="white",legend.frame = T)+
  tm_shape(BzDCT)+
  tm_text("NAME_D",size=.5,ymod=-1)+
  tm_shape(MaziaSHP)+
  tm_borders("green",lty=2)+
  tm_add_legend(labels="Mazia/Matsch Valley Extent",col="white",border.col="green")+
  tm_shape(Grasslands_box)+
  tm_borders("red")+
  tm_add_legend(labels="Research Area",col="white",border.col="red")


# ** Vinschgau ------------------------------------------------------------
tmvi<-tm_shape(vinschgau)+
  tm_grid(col = "grey90",ticks = T)+
  tm_fill("grey95")+
  tm_borders("orange")+
  tm_compass(type = "8star", position = c("left", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10), size = .8,position = "left")+
  tm_xlab("Longitude")+
  tm_ylab("Latitude")+
  tm_layout(legend.bg.color="white",legend.frame = T)+
  tm_add_legend(labels="Venosta/Vinschgau Valley Extent",col="white",border.col="orange")+
  tm_shape(MaziaSHP)+
  tm_borders("green",lty=2)+
  tm_add_legend(labels="Mazia/Matsch Valley Extent",col="white",border.col="green")+
  tm_shape(Grasslands_box)+
  tm_borders("red")+
  tm_add_legend(labels="Research Area",col="white",border.col="red")



tmstat<-tmraw + 
  tm_shape(spistats)+
  tm_symbols("ALT",size=1,
             breaks = seq(250,2250,500),title.col ="Altitude of Meteorological Stations",
             palette="Blues")


tmstat.vi<-tmvi+
  tm_shape(vinschgaustats)+
  tm_symbols("ALT",size=1,breaks = seq(250,2250,500),title.col ="Altitude of Meteorological Stations",palette="Blues")+
  tm_text("SCODE",ymod=1)

tmap_save(tmstat.vi,filename = "Paper2/Vinschgau.png",width = 2000,height = 2000,
          insets_tm = tmview,
          insets_vp = viewport(0.81, 0.848, width = 0.35, height = 0.3))





# Plotting ----------------------------------------------------------------

arrg<-tmap_arrange(tmstat,tmsite,sync=T,widths = c(.63,.37),ncol=2)
tmap_save(arrg,filename = "Paper2/ResearchArea2.png",width = 3500,height = 1300)

ggplot(atib2.plotT,aes(Date,Value,color=Statistics)) + 
  geom_line(linetype="dotted")+ 
  scale_color_manual(values=c("red","black","blue"))+
  scale_x_date(breaks="2 month")

ggplot(atib2.plotMaxT,aes(Date,Value,color=Statistics)) + 
  geom_point()+ 
  geom_line(linetype="dashed")+
  scale_color_manual(values=c("red"))+
  scale_x_date(breaks="2 month")+
  ggtitle("Maximum Temperature per day")+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 50), se = FALSE,col="blue")

atib2.plotN<-atib2 %>% 
  filter(Sensor=="N") %>% 
  gather(key="Statistics",value="Value",Sum)

ggplot(atib2.plotN,aes(Date,Value))+
  geom_bar(stat="identity",fill="blue")+
  scale_x_date(breaks="2 month")


# Temperature Metrics
atib.T <- atib2 %>% 
  filter(Sensor=="LT") %>% 
  mutate(Frost=as.numeric(Min<0)) %>% 
  mutate(GDD10=((Max+Min)/2-10)) %>% 
  mutate(GDD10=replace(GDD10,GDD10<0,0)) %>% 
  mutate(GDD10CS=cumsum(GDD10))
  
# Precipitation Metrics

atib.N <- atib2 %>% 
  filter(Sensor=="N")

ggplot(atib.N,aes(Date,Sum))+geom_bar(stat="identity")

# SnowFall_Probability

nst<-atib %>% group_by(TimeStamp) %>% nest



e <- 1/(10^(decimals+1))
s <- seq((-5),5,e)
norm<-pnorm(rev(s),sd = 1)
mat<-cbind(LT=s,Prob=norm)

atib_snowP<- atib %>% 
  filter(Sensor=="LT") %>% 
  mutate(SnowP=sapply(Value,function(x,m=mat,decimals=1){
    
    x <- round(x,decimals)
    w1<- which(m[,1]>=x)
    w2<- which(m[,1]<=x)
    
    ret<-mean(m[min(w1),2],m[max(w1),2],na.rm = T)
    return(ret)
    
  }))
  
  


if (is.null(url)) 
  url <- "http://daten.buergernetz.bz.it/services/meteo/v1/sensors"

ui <- cbind(sapply(u, "[[", 1), sapply(u, "[[", 2)) %>% 
  as.tibble
colnames(ui) <- c("SCODE", "Sensor")

ui <- sapply(u,magrittr::extract)

ret<-left_join(ui,getMeteoStat())
as.sf<-st_as_sf(ret2,coords = c("LONG","LAT"),crs=4326,na.fail = F)


install.packages("stars")
library("stars")
library("raster")

lf<-list.files("U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps_NoLC_noLAEA/T32TPS/",pattern = "masked",full.names = T)

ptm<-proc.time()
star<-read_stars(lf[1])
proc.time()-ptm

ptm<-proc.time()
ras<-raster(lf[1])
proc.time()-ptm



# Uncommented -------------------------------------------------------------

# # OLD GGPLOTS for Station SPI
# 
# d.spi.plots<-d.spi %>% 
#   mutate(GG=map2(SCODE,SPI1,function(xx,yy){
#     
#     
#     yy$Severity <- factor(yy$Severity, levels = unique(spiSeverity$Value))
#     gg.spi<-ggplot(yy,aes(Date,spi,fill=Severity))+ 
#       geom_bar(stat="identity")+
#       scale_x_date(breaks = "12 months", labels=date_format("%Y-%m"))+
#       theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#       ylim(c(-3,3))+
#       ylab("Mean SPI value")+
#       labs(title=paste("Standard Precipitation Index at Station",xx),
#            subtitle= "SPI crom 1978 to 2018")
#     
#   }))
# 
# # OLD By whole SouthTyrol
# 
# gg.spi<-ggplot(d.spi.scode,aes(Date,Mean,fill=Value))+ 
#   geom_bar(stat="identity")+
#   scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Mean SPI value")+
#   labs(title="Standard Precipitation Index across meteorological Stations in South Tyrol",
#        subtitle= "SPI since 2015 based on a 40 year Time Series on 54 Stations")
# 
# 
# d.spi.scode<-d.spi2 %>% 
#   select(SCODE,SPI1) %>% 
#   unnest %>% 
#   mutate(month=as.factor(month)) %>% 
#   group_by(Date,year,month) %>% 
#   summarize(Mean=mean(spi),
#             Max=max(spi),
#             Min=min(spi)) %>% 
#   mutate(Severity=map(Mean,function(x,t=spiSeverity){
#     
#     ret<-t[which(x>t$From & x<t$To),]
#     return(ret)
#     
#   })) %>% unnest %>% ungroup
# 
# 
# gg.spi<-ggplot(d.spi.scode,aes(Date,Mean,fill=Value))+ 
#   geom_bar(stat="identity")+
#   scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#   ylab("Mean SPI value")+
#   labs(title="Standard Precipitation Index across meteorological Stations in South Tyrol",
#        subtitle= "SPI since 2015 based on a 40 year Time Series on 54 Stations")
# 



# d1<-data.raw %>% filter(Sensor=="N") %>% 
#   filter(Statistics=="Sum") %>%  
#   group_by(SCODE,SCODE2) %>% 
#   nest
# 
# d2<-d1$data[[1]] %>% 
#   complete(Date=seq.Date(as.Date("1968-01-01"),
#                          as.Date(paste0(max.year,"-12-31")),by="day")) %>% 
#   mutate(Month=month(Date)) %>% 
#   mutate(Year=year(Date)) %>% 
#   select(Year,Month,Value) %>% 
#   group_by(Year,Month) %>% 
#   filter(Year<=max.year) %>% 
#   summarize(Value=sum(Value,na.rm = F)) %>% 
#   ungroup



# * SPI1 --------------------------------------------------------------------


# mutate(SPI2=map(subtab,spi))
# 
# tester<-d3$data[[1]]
# subtab<- tester %>% 
#   complete(Date=seq.Date(as.Date("1978-01-01"),
#                          as.Date(paste0(max.year,"-12-31")),by="day")) %>% 
#   mutate(Month=month(Date)) %>% 
#   mutate(Year=year(Date)) %>% 
#   filter(Year<=max.year) %>% 
#   filter(Year>=min.spi)
# 
# subtab.spi<- subtab %>% 
#   group_by(Year,Month) %>% 
#   summarize(Value=sum(N_Sum,na.rm = F)) %>% 
#   ungroup
# 
# 
# # SPI calculated with the SPI Package
# spi.tab1<- subtab.spi  %>% 
#   spread(Year,Value) %>% 
#   as.data.frame()
# 
# write.table(spi.tab1,file="spi.txt",quote=FALSE)	  
# spi1<-spi::spi(3,"spi.txt",1978,2018,title = "O",output = "test.txt")
# 
# # SPI calculated with the Precintcon Package
# write.table(subtab.spi,file="spi2.txt",quote=FALSE,row.names=FALSE,col.names = TRUE,sep=",")
# sdat<-precintcon::read.data("spi2.txt", header = T)
# spi2<-precintcon::spi(sdat)

# 
# 
# 
# # Wiski request
# library("readr")
# library("readxl")
# 
# req<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/WHISKI_neededData_RossiMattia.csv")
# wsk<-read_xlsx("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Whiski_Stations.xlsx",sheet = 2)
# 
# req2<-req %>% rename(NUMMER=SCODE)
# lj<-left_join(req2,wsk,by="NUMMER") %>% 
#   filter(PARAMETER=="LT"|PARAMETER=="N") %>% 
#   filter(!is.null(REXCHANGE)) %>% 
#   arrange(NUMMER)
# 
# write_csv(lj,"WHISKY_Request_RoM_2207.csv")

# Soil Water Potential
# swp.tab<-atib.all %>% filter(MetricS=="SWP" & Statistic=="Avg")
# ylab<-paste(unique(swc.tab$MetricL),"in",unique(swc.tab$Unit))
# 
# gg.swp<-ggplot(swp.tab,aes(Date,Mean,col=Plot,lty=Plot))+ theme_light() +
#   geom_point(alpha=.15)+
#   facet_grid(vars(Station),vars(Depth))+
#   ylab(ylab)+
#   labs(title="Soil Water Potential in the Muntatschinig Area",
#        subtitle= "Spline Interpolated Time Series 2017 and 2018")+
#   geom_smooth(method = lm, formula = y ~ splines::bs(x, 20), se = FALSE)+
#   scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# ggsave(gg.swp,filename="swp_plot.png",device="png",height=6,width=12,units="in")


# COMBINED SWC AND SWP PLOT
# atib2.avg<-atib.all %>% filter(Statistic=="Avg")
# ggplot(atib2.avg,aes(Date,Mean,pch=Station,linetype=Station))+ theme_bw()+
#   geom_point(alpha=.5)+
#   geom_line()+
#   facet_wrap(.~Key,scales = "free")
# 
# saveRDS(object = atib.all,file = "C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/SWPSWC_Vinschgau_2017-2018_Daily.rds")


