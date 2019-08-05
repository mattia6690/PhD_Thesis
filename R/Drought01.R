
library("devtools")
devtools::install_github("https://github.com/mattia6690/MonalisR/tree/dev")
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
# Import Data -------------------------------------------------------------

data.raw<-readRDS("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/BoundData2.rds")
max.year<-2018
min.spi <-1978

# SPI Calculation --------------------------------------------------------------

library("spi")
library("precintcon")

stats<-MonalisR::getMeteoInfo(format="spatial")
coords<-sf::st_coordinates(stats)
stats.coords<-bind_cols(SCODE=stats$SCODE,LON=coords[,1],LAT=coords[,2])

d3<-data.raw  %>% 
  unite(New,Sensor,Statistics,sep="_") %>% 
  spread(New,Value) %>% 
  arrange(Date,SCODE) %>% 
  left_join(stats.coords,by="SCODE") %>% 
  group_by(SCODE,SCODE2) %>% 
  nest

d4<-d3 %>% 
  mutate(subtab=map(data,function(x){
    
    x %>% 
      complete(Date=seq.Date(as.Date("1978-01-01"),
                             as.Date(paste0(max.year,"-12-31")),by="day")) %>% 
      mutate(Month=month(Date)) %>% 
      mutate(Year=year(Date)) %>% 
      filter(Year<=max.year) %>% 
      filter(Year>=min.spi)
  }))

d.spi.raw<- d4 %>% 
  mutate(subtab.spi=map(subtab,function(x){
    
    x %>% 
      group_by(Year,Month) %>% 
      summarize(Value=sum(N_Sum,na.rm = F)) %>% 
      ungroup
    
  }))

d.spi<-d.spi.raw %>% 
  mutate(SPI1=map(subtab.spi,function(x){
    
    write.table(x,file="spi2.txt",quote=FALSE,row.names=FALSE,col.names = TRUE,sep=",")
    sdat<-precintcon::read.data("spi2.txt", header = T)
    spi2<-precintcon::spi(sdat)
    
  }))


spitab<-read.table(file = "spitab.txt")

d.spi2<-d.spi %>% 
  select(SCODE,SPI1) %>% 
  unnest %>% 
  mutate(Date=paste(year,sprintf("%02d",month),"01",sep="-")) %>% 
  mutate(Date=as.Date(Date)) %>% 
  filter(year>=2015) %>% 
  filter(year<2019) %>% 
  filter(!spi>5) %>% 
  mutate(month=as.factor(month))

d.spi.scode<-d.spi2 %>% 
  group_by(Date,year,month) %>% 
  summarize(Mean=mean(spi),
            Max=max(spi),
            Min=min(spi)) %>% 
  mutate(Severity=map_chr(Mean,function(x,t=spitab){
    
    ret<-as.character(t$Value[which(x>t$From & x<t$To)])
    return(ret)
    
  })) %>% ungroup


gg.spi<-ggplot(d.spi.scode,aes(Date,Mean,col=Mean))+ 
  geom_line()+
  geom_point()+
  scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot SPI

spi.trans<-spi2 %>% 
  as_tibble %>% 
  mutate(Date=paste(year,sprintf("%02d",month),"01",sep="-")) %>% 
  mutate(Date=as.Date(Date))

g1<-ggplot(spi.trans,aes(Date,spi))+ 
  geom_point(aes(col=as.factor(year)))+
  geom_line(linetype="dotted")

g2<-g1 +
  scale_x_date(limits=c(as.Date("2010-01-01"),as.Date("2018-12-31")))

# PDSI Calculation --------------------------------------------------------

library("scPDSI")
library("SPEI")
library("Evapotranspiration")

subtab.et<- subtab %>% 
  group_by(Year,Month) %>% 
  summarize(LT_meanM=mean(LT_Mean,na.rm = F),
            NS_sumM=sum(N_Sum)) %>% 
  ungroup %>% 
  as.data.frame()

val<-subtab.et$LT_meanM
lat<-unique(subtab$LAT) %>% .[!is.na(.)]

subtab.et$PET   <- SPEI::thornthwaite(val,lat,na.rm = T) %>% as.numeric
subtab.et$BAL   <- subtab.et$NS_sumM-subtab.et$PET
subtab.et$SPEI  <- SPEI::spei(subtab.et[,'BAL'], 1,na.rm = T)$fitted %>% as.numeric
subtab.et$SPI   <- SPEI::spi(subtab.et[,'NS_sumM'], 1,na.rm = T)$fitted %>% as.numeric

subtab.et.tib   <- as.tibble(subtab.et) %>% 
  mutate(Date=paste(year,sprintf("%02d",month),"01",sep="-")) %>% 
  mutate(Date=as.Date(Date))

# Plotting ----------------------------------------------------------------

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


# SWC and SWP -------------------------------------------------------------

# Function to Tidy Alpenv Data
readSWCAlpEnv<-function(table,Station="",headRows,headNames,TimeCol,StartCol){
  
  Time    <- table[headRows+1:length(table$X1),TimeCol]
  TimeCHR <- map(Time,function(x) as_datetime(as.character(x),format="%Y-%m-%d %H:%M"))
  
  data.seqC<-c(1+StartCol:ncol(table)-1)
  data.seqR<-c(headRows+1:nrow(table))
  
  head<- table[1:headRows,startcol:ncol(table)]%>% 
    t %>% 
    as_tibble %>% 
    setNames(headNames) %>% 
    add_column(ID=c(1:nrow(.)),.before = T)
  
  
  data<- head %>% 
    mutate(Data=map(ID,function(x,t=table,hr=data.seqR,sc=data.seqC,time=TimeCHR){
      
      ret <- t[hr,sc[x]]
      ret <- as_tibble(cbind(time,ret))
      ret <- setNames(ret,c("TimeStamp","Value"))
      ret <- mutate(ret,Value=as.numeric(Value))
      return(ret)
      
    }))
  
  meta<- data %>% 
    add_column(Station=Station,.before = T) %>% 
    unnest
  
  return(meta)
  
}

headRows<-3
timecol <-1
startcol<-2
headNames<-c("Key","Unit","Statistic")

b2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/B2_2017-2018.csv",skip = 1,col_names = F)
p2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/P2_2017-2018.csv",skip = 1,col_names = F)

t1<-readSWCAlpEnv(b2,Station = "Vimes1500",
              headRows = 3, headNames = c("Key","Unit","Statistic"),
              TimeCol = 1, StartCol = 2)

t2<-readSWCAlpEnv(p2,Station = "P2",
                  headRows = 3, headNames = c("Key","Unit","Statistic"),
                  TimeCol = 1, StartCol = 2)


complete<-rbind(t1,t2)

atib<-complete %>% 
  mutate(Date=date(TimeStamp)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Day=day(Date)) %>% 
  mutate(Value=as.numeric(Value))

atib.gr1<- atib %>% 
  group_by(Date,Station,Key,Statistic, Unit) %>% 
  summarize(Mean=mean(Value),
            Max=max(Value),
            Min=min(Value),
            Stdev=sd(Value),
            Sum=sum(Value)) %>% 
  ungroup

atib.all<- atib.gr1 %>% 
  mutate(MetricS=lapply(Key,function(x){
    
    sp <- strsplit(x,"_")[[1]]
    bind_cols(MetricS = sp[1], Plot = sp[3], Depth = paste(sp[4],"cm"), Statistic = sp[5])
    
  })) %>% 
  unnest %>% 
  mutate(MetricL=sapply(MetricS,function(x) ifelse(x=="SWC","Soil Water Content","Soil Water Potential")))


atib2.avg<-atib.all %>% filter(Statistic=="Avg")
ggplot(atib2.avg,aes(Date,Mean,pch=Station,linetype=Station))+ theme_bw()+
  geom_point(alpha=.5)+
  geom_line()+
  facet_wrap(.~Key,scales = "free")

saveRDS(object = atib.all,file = "C:/Users/MRossi/Documents/03_Data/03_InSitu/08_SWC/SWPSWC_Vinschgau_2017-2018_Daily.rds")


# Plot both Soil Water Content and Soil Water Potential not in the same plot
# Soil Water Content
swc.tab<-atib.all %>% filter(MetricS=="SWC" & Statistic=="Avg")
ylab<-paste(unique(swc.tab$MetricL),"in",unique(swc.tab$Unit))

gg.swc<-ggplot(swc.tab,aes(Date,Mean,col=Plot,lty=Plot))+ theme_light() +
  geom_point(alpha=.15)+
  facet_grid(vars(Station),vars(Depth))+
  ylab(ylab)+
  labs(title="Soil Water Content in the Muntatschinig Area",
       subtitle= "Spline Interpolated Time Series 2017 and 2018")+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 20), se = FALSE)+
  scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(gg.swc,filename="swc_plot.png",device="png",height=6,width=12,units="in")

# Soil Water Potential
swp.tab<-atib.all %>% filter(MetricS=="SWP" & Statistic=="Avg")
ylab<-paste(unique(swc.tab$MetricL),"in",unique(swc.tab$Unit))

gg.swp<-ggplot(swp.tab,aes(Date,Mean,col=Plot,lty=Plot))+ theme_light() +
  geom_point(alpha=.15)+
  facet_grid(vars(Station),vars(Depth))+
  ylab(ylab)+
  labs(title="Soil Water Potential in the Muntatschinig Area",
       subtitle= "Spline Interpolated Time Series 2017 and 2018")+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 20), se = FALSE)+
  scale_x_date(breaks = "2 months", labels=date_format("%Y-%m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(gg.swp,filename="swp_plot.png",device="png",height=6,width=12,units="in")


# Uncommented -------------------------------------------------------------



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




# Wiski request
library("readr")
library("readxl")

req<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/WHISKI_neededData_RossiMattia.csv")
wsk<-read_xlsx("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Whiski_Stations.xlsx",sheet = 2)

req2<-req %>% rename(NUMMER=SCODE)
lj<-left_join(req2,wsk,by="NUMMER") %>% 
  filter(PARAMETER=="LT"|PARAMETER=="N") %>% 
  filter(!is.null(REXCHANGE)) %>% 
  arrange(NUMMER)

write_csv(lj,"WHISKY_Request_RoM_2207.csv")


