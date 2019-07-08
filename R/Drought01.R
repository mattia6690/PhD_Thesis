
library(devtools)
devtools::install_github("https://github.com/mattia6690/MonalisR@dev")
library("MonalisR")
library("tibble")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")

s <- "2017-01-01 00:00"
e <- "2019-01-01 00:00"

SCODE="02500MS"
gm<-getMeteoSensor(SCODE = SCODE)

temp<-downloadMeteo(station_code = SCODE,
                 sensor_code = "LT",
                 datestart = s,
                 dateend = e)

ns<-downloadMeteo(station_code = SCODE,
                 sensor_code = "N",
                 datestart = s,
                 dateend = e)

atib<-rbind(as_tibble(temp),as_tibble(ns)) %>% 
  mutate(Date=as.Date(TimeStamp)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date))
atib2<- atib %>% 
  group_by(Date,Year,Month,Station,Sensor) %>% 
  summarize(Mean=mean(Value),
            Max=max(Value),
            Min=min(Value),
            Stdev=sd(Value),
            Sum=sum(Value)) %>% 
  ungroup()
  

atib2.plot<-atib2 %>% gather(key="Statistics",value="Value",Mean,Max,Min,Stdev)

ggplot(atib2,aes(Date,Value,color=Variable)) + 
  geom_line()+ 
  scale_color_manual(values=c("red","black","blue"))

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



