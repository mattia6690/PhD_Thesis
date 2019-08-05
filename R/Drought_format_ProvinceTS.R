
library("tibble")
library("magrittr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lubridate")
library("purrr")
library("readr")
library("stringr")

# Whiski Michael------------------------------------------------------------------

load("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/clim series wiski v2 - daily.rda")
tbl.m1<-as_tibble(dt.clim) %>% 
  mutate(Sensor=param) %>% 
  mutate(Sensor=str_replace_all(Sensor,"snow.height","HS_Mean")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"pre"  ,"N_Sum")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"tmax" ,"LT_Max")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"tmin" ,"LT_Min")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"tmean","LT_Mean")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"tmax" ,"LT_Mean")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"rad"  ,"GS_Mean")) %>% 
  mutate(Sensor=str_replace_all(Sensor,"relhum" ,"RH_Mean"))

tbl.m2<-tbl.m1 %>% 
  rename(SCODE2=st.id) %>% 
  rename(Date=date) %>% 
  rename(Value=value) %>% 
  select(-param) %>% 
  separate(Sensor,c("Sensor","Statistics"),"_")

saveRDS(tbl.m2,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_Mich_Stats.rds")
rm(tbl.m1)


# Whiski Bart -------------------------------------------------------------

tbl.b1<-readRDS("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Rossi_N_T_bound.rds")

tbl.b2<-tbl.b1 %>% 
  filter(Status==40) %>% 
  rename(SCODE=SANR) %>%
  mutate(SCODE2=substr(SCODE,1,4)) %>% 
  rename(Sensor=CNAME) %>% 
  mutate(Date=as_date(TimeStamp)) %>% 
  mutate(year=year(Date)) %>% 
  mutate(month=month(Date)) %>% 
  mutate(day=month(Date))

tbl.bsel<- tbl.b2 %>% 
  group_by(SCODE,SCODE2,year,month,day,Date,Sensor) %>% 
  summarize(Sum=sum(Value),
            Mean=mean(Value),
            Min=min(Value),
            Max=max(Value),
            Stdev=sd(Value)) %>% 
  ungroup

saveRDS(tbl.bsel,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_Bart_Stats_raw.rds")

tbl.bstat<-tbl.bsel %>% gather(key="Statistics",value="Value",Mean,Max,Min,Sum,Stdev)
saveRDS(tbl.bstat,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_Bart_Stats.rds")


# OpenData ----------------------------------------------------------------

# If you want to download -> Takes ages and is therefore uncommented

# s <- "2016-01-01 00:00"
# e <- "2019-01-01 00:00"
# library("devtools")
# devtools::install_github("https://github.com/mattia6690/MonalisR/tree/dev")
# library("MonalisR")
#
# gm<-getMeteoInfo(format = "spatial")  %>% select(DATE,SCODE,ALT,NAME_D,TYPE,DESC_D,UNIT,VALUE)
# vi_stats<-buffmeteo(as_Spatial(gm),10000) %>% as_tibble %>% select(Province) %>% unique %>% unlist %>% as.character
# 
# vi_stats<-buffmeteo(as_Spatial(gm),10000) %>% as_tibble %>% select(Province) %>% unique %>% unlist %>% as.character
# 
# 
# temp<-downloadMeteo2(station_code = vi_stats,
#                      sensor_code = c("LT","N","HS"),
#                      datestart = s,
#                      dateend = e)

od.clim<-readRDS("C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_OpenData.rds")

tbl.o1<-od.clim %>% 
  mutate(SCODE=as.character(SCODE)) %>% 
  mutate(SCODE2=substr(SCODE,1,4)) %>% 
  mutate(Sensor=as.character(Sensor)) %>% 
  mutate(Date=as.Date(TimeStamp)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  select(-c(Start,End))

tbl.o2<- tbl.o1 %>% 
  group_by(Date,Year,Month,SCODE,SCODE2,Sensor) %>% 
  summarize(Mean=mean(Value),
            Max=max(Value),
            Min=min(Value),
            Stdev=sd(Value),
            Sum=sum(Value)) %>% 
  ungroup()
saveRDS(tbl.o2,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_OpenData_Stats_raw.rds")

tbl.o3<-tbl.o2 %>% gather(key="Statistics",value="Value",Mean,Max,Min,Sum,Stdev)
saveRDS(tbl.o3,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/Vinschgau_OpenData_Stats.rds")


# Join Tables -------------------------------------------------------------
# Add the sources to the tables

tbl.mich<-tbl.m2 %>% mutate(Source="Michael")
tbl.open<-tbl.o3 %>% mutate(Source="OpenData")
tbl.bart<-tbl.bstat %>% mutate(Source="WISKI")

# Join the Codes

atib.cd <- tbl.open %>% select(SCODE,SCODE2) %>% unique
tbl1.cd <- tbl.mich %>% select(SCODE2) %>% unique

codes.join <- left_join(atib.cd,tbl1.cd,by="SCODE2")

# Append the SCODE to the WHISKI Database

wiski <- left_join(tbl.mich,codes.join,by="SCODE2") %>% 
  select(Date,SCODE,SCODE2,Source,Sensor,Statistics,Value)

openda <- tbl.open %>% 
  select(Date,SCODE,SCODE2,Source,Sensor,Statistics,Value)

wiski2 <-left_join(tbl.bart,codes.join,by=c("SCODE","SCODE2")) %>% 
  select(Date,SCODE,SCODE2,Source,Sensor,Statistics,Value)

bound<-rbind(wiski,openda,wiski2) %>% arrange(Date,SCODE2)
bound2<-bound %>% filter(!is.na(SCODE))

saveRDS(bound2,"C:/Users/MRossi/Documents/03_Data/03_InSitu/02_Province/BoundData2.rds")


# Append Station Name
gm<-getMeteoInfo()

lj<-left_join(bound2,gm,by="SCODE")

stats_order<-bound2 %>% select(SCODE) %>% unique %>% left_join(.,gm) %>% select(SCODE,NAME_D,NAME_I,ALT,LONG,LAT) %>% unique
write_csv(stats_order,"WHISKI_neededData_RossiMattia.csv")
