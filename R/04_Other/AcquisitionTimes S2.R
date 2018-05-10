
source("R/BaseFunctions.R")
library(rvest)
library(httr)
library(stringr)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(Rcpp)
#devtools::install_github("tidyverse/ggplot2")   # You need the newest possible ggplot version (>2.2.1) with geom_sf() function
library(ggplot2)

tempdir<-"C:/Users/MRossi/Documents/08_Temp/"
outname<-paste0(tempdir,"S2_Tiles_lac.shp")
inter2<-st_read(outname)

outline<-inter2 %>% group_by(Inside_AC) %>% summarize()

s2kmltable<-function(kmlfile){
  
  rl<-readLines(kmlfile)
  re1 <- grep("                  <name>" ,rl)
  if(length(re1)==0) re1 <- grep("\t\t\t\t\t<name>" ,rl)
  
  start<-re1[-length(re1)]
  end<-re1[-1]
  
  test<-map2(start,end,function(i,j){
    
    re2<-rl[i:j]
    re2rep<-str_replace_all(re2,"                  ","")
    
    str2<-"<Data name" %>% grep(.,re2rep) %>% re2rep[.]
    names<-str_split(str2,pattern='"') %>% lapply(.,function(i) i[2]) %>% unlist
    
    str3<-"<value" %>% grep(.,re2rep) %>% re2rep[.]
    values <- str_match(str3, ">(.*?)<")[,2]
    
    dings<-rbind(names,values)
    return(dings)
    
  })
  
  namesall<-lapply(test,function(i) length(i[1,])) %>% unlist %>% unique
  
  if(length(namesall)==1){
    
    namesfinal<-test[[1]][1,]
    kmltable<-test %>% lapply(function(i) i[2,]) %>% 
      do.call(rbind,.) %>% 
      as.tibble %>% 
      setNames(namesfinal)
    return(kmltable)
  }else warning("Uneven rows")
  
}

# Harvest Data From Server
starturl<-"https://sentinel.esa.int"
url<-paste0(starturl,"/web/sentinel/missions/sentinel-2/acquisition-plans")
webpage <- read_html(url)

names<-c('div.sentinel-2a','div.sentinel-2b')

urls<-map(names,function(i){
  
  titleA<-html_nodes(webpage,i)
  matched <- str_match_all(titleA, "<a href=\"(.*?)\"")[[1]][1:2,2]
  urls<-paste0(starturl,matched)
  
}) %>% unlist

satellite<- urls %>% 
  unlist %>% 
  basename %>% 
  map(.,function(i) str_split(i,pattern="_") %>% unlist %>% .[1]) %>% 
  unlist

maps<-map2(urls,satellite,function(url,sa){
  print(url)
  
  name<- basename(url)
  file<- paste0(tempdir,name)
  if(!file.exists(file)) download.file(url,file)
  sr<-st_read(file)
  
  table<-s2kmltable(file) %>% 
    mutate(Name=ID)
  inter3<-st_intersection(inter2,sr)
  inter3$Satellite=sa
  lj<-left_join(inter3,table,by="Name") %>% 
    select(Satellite,Tile,ObservationTimeStart,ObservationTimeStop,Scenes) %>% 
    arrange(ObservationTimeStart)
})

lj2<-do.call(rbind,maps)
lj3<-lj2 %>% 
  arrange(ObservationTimeStart) %>% 
  mutate(Satellite2=as.numeric(ifelse(Satellite=="Sentinel-2B",2,1)))

dates<-as_date(lj3$ObservationTimeStart)

from<-dates %>% min
to<-dates %>% max

lj4<-lj3[which(dates>(Sys.Date()-7)),]





fileout <- paste0(sentineldir,"AcquisitionPlan_",from,"_to_",to,"_LAC")
ggtitle <- paste("Sentinel 2 Acquisition Plan",from,"to",to,"- Large Alpine Convention")

g1<-ggplot()+
  geom_sf(data=outline)+
  geom_sf(data=lj4,aes(fill=Satellite))+
  facet_wrap(~ObservationTimeStart,ncol=7)+
  theme(panel.grid.major.x = element_line(color = "grey", linetype = 2))+
  ggtitle(ggtitle)

ggsave(g1,filename=paste0(fileout,".png"),device="png",width=14,height=10)

tilefreq<-table(lj2$Tile)
ljtile<-lj3 %>% 
  select(ObservationTimeStart,Satellite,Tile) %>% 
  distinct

table<-ljtile %>% 
  as.data.frame() %>% 
  select(-geometry) %>% 
  separate(.,ObservationTimeStart,c("Date","Time"),sep="T")

write.csv(table,paste0(fileout,".csv"))
saveRDS(table,paste0(fileout,"rds"))
