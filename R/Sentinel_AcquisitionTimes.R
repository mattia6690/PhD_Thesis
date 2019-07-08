
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
library(ggplot2)
library(rlang)
library(plotly)

tempdir<-"C:/Users/MRossi/Documents/Temp/"
outname<-paste0(tempdir,"S2_Tiles_lac_v2.shp")
outline<-st_read(outname)

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
  inter3<-st_intersection(outline,sr)
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

satellites2<-lj4 %>% as.data.frame() %>% 
  select(Satellite) %>% lapply(.,function(i) str_split(i,"-",simplify = T)) %>% 
  as.data.frame() %>% .[,2]

lj4<-lj4 %>% mutate(Sentinel=satellites2)


fileout <- paste0(sentineldir,"AcquisitionPlan_",from,"_to_",to,"_LAC")
ggtitle <- paste("Sentinel 2 Acquisition Plan",from,"to",to,"- Large Alpine Convention")

sti<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol") %>% as(.,"sf")

g1<-ggplot()+
  geom_sf(data=outline)+
  geom_sf(data=lj4,aes(fill=Satellite))+
  geom_sf(data=sti,color="black",fill=NA)+
  facet_wrap(~ObservationTimeStart,ncol=6)+
  theme(panel.grid.major.x = element_line(color = "grey", linetype = 2))+
  scale_fill_manual(values=c("darkgrey", "lightgrey"))+
  ggtitle(ggtitle)

ggsave(g1,filename=paste0(fileout,".png"), width = 297, height = 210, units = "mm")

tilefreq<-table(lj2$Tile)
ljtile<-lj3 %>% 
  dplyr::select(ObservationTimeStart,Satellite,Tile) %>% 
  distinct(.,.keep_all=T)

table<-ljtile %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry) %>% 
  tidyr::separate(.,ObservationTimeStart,c("Date","Time"),sep="T")

write.csv(table,paste0(fileout,".csv"))
saveRDS(table,paste0(fileout,"rds"))

freq<-lj4 %>% 
  as.data.frame %>% 
  dplyr::select(Tile) %>% 
  table %>% 
  as.data.frame() %>% 
  setNames(c("Tile","Frequency"))

lj5<-left_join(lj4,freq)
lj5$Date<-lj5$ObservationTimeStart %>% as_date

g2<-ggplot()+
  geom_sf(data=outline)+
  geom_sf(data=lj5,aes(fill=Frequency,text=paste("<b> Sentinel",Sentinel,"</b> flight on",Date)))


# inter3<-inter2 %>% 
#   mutate(
#     lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
#     lat = map_dbl(geometry, ~st_centroid(.x)[[2]])
#   ) 
# 
# sti<-readOGR("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","SouthTyrol") %>% as(.,"sf")
# ggplot(inter3) + 
#   geom_sf() +
#   geom_sf(data=sti,color="red")+
#   geom_text(aes(label = Tile, x = lon, y = lat), size = 5) +
#   ggtitle("Sentinel 2 Tiles in the Large Alpine Convention")+
#   scale_fill_distiller(palette = "Spectral")

