library("stringr")
library("raster")
library("rgdal")
library("tidyverse")

# Read the Available Files 

NDVIsaodir<-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps"
LAIsaodir<-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/SentinelLAI"
Metasaodir<-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/Metadata_xmls"
RemSenoutdir<-"C:/Users/MRossi/Documents/03_Data/01_RemSen"

lf<-list.files("U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps",recursive=T,pattern="tif")
names<-c("Platform","Sensor","Level","GResol","AcqDate","Baseline","Sen2Cor","Tile","ProdDescr","Product","Projection")


NDVI_table_SA<-function(lst,nms){
  
  df<-matrix(ncol=length(names)) %>% as.data.frame %>% setNames(.,names)
  for(i in lf){
    
    str<-str_replace(i,".tif","")
    str<-str_replace(str,"_masked","")
    str<-str_split(str,pattern="_")[[1]]
    str[1]<-str[1] %>% str_split(.,"/") %>% unlist %>% .[2]
    
    df<-rbind(df,str)
    
  }
  df<-df[-1,] %>% as.tibble
  return(df)
}

df<-NDVI_table_SA(lf,names)
write.csv(df,paste0(RemSenoutdir,"/Sentinel_2A/Availabe_SAO_Products_List.csv"))

df_prod<-df %>% select(matches("Product")) %>% unique %>% as.character
df_date<-df %>% filter(Platform=="S2A") %>% select(matches("AcqDate")) %>% 
  as.matrix %>% as.Date(.,"%Y%m%d") %>% table %>% data.frame %>% as.tibble()

x11()
g1<-ggplot(df_date,aes(y=Freq,x=as.Date(.)))+
  geom_bar(stat="identity")+
  ggtitle(paste("Number of available Sentinel 2A",df_prod,"Scenes per Date"))+
  ylab("No acquisitions")+ xlab("Date")+
  scale_y_continuous(breaks=seq(1,10,1))

ggsave(g1,filename = paste0(RemSenoutdir,"/Sentinel_2A/Product_availability per date.png"))


lf<-list.files("U:/SAO/SENTINEL-2/SentinelVegetationProducts/S2_NDVIMaps",recursive=T,pattern="tif",full.names=T)


