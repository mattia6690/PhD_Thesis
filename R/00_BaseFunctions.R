
#################
# Required Functions and Dependencies
#################

library("devtools")
install_github("mattia6690/Mfunctions");library(Mfunctions)
loadandinstall("raster")
loadandinstall("readr")
loadandinstall("rgeos")
loadandinstall("stringr")
loadandinstall("plotrix")
loadandinstall("gridExtra")
loadandinstall("grid")
loadandinstall("lattice")
loadandinstall("rgdal")
loadandinstall("lubridate")
loadandinstall("ggmap")
loadandinstall("tidyverse")
loadandinstall("reshape2")
loadandinstall("leaflet")
loadandinstall("jsonlite")
loadandinstall("geojsonio")

source("C:/Users/MRossi/Documents/07_Codes/ProvinceST_R_Access/R/ProvinceST_StationDownload.R")

# Define Folder Locations
# Input
RemSenFolder1<- "C:/Users/MRossi/Documents/03_Data/01_RemSen/"
Workspacedir<-"Y:/Workspaces/RosM/"
SAO_Vegetationdir <-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/"
SAO_NDVIdir<- paste0(SAO_Vegetationdir,"/S2_NDVIMaps")
SAO_LAIdir<-  paste0(SAO_Vegetationdir,"/SentinelLAI")
SAO_Metadir<- paste0(SAO_Vegetationdir,"/Metadata_xmls")

Monalisa_dir<-"C:/Users/MRossi/Documents/03_Data/04_MONALISA/"

InSitu_dir<-"C:/Users/MRossi/Documents/03_Data/03_InSitu/"

# Output


# Standard Error Function
se<-function(x){se<-sd(x,na.rm=T)/length(which(!is.na(x)));return(se)}

# Factor to Numeric
unfac<-function(x){unf<-as.numeric(as.character(x));return(unf)}

hypindices<-function(input,wavel1,wavel2,stat="NDVI"){
  
  hyp1<- mean(input[between(input$Wavel,wavel1[1],wavel1[2]),]$Refl)
  hyp2<- mean(input[between(input$Wavel,wavel2[1],wavel2[2]),]$Refl)
  if(stat=="PRI")  hyp <- round((hyp1-hyp2)/(hyp1+hyp2),3)
  if(stat=="NDVI") hyp <- round((hyp2-hyp1)/(hyp2+hyp1),3)
  return(hyp)
  
}

Create_availability_table_SA<-function(lst,nms){
  
  df<-matrix(ncol=length(nms)) %>% as.data.frame %>% setNames(.,nms)
  for(i in lst){
    
    str<-str_replace(i,".tif","")
    str<-str_replace(str,"_masked","")
    str<-str_split(str,pattern="_")[[1]]
    str[1]<-str[1] %>% str_split(.,"/") %>% unlist %>% .[2]
    
    df<-rbind(df,str)
    
  }
  df<-df[-1,] %>% as.tibble
  return(df)
}

