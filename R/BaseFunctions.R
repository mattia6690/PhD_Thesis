# 1. Initialize Packages ----

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
loadandinstall("RColorBrewer")
loadandinstall("rasterVis")

# 2. Define Directories ----


#* 2.1 Directory1: Local ----
RemSenFolder1<- "C:/Users/MRossi/Documents/03_Data/01_RemSen/"
InSitu_dir<-"C:/Users/MRossi/Documents/03_Data/03_InSitu/"
  dirlai<-paste0(InSitu_dir,"/04_LAI")
  dirhyp<-paste0(InSitu_dir,"/05_HyperSpec")
  dirstat<-paste0(InSitu_dir,"/07_FieldCampaign17/00_Raw")
Monalisa_dir<-"C:/Users/MRossi/Documents/03_Data/04_MONALISA/"
ProviceFolder<- "C:/Users/MRossi/Documents/03_Data/06_Province/"

#* 2.2 Directory2: Workspace ----
Workspacedir<-"Y:/Workspaces/RosM/"

#* 2.3 Directory3: SAO Server ----
SAO_Vegetationdir <-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/"
SAO_NDVIdir<- paste0(SAO_Vegetationdir,"S2_NDVIMaps")
SAO_LAIdir<-  paste0(SAO_Vegetationdir,"SentinelLAI")
SAO_Metadir<- paste0(SAO_Vegetationdir,"Metadata_xmls")


# 3. Global Input ----

sen2names<-c("Platform","Sensor","Level","GResol","AcqDate","Baseline","Sen2Cor","Tile","ProdDescr","Product","Projection")
metricsList<-list()

# 4.Central Functions ----

#* 2.1. General Functions ----

#' Calculation of the Standard Error
se<-function(x){se<-sd(x,na.rm=T)/length(which(!is.na(x)));return(se)}

#' Convert a Factor to a numeric value
unfac<-function(x){unf<-as.numeric(as.character(x));return(unf)}

#* 2.2. InSitu Functions ----

#' Initialize the InSitu Data Table
Insitu.init<-function(table,tableColumn,pattern,names){
  
  lst<-list()
  header<-.getheader(table)
  stat  <-header$X2[1]
  dat   <-header$X2[2] %>% as.numeric %>% sprintf("%06d", .)
  # Create Filename
  table2  <-.remheader(table)
  tab<-table2[table2$Type==pattern,]
  tab<-tab[,which(grepl("ID",colnames(tab)))]
  
  
  # Create the Matrix
  field_mat1<-matrix(ncol=length(names),
                     nrow=(length(tableColumn)*nrow(tab))) %>%  as.data.frame
  colnames(field_mat1)<-names
  
  field_mat1$FOI   <-stat
  field_mat1$Date  <-dat
  field_mat1$SubID <-rep(tableColumn,each=nrow(tab))
  field_mat1$Sample<-rep(seq(1,nrow(tab),1),times=length(tableColumn))
  lst[[1]]<-field_mat1
  lst[[2]]<-tab
  
  return(lst)
}


#' Get the Header from the In-Situ Table I created
.getheader<-function(table){table %>% slice(.,head1) %>% return(.)}

#' Remove the Header from the In-Situ Table I created
.remheader<-function(table){table %>% slice(5:nrow(.)) %>% setNames(.,.[1,]) %>% slice(2:nrow(.)) %>% return(.)}

#' Calculate the Indics from the Hyperspectral Datasets
hypindices<-function(input,wavel1,wavel2,stat="NDVI"){
  
  hyp1<- mean(input[between(input$Wavel,wavel1[1],wavel1[2]),]$Refl)
  hyp2<- mean(input[between(input$Wavel,wavel2[1],wavel2[2]),]$Refl)
  if(stat=="PRI")  hyp <- round((hyp1-hyp2)/(hyp1+hyp2),3)
  if(stat=="NDVI") hyp <- round((hyp2-hyp1)/(hyp2+hyp1),3)
  return(hyp)
  
}

#* 2.3 Remote Sensing Funtions ----

#' A table indicating the Availability of Sentinel Data
S2_avail<-function(lst,nms){
  
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


listdownload<-function(path){
  
  lf1<-list.files(path,full.names = T)
  lf<-list.files(path,full.names = F)
  
  spl<-str_split(lf,"_") %>% do.call(rbind,.)
  spl<-spl[,1:2] %>% as.tibble()
  spl<- spl %>% add_column(lf1)
  colnames(spl)<-c("CODE","Product","Path")
  return(spl)
}

#' Convert the Directory to a Date Object
S2_dir2date<- function(dirlist){
  
  date<-str_split(dirlist,"/") %>% sapply(., "[[", 7)
  date<-str_split(date,"_") %>% sapply(., "[[", 5)
  date<-as.Date(date,format="%Y%m%d")
  return(date)
}

#' Function for automatic replacement of rasters in a certain range
rasterNA<-function(scene,range,proc.time=F){
  
  start.rasterNA<-Sys.time()
  r1<- raster(scene)
  t5<- values(r1)
  wh_ras<-which(t5<range[1]|t5>range[2])
  t5[wh_ras]<-NA
  r2<-setValues(r1,as.numeric(t5))
  rm(r1)
  end.rasterNA<-Sys.time()
  if(proc.time==T) print(end.rasterNA-start.rasterNA)
  
  return(r2)
  
}
