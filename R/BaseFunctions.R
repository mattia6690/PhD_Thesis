# 1. Initialize Packages ----

library("devtools")
install_github("mattia6690/Mfunctions");library(Mfunctions)
loadandinstall("raster")
loadandinstall("plyr")
loadandinstall("dplyr")
loadandinstall("tidyr")
loadandinstall("purrr")
loadandinstall("magrittr")
loadandinstall("readr")
loadandinstall("rgeos")
loadandinstall("stringr")
loadandinstall("plotrix")
loadandinstall("gridExtra")
loadandinstall("grid")
loadandinstall("lattice")
loadandinstall("rgdal")
loadandinstall("lubridate")
loadandinstall("reshape2")
loadandinstall("leaflet")
loadandinstall("jsonlite")
loadandinstall("geojsonio")
loadandinstall("RColorBrewer")
loadandinstall("rasterVis")
loadandinstall("Rlof")
loadandinstall("scales")
loadandinstall("caret")
loadandinstall("dlnm")
loadandinstall("modelr")
loadandinstall("DMwR")
loadandinstall("hms")
loadandinstall("tibble")
loadandinstall("Hmisc")
loadandinstall("ggplot2")
loadandinstall("sf")

# 2. Define Directories ----


#* 2.1 Directory1: Local ----
DataDir<-"C:/Users/MRossi/Documents/03_Data/"
RemsenDir<- paste0(DataDir,"01_RemSen/")
  sentineldir<-paste0(RemsenDir,"Sentinel/")
PhenocamDir<- paste0(DataDir,"02_PhenoCam/")
InSitu_dir<-paste0(DataDir,"03_InSitu/")
  dirlai<-paste0(InSitu_dir,"04_LAI/")
  dirhyp<-paste0(InSitu_dir,"05_HyperSpec/2017/")
  dirgps<-paste0(InSitu_dir,"06_GPS/")
  dirfield<-paste0(InSitu_dir,"07_FieldCampaign17/")
  dirstat<-paste0(InSitu_dir,"07_FieldCampaign17/00_Raw/")
MonalisaDir<-paste0(DataDir,"04_MONALISA/")
Monalisa17Dir<-paste0(DataDir,"04a_MONALISA17/")
ProviceDir<- paste0(DataDir,"05_Province/")
MetricsDir<- paste0(DataDir,"06_Metrics/")
ManagementDir<-paste0(DataDir,"Management/")

#* 2.2 Directory2: Server ----
WorkspaceDir<-"Y:/Workspaces/RosM/"
SAO_Vegetationdir <-"U:/SAO/SENTINEL-2/SentinelVegetationProducts/"
SAO_NDVIdir<- paste0(SAO_Vegetationdir,"S2_NDVIMaps_NoLC_noLAEA")
SAO_LAIdir<-  paste0(SAO_Vegetationdir,"SentinelLAI")
SAO_Metadir<- paste0(SAO_Vegetationdir,"Metadata_xmls")

# 2.4. Directory Monalisa Phenocams
MNLS_Phenodir<- "M:/ProjectData/MONALISA/Pillar2/PhenoCam/PhenoCam/"

# 3. Global Input ----
sen2names<-c("Platform","Sensor","Level","GResol","AcqDate","Baseline","Sen2Cor","Tile","ProdDescr","Product","Projection")

# 4.Central Functions ----

#* 4.1. General Functions ----

#' Calculation of the Standard Error
se<-function(x){se<-sd(x,na.rm=T)/length(which(!is.na(x)));return(se)}

#' Convert a Factor to a numeric value
unfac<-function(x){unf<-as.numeric(as.character(x));return(unf)}

#' Source multiple FIles in Direcory
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

# Search for the same strings in two lists
stringinlist<-function(string1,string2){
  
  c1<-multigrepl(string1,string2) %>% string2[.]
  c2<-multigrepl(string2,string1) %>% string1[.]
  
  c3<-c(c1,c2) %>% unique
  return(c3)
  
}

# Write the first Cap as capita
simpleCap <- function(x,first=T) {
  s <- strsplit(x, " ")[[1]]
  if(first==F) paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),sep = "", collapse = " ")
  if(first==T) paste(toupper(substring(s, 1, 1)), substring(s, 2),sep = "", collapse = " ")
}

# Factors to numeric conversion
as.numeric.factor <- function(x) {as.numeric(as.character(x))}

# Save both RDS and CSV
write.RDSCSV<-function(file,name){
  
  write.csv(file,file = paste0(name,".csv"))
  saveRDS(file,file = paste0(name,".rds"))
  
}

# Transforms an lm function output to text usable for ggplots
# Greetings to Jodie Burchell (http://t-redactyl.io/)
r2.equation = function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}


#* 4.2. InSitu Functions ----

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
.remheader<-function(table){table %>% .[5:nrow(.),] %>% setNames(.,.[1,]) %>% .[2:nrow(.),] %>% return(.)}

#' Calculate the Indics from the Hyperspectral Datasets
hypindices<-function(input,wavel1,wavel2,stat="NDVI"){
  
  hyp1<- mean(input[between(input$Wavel,wavel1[1],wavel1[2]),]$Refl)
  hyp2<- mean(input[between(input$Wavel,wavel2[1],wavel2[2]),]$Refl)
  if(stat=="PRI")  hyp <- round((hyp1-hyp2)/(hyp1+hyp2),3)
  if(stat=="NDVI") hyp <- round((hyp2-hyp1)/(hyp2+hyp1),3)
  return(hyp)
  
}

#' Get the Density Limits
.getDensityLimits<-function(selection,method="max"){
  
  den<-density(selection)
  peak<-den$x[den$y==max(den$y,na.rm = T)]
  Iqrmax<-peak+IQR(selection)*1.5
  if(method=="max"){
    upper<-max(selection)
    diff<-upper-peak
    lower<-peak-diff
  } else if(method=="sd"){
    upper<-peak+(2*sd(selection))
    lower<-peak-(2*sd(selection))
  } else{stop("No valid method for Density Function")}
  
  return(c(lower,peak,upper,Iqrmax))
  
}

#* 4.3 Remote Sensing Funtions ----

#' A table indicating the Availability of Sentinel Data
S2_avail<-function(lst,nms){
  
  df<-matrix(ncol=length(nms)) %>% as.data.frame %>% setNames(.,nms)
  for(i in lst){
    
    str<-str_replace(i,".tif","")
    str<-str_replace(str,"_masked","")
    str<-str_split(str,pattern="_")[[1]]
    
    df<-rbind(df,str)
    
  }
  df<-df[-1,] %>% as.tibble
  return(df)
}


listdownload<-function(path,pattern=NULL){
  
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

# Alternative way to plot Raster with 
lp<- function(r,main=""){
  
  colr <- colorRampPalette(brewer.pal(11, 'RdYlBu'))
  colr <- colorRampPalette(c("blue","chocolate","green"))
  rasterVis::levelplot(r,margin=F, colorkey=list(space='bottom'),  
                       par.settings=list(axis.line=list(col='transparent')),
                       scales=list(draw=T),            # suppress axis labels
                       col.regions=colr,
                       at=seq(-10000, 10000, len=1000),main=main)
  
  
}

# Crop Raster by multiple Shapes
cutrasters<-function(r1,shps){

  rasters<-list()
  
  for(j in 1:nrow(shps)){
    
    checker<- st_within(shps[j,],st_as_sfc(st_bbox(r1))) %>% unlist
    name   <- shps$Station[j] %>% as.character %>% simpleCap
    
    if(length(checker)==1){
      
      cropper<-as(extent(shps[j,]),"SpatialPolygons")
      cr<-raster::crop(r1,cropper)
      rasters[[j]]<-cr
      names(rasters)[[j]]<-name
      
    }else{
      
      rasters[[j]]<-NA
      names(rasters)[[j]]<-name
      
    }
  }
  return(rasters)
}

# Mask  one or multiple values with a list of rasters
rasterLmask<-function(rasterList,values){
  
  r<-lapply(rasterList, function(x){
    check<-is.element(values(x),values)
    if(any(check)) x[which(check)]<-NA
    return(x)
  }) 
  
  return(r)
  
}

#* 4.4. Plotting ----

# Make a nice Corplot by excluding one side (less redundancy)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


# Convert a table to Latex and then PNG
table.png <- function(obj, name, dir=NA ,res=100) { 
  first <- name
  name <- paste(name,".tex",sep="")
  sink(file=name)
  cat('
      \\documentclass{report}
      \\usepackage[paperwidth=5.5in,paperheight=7in,noheadfoot,margin=0in]{geometry}
      \\begin{document}\\pagestyle{empty}
      ')
  print(xtable::xtable(obj))
  cat('
      \\end{document}
      ')
  sink()
  tools::texi2dvi(file=name)
  cmd <- paste0("dvipng -D ",res,"in -T tight ", shQuote(paste(first,".dvi",sep="")))
  invisible(system(cmd))
  cleaner <- c(".tex",".aux",".log",".dvi")
  invisible(file.remove(paste(first,cleaner,sep="")))
  
  if(!is.na(dir)) {
    to<-paste0(dir,first,".png")
    from<-paste0(first,"1.png")
    file.copy(from,to,overwrite = T)
  }
}


#* 4.5. Combine Metrics ----

# Check for the nearest date within a range and extrac
nearDate<-function(obj1,obj2,maxdays,valuesonly=T){
  
  nmat<-data.frame()
  for(i in 1:length(s2$Date)){
    
    x<-obj1$Date[i]
    y<-obj2$Date
    diffs<-abs(y-x)
    
    if(min(diffs)<=maxdays){
      neardate<-which(abs(diffs) == min(abs(diffs)))
      neardate<-y[neardate]
      nmat[i,1]<-as.character(x)
      nmat[i,2]<-as.character(neardate)
      nmat[i,3]<-min(diffs)
    }
  }
  if(nrow(nmat)==0) break
  nmat<-nmat[complete.cases((nmat)),]
  
  if(nrow(nmat)==0) break
  # Sentinel Join
  nmat1<-nmat$V1 %>% as.Date %>% as.tibble
  colnames(nmat1)<-"Date"
  d1<-obj1 %>% select(Date,Value) %>% left_join(nmat1,.,by="Date")
  
  # Ground Join
  nmat1<-nmat$V2 %>% as.Date %>% as.tibble
  colnames(nmat1)<-"Date"
  d2<-obj2 %>% select(Date,Value) %>% left_join(nmat1,.,by="Date")
  
  d3<-nmat[,3]
  
  if(valuesonly==F){
    cb<-cbind.data.frame(d1,d2,d3)
    colnames(cb)<-c("Date_obj1","Value_obj1","Date_obj2","Value_obj2","Diff")
  }
  if(valuesonly==T){
    
    cb<-cbind.data.frame(d1$Value,d2$Value,d3)
    colnames(cb)<-c("Value_obj1","Value_obj2","Diff")
    
  }
  return(cb)
  
}

# Add Value from nearest Date for RandomFores Sampling
addNear<-function(oldtab,newtab,col="Sentinel"){
  
  newtab2<-newtab %>% filter(is.na(get(col))) %>% select(Date)
  wh<-which(is.na(newtab[[col]]))
  values<-array()
  
  for(i in 1:nrow(newtab2)){
    date<-newtab2[i,]
    dates<-oldtab %>% select(Date)
    dt<-map(1:nrow(dates), function(i) dates[i,]-date) %>% unlist %>% as.numeric %>% abs
    if(any(dt<5)){
      
      rws<-which.min(dt)
      newval<-oldtab %>% slice(rws) %>% select(eval(col)) %>% as.numeric
      values[[i]]<-newval
    } else values[[i]]<-NA
  }
  
  newtab[[col]][wh]<-values
  return(newtab)
}

# Bind all columns of data frame df together and rename them
allbind<-function(df,y1,y2) cbind(select(df,"Date"),select(df,y1),select(df,y2)) %>% setNames(c("Date","X","Y"))

# Get the LM by two strings for columns
mod_fun<-function(df,y1,y2,glance=T) {
  
  a<-df %>% dplyr::select(y1) %>% unique
  b<-df %>% dplyr::select(y2) %>% unique
  
  if(nrow(a)>1 & nrow(b)>1) {
    
    fit<-lm(get(y2) ~ get(y1), data=df, na.action = na.omit)
    if(glance==T) fit<-glance(fit)
    return(fit)
    
  }else{NA}
  
}

# Get the coefficients for the Cross correlation function
ccf_fun<-function(df,y1,y2) {
  
  a<-df %>% dplyr::select(c(y1,y2)) %>% na.omit
  if(nrow(a)>0) {
    c<-Ccf(a[,1],a[,2],plot=F,lag.max = 20)
    d<-cbind.data.frame(c[[4]],c[[1]])
    names(d)<-c("Lags","R")
    return(d)
  }
  
}

# Link the Locations
loclink2<-map(stations,function(x) {
  cbind.data.frame(x,
                   c("ID1","ID2","ID3","ID4"),
                   c(paste0(x,"_A"),paste0(x,"_B"),paste0(x,"_C"),paste0(x,"_D")))
}) %>% 
  do.call(rbind,.) %>% 
  setNames(c("Station","OP1","ROI")) %>% 
  add_column(Date=NA,.before=T) %>% as.tibble


