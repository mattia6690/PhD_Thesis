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
  dirfield<-paste0(InSitu_dir,"04_FieldCampaign/")
  dirlai<-paste0(InSitu_dir,"04a_LAI/")
  dirhyp<-paste0(InSitu_dir,"04b_Spectrometer/")
  dirbio<-paste0(InSitu_dir,"04c_Biomass/")
  dirgps<-paste0(InSitu_dir,"04d_GPS/")
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



# Get hyperspectral Data
getVals.spectrometer<-function(data,date,directory,verbose=F,addGPS=T){
  
  # Prepare
  
  scale2<-"Spectrometer"
  hy.raw <- data.raw %>% 
    add_column(Scale2=scale2,.after = "Scale1") %>% 
    filter(Type=="Hyperspectral")  %>% 
    filter(!is.na(Acquisition)) %>% 
    mutate(Acquisition=as.numeric(Acquisition)) %>% 
    mutate(File=map2_chr(Acquisition, Date, function(x,y) {
      
      hdate<- format(y,"%m%d%y")
      ret  <- paste("HRPDA",hdate,sprintf("%04d",x),"sig",sep=".")
      
    })) %>% 
    mutate(Directory=paste0(dirhyp,"01_Raw/",year(date),"/"))
  
  if(verbose==T) print(paste("A total of",nrow(hy.raw),"Spectrometers measured"))
  # Availability
  
  existance<-file.exists(paste0(hy.raw$Directory,hy.raw$File))
  hy.raw<-hy.raw[which(existance==T),]
  
  if(verbose==T) print(paste("A total of",nrow(hy.raw),"Spectrometers available"))
  
  hy.data <- hy.raw %>% 
    mutate(RawData=map2(Directory,File,function(x,y){
      
      fname<-paste0(x,y)
      t<-read_table(fname,col_types = cols("/*** Spectra Vista SIG Data ***/" = col_character()))
      colnames(t)<-"data"
      t1<-tidyr::separate(t,data,c("Key","Values"),"=")
      
      return(t1)
      
    })) %>% 
    mutate(Metadata=map(RawData,function(x) {
      
      y<-x %>% dplyr::filter(!is.na(Values))
      return(y)
      
    })) %>% 
    mutate(Data=map(RawData,function(x){
      
      y<- x %>% 
        dplyr::filter(is.na(Values)) %>% 
        select(-Values) %>% 
        separate(.,col="Key", into=c("Wavel","Rup","Rdn","Refl"),sep="  ") %>% 
        mutate_all(as.numeric)
      return(y)
      
    })) %>% select(-RawData)
  
  #** Add GPS Information
  if(addGPS==T){
    
    hy.data<- hy.data %>% 
      mutate(Location=map(Metadata,function(x){
        
        Lat <- x %>% 
          filter(Key=="latitude") %>% 
          select(Values) %>% 
          str_replace_all("N","") %>%  
          str_split(",") %>% 
          unlist %>% 
          as.numeric %>% 
          mean(na.rm=T) %>% "/" (100)
        Lon <- x %>% 
          filter(Key=="longitude") %>% 
          select(Values) %>% 
          str_replace_all("E","") %>%  
          str_split(",") %>% 
          unlist %>% 
          as.numeric %>% 
          mean(na.rm=T) %>% "/" (100)
        
        LatLon<-bind_cols(Lat=Lat,Lon=Lon)
        
        return(LatLon)
        
      })) %>% unnest(Location)
    
  }
  return(hy.data)
}

# Get LAI Data
getVals.LAI<-function(data,date,directory,verbose=F){
  
  # Prepare the Values
  scale2<-"Leaf Area"
  date2<-format(date,"%d%m%y")
  lai.raw <- data %>% 
    filter(Type==scale2)  %>% 
    filter(!is.na(Acquisition)) %>% 
    add_column(Scale2=scale2,.after = "Scale1") %>% 
    mutate(File=map_chr(Acquisition,function(x) paste0(date2,"_",x,".csv"))) %>% 
    mutate(Directory=paste0(directory,"01b_Renamed/"))
  
  # Extract and add the Values
  lai.data <- lai.raw %>% 
    mutate(Data=pmap(.,function(Directory,File,...){
      
      Prop2  <-c("Meta","Metrics","GPS")
      Meta   <-c("LAI_FILE","VERSION","DATE")
      Metrics<-c("LAI","SEL","ACF","DIFN","MTA","SEM","SMP")
      GPS    <-c("GPSLAT","GPSLONG")
      rd     <-suppressMessages(read_csv(paste0(Directory,"/",File)))
      
      rd1<-rd %>% filter(V1 %in% Meta) %>% select(V1,V2) %>% setNames(c("Prop3","Value")) %>% 
        mutate(Prop2=Prop2[1])
      rd2<-rd %>% filter(V1 %in% Metrics) %>% select(V1,V2) %>% setNames(c("Prop3","Value"))%>% 
        mutate(Prop2=Prop2[2])
      rd3<-rd %>% filter(V1 %in% GPS) %>% select(V1,V2) %>% setNames(c("Prop3","Value"))%>% 
        mutate(Prop2=Prop2[3])
      
      ret<-rbind(rd1,rd2,rd3)
      return(ret)
      
    }))
  
  # Combine the values
  if(nrow(lai.data)>0){
    
    lai.final<-lai.data %>% 
      unnest %>% 
      select(names) %>% 
      mutate(Value=as.character(Value))
    return(lai.final)
    
  } else { return(lai.data)}
}

# Get Biomass Data
getVals.biomass<-function(data,date,directory,verbose=F){
  
  #Prepare
  scale2<-"Biomass"
  prop2 <-"Phytomass"
  bio.raw <- data.raw %>% 
    filter(Type==scale2) %>% 
    mutate(Prop2=prop2) %>% 
    add_column(Scale2=scale2,.after = "Scale1") %>% 
    mutate(Acquisition=as.numeric(Acquisition)) %>% 
    group_by(Date,Station,Scale1,Scale2,Prop1,Prop2) %>% 
    nest
  
  #Calculate Metrix
  bio.data <- bio.raw %>% 
    mutate(Metrics=map(data,function(x){
      
      v<- mutate(x,Name=substr(Name,1,2)) %>% spread(Name,Acquisition)
      
      if(is.na(v$W2)) v$W2 <- v$W1
      
      Wet   <- v$W3 - v$T3
      Dry   <- v$W4 - v$T3
      Water <- round((1-(Dry/Wet))*100,2)
      Error <- round(abs((v$W2 - v$T1) - (v$W3 - v$T3)),2)
      
      ret<-rbind(c("Wet",Wet),
                 c("Dry",Dry),
                 c("Water Percentage",Water),
                 c("Error",Error)) %>% 
        as_tibble(.) %>% 
        setNames(c("Prop3","Value")) %>% 
        mutate(Values=as.character(Value))
    }))
  
  #Combine Metrics
  bio.final<- bio.data %>% 
    select(-data) %>% 
    unnest %>% 
    select(Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
    arrange(Date,Station) %>% 
    mutate(Value=as.character(Value))
  
  return(bio.final)
  
  
}

ndveg.indices<-function(input,lowrange,highrange,stat="highlow",round=3){
  
  if(length(lowrange)!=2 | length(highrange)!=2) stop("You have to put a range consisting of 2 numbers for both ranges")
  
  low  <- input %>% 
    dplyr::filter(between(Wavel,lowrange[1],lowrange[2])) %>% 
    select(Refl) %>% 
    unlist %>% 
    mean(.,na.rm=T)
  high <- input %>% 
    filter(between(Wavel,highrange[1],highrange[2])) %>% 
    select(Refl) %>% 
    unlist %>% 
    mean(.,na.rm=T)
  
  if(stat=="lowhigh")  hyp <- round((low-high)/(low+high),round)
  if(stat=="highlow")  hyp <- round((high-low)/(high+low),round)
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

# # Link the Locations
# loclink2<-map(stations,function(x) {
#   cbind.data.frame(x,
#                    c("ID1","ID2","ID3","ID4"),
#                    c(paste0(x,"_A"),paste0(x,"_B"),paste0(x,"_C"),paste0(x,"_D")))
# }) %>% 
#   do.call(rbind,.) %>% 
#   setNames(c("Station","OP1","ROI")) %>% 
#   add_column(Date=NA,.before=T) %>% as.tibble


