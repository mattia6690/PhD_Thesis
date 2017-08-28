###########################
#' 00. In Situ Reader Script
#'
#' This script allows to read the information contained in my field  protocols
#' It combines multiple procedures containing for extracting:
#' * Hyperspectral measurements
#' * LAI measurements
#' * Biomass estimation
#' * Soil Water Approximations
#' * Exact GPS locations
#' 
###########################
source("C:/Users/MRossi/Documents/07_Codes/PhD_Thesis/R/00_BaseFunctions.R")

### 01. Read ###
# State the in- and output directories
dirlai<-paste0(getwd(),"/04_LAI")
dirhyp<-paste0(getwd(),"/05_HyperSpec")
dirstat<-paste0(getwd(),"/07_FieldCampaign17/00_Raw")

# Read the csv tables with the Information per station
head1<-seq(1,4,1) # Header Position
lf<-list.files(dirstat,pattern =".csv")
lf_full<-list.files(dirstat,pattern =".csv",full.names = T)

### 02. Plots ###
lstall<-list()
cblat<-list();cblon<-list();allISgps<-list()
for(i in 1:length(lf_full)){
  
  itab <-read_csv(lf_full[i],col_names = F)
  itab_head <- slice(itab,head1)
  itab <-itab %>% slice(5:nrow(itab)) %>% setNames(.,.[1,]) %>% slice(2:nrow(itab))
  
  # Station and Data
  stat <- itab_head$X2[1]
  dat  <- itab_head$X2[2] %>% as.numeric %>% sprintf("%06d", .)
  nms1 <- select(itab,contains("ID")) %>% names
  
  # Matrix For the Relevant Informations
  cnames<-c("FOI","Date","SubID","Sample")
  hnames<-c("PRI1","PRI2","NDVI1","NDVI2","NDVI3","NDVI4")
  lnames<-c("LAI","SEL","ACF","DIFN","MTA","SEM","SMP")
  bnames<-c("BioWet","BioDryRaw","BioWatRawPerc","MeanErr")
  wnames<-c("SW_perc")

  #GPS Extraction
  gpsIS<-itab %>% filter(Type=="Location") %>% select(starts_with("ID")) %>% 
    t %>% as.data.frame %>% setNames(c("Lat","Lon")) %>% rownames_to_column(var="SubID") %>% 
    add_column(Sensor="InSitu",.before=T) %>% add_column(Date=dat,.before=T) %>% add_column(Stat=stat,.before=T)
  
  
  ###################
  # 01. Hyperspectral
  ###################
  
  # Create Filename
  hyp_tab<-itab[itab$Type=="Hyperspectral",]
  hyp_dat<-paste(strsplit(dat, "", fixed=T)[[1]][c(3,4,1,2,5,6)], collapse='')
  
  # Create the Matrix
  field_mat1<-as.data.frame(matrix(ncol=length(cnames)+length(hnames),
                                   nrow=(length(nms1)*nrow(hyp_tab))))
  colnames(field_mat1)<-c(cnames,hnames)
  
  field_mat1$FOI<-stat
  field_mat1$Date<-dat
  field_mat1$SubID<-rep(nms1,each=nrow(hyp_tab))
  field_mat1$Sample<-rep(seq(1,nrow(hyp_tab),1),times=length(nms1))
  
  # Loop through the files and insert the data in the matrix @ given place
  gpsH<-list(); gpsHLon<-array(); gpsHLat<-array()
  for(j in 1:length(nms1)){
    
    n<-nms1[j]
    id<-as.numeric(as.matrix(hyp_tab[[n]]))
    if(all(is.na(id))==T){next}
    hyp_nm <-paste("HRPDA",hyp_dat,sprintf("%04d",id),"sig",sep=".")
    
    # REad the Files
    hyp_lf <-list.files(dirhyp)
    hyp_lf2<-list.files(dirhyp,full.names = T)
    hyp_lf2<-hyp_lf2[which(is.element(hyp_lf,hyp_nm))]
    if(length(hyp_lf2)==0){next}
    
    # Rearrange Data
    for(k in 1:length(hyp_lf2)){
      
      rd<-read_table(hyp_lf2[k],col_types = cols("/*** Spectra Vista SIG Data ***/" = col_character()))
      colnames(rd)<-"data"
      
      #GPS Extraction
      gpsHLon[k]<-toString(rd$data[which(grepl("longitude",rd$data))]) %>%
        str_replace("longitude=","") %>% str_replace_all("E","") %>%
        str_split(",") %>% unlist %>% as.numeric %>% mean(na.rm=T) %>% "/" (100) 
      gpsHLat[k]<-toString(rd$data[which(grepl("latitude",rd$data))]) %>% 
        str_replace("latitude=","") %>% str_replace_all("N","") %>%  
        str_split(",") %>% unlist %>% as.numeric %>% mean(na.rm=T) %>% "/" (100) 

      rd<-rd[which(rd[1]=="data=")+1:nrow(rd),]
      s1<-na.omit(separate(rd,col = data, into=c("Wavel","Rup","Rdn","Refl"),sep="  ",convert=))
      class(s1$Wavel)<-"numeric"
      class(s1$Refl)<-"numeric"
      
      # PRI and NDVI Production
      pri1 <-hypindices(s1,c(530,534),c(568,572),stat="PRI")
      pri2 <-hypindices(s1,c(520,544),c(558,582),stat="PRI")
      ndvi1<-hypindices(s1,c(648,652),c(808,812),stat="NDVI")
      ndvi2<-hypindices(s1,c(638,662),c(798,822),stat="NDVI")
      ndvi4<-hypindices(s1,c(635,695),c(727,957),stat="NDVI")
      
      wh_hyp<-which(field_mat1$Sample==k&field_mat1$SubID==n)
      field_mat1$PRI1 [wh_hyp]<-pri1
      field_mat1$PRI2 [wh_hyp]<-pri2
      field_mat1$NDVI1[wh_hyp]<-ndvi1
      field_mat1$NDVI2[wh_hyp]<-ndvi2
      #field_mat1$NDVI3[wh_hyp]<-ndvi3
      field_mat1$NDVI4[wh_hyp]<-ndvi4
      
    }
    
    gpsH[[j]]<-rowMeans(rbind(gpsHLat,gpsHLon),na.rm=T) %>% round(5) %>% c(n,.)
  }
  
  if(length(gpsH)>0) gpsH<-gpsH %>% do.call(rbind,.) %>% data.frame %>% 
    setNames(c("SubID","Lat","Lon")) %>% 
    add_column(Sensor="Hyper",.before=T) %>% add_column(Date=dat,.before=T) %>% add_column(Stat=stat,.before=T)

  ############
  # 02.LAI
  ############
  
  # Create Filename
  lai_tab <-itab[itab$Type=="Leaf Area",]
  lai_tab <-lai_tab[,which(grepl("ID",colnames(lai_tab)))]
  
  # Create the Matrix
  field_mat2<-as.data.frame(matrix(ncol=(length(cnames)+length(lnames)),
                                   nrow=(length(nms1)*nrow(lai_tab))))
  colnames(field_mat2)<-c(cnames,lnames)
  
  field_mat2$FOI<-stat
  field_mat2$Date<-dat
  field_mat2$SubID<-rep(nms1,each=nrow(lai_tab))
  field_mat2$Sample<-rep(seq(1,nrow(lai_tab),1),times=length(nms1))
  
  gpsLAILat<-array(dim=ncol(lai_tab)); gpsLAILon<-array(dim=ncol(lai_tab));gpsLAI<-list()
  # Loop through the files and insert the data in the matrix @ given place
  for(j in 1:ncol(lai_tab)){
    
    laID   <- names(lai_tab)[j]
    lai_nm <- lai_tab %>% select(.,matches(laID)) %>% as.matrix %>% paste0(dat,"_",.,".csv")
    lai_lf <- dirlai %>% list.files %>% .[which(is.element(.,lai_nm))]

    if(length(lai_lf)==0){next}
    
    for (k in 1:length(lai_lf)){
      
      # Read the File & Extract Stats
      rd   <-read.csv(paste0(dirlai,"/",lai_lf[k]))
      la1  <-rd$V2[which(is.element(rd$V1,lnames))]
      
      # Extrat GPS
      gpsLAILat[k]<-rd %>% filter(V1=="GPSLAT") %>% select(V2)  %>% as.matrix %>% as.numeric %>% {if(length(.)==0) NA else .}
      gpsLAILon[k]<-rd %>% filter(V1=="GPSLONG") %>% select(V2) %>% as.matrix %>% as.numeric %>% {if(length(.)==0) NA else .}

      # Write in the Matrix
      wh_fm<-which(is.element(names(field_mat2),lnames))
      wh_lai<-which(field_mat2$Sample==k&field_mat2$SubID==laID)
      field_mat2[wh_lai,wh_fm]<-la1
    }
    
    gpsLAI[[j]]<-rowMeans(rbind(gpsLAILat,gpsLAILon),na.rm=T) %>% round(5) %>% c(laID,.) 
  }
  
  if(length(gpsLAI)>0) {
    gpsLAI<- gpsLAI %>% do.call(rbind,.) %>% data.frame %>%  
    setNames(c("SubID","Lat","Lon")) %>% 
    add_column(Sensor="LAI",.before=T) %>% add_column(Date=dat,.before=T) %>% add_column(Stat=stat,.before=T)
  }
  
  ###############
  # 03.Biomass
  # ###############
bio_tab<-itab[itab$Type=="Biomass",]
bio_tab <-bio_tab[,which(grepl("ID",colnames(bio_tab)))]

# Create the Matrix
field_mat3<-as.data.frame(matrix(ncol=(length(cnames)+length(bnames)),
                                 nrow=(length(nms1)*3)))
colnames(field_mat3)<-c(cnames,bnames)

field_mat3$FOI<-stat
field_mat3$Date<-dat
field_mat3$SubID<-rep(nms1,each=3)
field_mat3$Sample<-rep(seq(1,3,1),times=length(nms1))

for(j in 1:ncol(bio_tab)){
  
  bio_tab2<-bio_tab[,j] %>% as.matrix %>% as.numeric
  if(is.na(bio_tab2[5])){bio_tab2[5]<-bio_tab2[4]} # Replace missing W2
  bwet<-  bio_tab2[4]-bio_tab2[1] # Wet Weight
  bdry<-  bio_tab2[7]-bio_tab2[3] # Dry Weight
  bwat<-  round((1-(bdry/bwet))*100,2) # Water Content Raw
  bunc<-  abs(bio_tab2[4]-bio_tab2[5]) + # Difference between 
          abs(bio_tab2[2]-bio_tab2[1]) + # Rest in Plastic Bag
          abs((bio_tab2[5]-bio_tab2[2])- # Rest not within Plastic Bag
              (bio_tab2[6]-bio_tab2[3]))
  
  wh1<-min(which(field_mat3[,3]==names(bio_tab)[j]))
  field_mat3[wh1,5]<-round(bwet,2)
  field_mat3[wh1,6]<-round(bdry,2)
  field_mat3[wh1,7]<-round(bwat,2)
  field_mat3[wh1,8]<-round(bunc,2)
  }

  ###############
  # 04.Water
  ###############
  wat_tab<-itab[itab$Type=="Water",]
  wat_tab <-wat_tab[,which(grepl("ID",colnames(wat_tab)))]
  
  # Create the Matrix
  field_mat4<-as.data.frame(matrix(ncol=(length(cnames)+length(wnames)),
                                   nrow=(length(nms1)*3)))
  colnames(field_mat4)<-c(cnames,wnames)
  
  field_mat4$FOI<-stat
  field_mat4$Date<-dat
  field_mat4$SubID<-rep(nms1,each=3)
  field_mat4$Sample<-rep(seq(1,3,1),times=length(nms1))
  
  rd<-wat_tab[1:3,]
  
  field_mat4$SW_perc<-unlist(rd)
  
  ###################################
  # 04.Merge the datasets and Export
  ###################################

  lj<-field_mat1 %>% right_join(.,field_mat2,by = c("FOI", "Date", "SubID", "Sample")) %>% 
    right_join(.,field_mat3,by = c("FOI", "Date", "SubID", "Sample")) %>%
    right_join(.,field_mat4,by = c("FOI", "Date", "SubID", "Sample"))
  
  write.csv(lj,paste0("07_FieldCampaign17/01_Day/",stat,"_",dat,"_combined.csv"))

  if(i==1) stat1<-stat
  if(stat1==stat){lstall[[length(lstall)+1]]<-lj}else{
    
    brow<-bind_rows(lstall)
    write.csv(brow,paste0("07_FieldCampaign17/02_Station/",stat1,"_combined_ALL.csv"))
    lstall<-list()
    lstall[[length(lstall)+1]]<-lj
    
  }
  if(i==length(lf_full)){
    
    brow<-bind_rows(lstall)
    write.csv(brow,paste0("07_FieldCampaign17/02_Station/",stat1,"_combined_ALL.csv"))
  } 
  
  stat1<-stat
  # List the GPS
  allISgps[[i]]<-list(gpsIS, gpsLAI,gpsH) %>% .[lapply(.,length)>0] %>% do.call(rbind,.)

}

# Merge the GPS
gps<-do.call(rbind,allISgps)
save(gps,file="07_FieldCampaign17/02_Station/GPS.RData")

