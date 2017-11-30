# 1. Initialization ----
source("R/BaseFunctions.R")


# 2. Tidy ----

head1<-seq(1,4,1) # Header Position
lf<-list.files(dirstat,pattern =".csv")
lf_full<-list.files(dirstat,pattern =".csv",full.names = T)


# 3. Computation ----
lstall<-list()
cblat<-list()
cblon<-list()
allISgps<-list()
for(i in 1:length(lf_full)){
  
  #* 3.1 Input and Start ----
  
  itab_raw <-read_csv(lf_full[i],col_names = F,col_types = cols())
  itab<-.remheader(itab_raw)
  
  # Station and Data
  nms1 <- itab %>% select(contains("ID")) %>% names
  
  # Matrix For the Relevant Informations
  cnames<-c("FOI","Date","SubID","Sample")
  hnames<-c("PRI1","PRI2","NDVI1","NDVI2","NDVI3")
  lnames<-c("LAI","SEL","ACF","DIFN","MTA","SEM","SMP")
  bnames<-c("BioWet","BioDryRaw","BioWatRawPerc","MeanErr")
  wnames<-c("SW_perc")
  
  #GPS Extraction
  gpsIS<-itab %>% filter(Type=="Location") %>% select(starts_with("ID")) %>% 
    t %>% as.data.frame %>% setNames(c("Lat","Lon")) %>% rownames_to_column(var="SubID") %>% 
    add_column(Sensor="InSitu",.before=T) %>% add_column(Date=dat,.before=T) %>% add_column(Stat=stat,.before=T)
  
  #* 3.2 Hyperspectral ----
  
  init<-Insitu.init(table=itab_raw,tableColumn = nms1,
                   pattern="Hyperspectral",
                   names=c(cnames,hnames))
  
  field_mat1<-init[[1]]
  hyp_tab<-init[[2]]
  norm_date<-field_mat1$Date %>% unique

  hyp_dat<-paste(strsplit(norm_date, "", fixed=T)[[1]]
                 [c(3,4,1,2,5,6)],collapse='')
  
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
      ndvi3<-hypindices(s1,c(635,695),c(727,957),stat="NDVI")
      
      wh_hyp<-which(field_mat1$Sample==k&field_mat1$SubID==n)
      field_mat1$PRI1 [wh_hyp]<-pri1
      field_mat1$PRI2 [wh_hyp]<-pri2
      field_mat1$NDVI1[wh_hyp]<-ndvi1
      field_mat1$NDVI2[wh_hyp]<-ndvi2
      field_mat1$NDVI3[wh_hyp]<-ndvi3
      
    }
    
    gpsH[[j]]<-rowMeans(rbind(gpsHLat,gpsHLon),na.rm=T) %>% round(5) %>% c(n,.)
  }
  
  if(length(gpsH)>0) gpsH<-gpsH %>% do.call(rbind,.) %>% data.frame %>% 
    setNames(c("SubID","Lat","Lon")) %>% 
    add_column(Sensor="Hyper",.before=T) %>% add_column(Date=dat,.before=T) %>% add_column(Stat=stat,.before=T)

  #* 3.3 LAI ----
  
  init<-Insitu.init(table=itab_raw,tableColumn = nms1,
                   pattern="Leaf Area",
                   names=c(cnames,lnames))
  
  field_mat2<-init[[1]]
  lai_tab<-init[[2]]
  date1<-field_mat2 %>% select(Date) %>% unique %>% as.character
  
  gpsLAILat<-array(dim=ncol(lai_tab)); gpsLAILon<-array(dim=ncol(lai_tab));gpsLAI<-list()
  # Loop through the files and insert the data in the matrix @ given place
  for(j in 1:ncol(lai_tab)){
    
    laID   <- names(lai_tab)[j]
    lai_nm <- lai_tab %>% select(.,matches(laID)) %>% as.matrix %>% paste0(date1,"_",.,".csv")
    lai_lf <- dirlai %>% list.files(.) %>% .[which(is.element(.,lai_nm))]

    if(length(lai_lf)==0){next}
    
    for (k in 1:length(lai_lf)){
      
      # Read the File & Extract Stats
      rd   <-read.csv(paste0(dirlai,"/",lai_lf[k]))
      la1  <-rd$V2[which(is.element(rd$V1,lnames))]
      
      # Extract GPS
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
  
  
  #* 3.4 Biomass ----
  
  init<-Insitu.init(table=itab_raw,tableColumn = nms1,
                   pattern="Biomass",
                   names=c(cnames,bnames))
  
  field_mat3<-init[[1]]
  field_mat3<-field_mat3 %>% filter(.,Sample==1)
  bio_tab<-init[[2]]
  
  for(j in nms1){
    
    bio_tab2<-bio_tab[,j] %>% as.matrix %>% as.numeric
    if(is.na(bio_tab2[5])){bio_tab2[5]<-bio_tab2[4]} # Replace missing W2
    bwet<-  bio_tab2[4]-bio_tab2[1] # Wet Weight
    bdry<-  bio_tab2[7]-bio_tab2[3] # Dry Weight
    bwat<-  round((1-(bdry/bwet))*100,2) # Water Content Raw
    bunc<-  abs(bio_tab2[4]-bio_tab2[5]) + # Difference between 
      abs(bio_tab2[2]-bio_tab2[1]) + # Rest in Plastic Bag
      abs((bio_tab2[5]-bio_tab2[2])- # Rest not within Plastic Bag
            (bio_tab2[6]-bio_tab2[3]))
    
    wh1<-min(which(field_mat3[,3]==j))
    field_mat3[wh1,5]<-round(bwet,2)
    field_mat3[wh1,6]<-round(bdry,2)
    field_mat3[wh1,7]<-round(bwat,2)
    field_mat3[wh1,8]<-round(bunc,2)
  }


  #* 3.5 Soil Water Content ----
  
  init<-Insitu.init(table=itab_raw,tableColumn = nms1,
                          pattern="Water",
                          names=c(cnames,wnames))
  
  field_mat4<-init[[1]]
  field_mat4<-field_mat4 %>% filter(.,Sample<5)
  wat_tab<-init[[2]]
  
  rd<-wat_tab[1:4,]
  field_mat4$SW_perc<-rd %>% unlist
  
  
  #* 3.6 Merge the Datasets ----

  lj<-field_mat1 %>% full_join(.,field_mat2,by = c("FOI", "Date", "SubID", "Sample")) %>% 
    full_join(.,field_mat3,by = c("FOI", "Date", "SubID", "Sample")) %>%
    full_join(.,field_mat4,by = c("FOI", "Date", "SubID", "Sample"))
  
  lj<- lj %>% arrange(Sample) %>% arrange(SubID)
  
  stat<-lj$FOI %>% unique
  dat <-lj$Date %>% unique
  write.csv(lj,paste0(InSitu_dir,"07_FieldCampaign17/01_Day/",stat,"_",dat,"_combined.csv"))

  if(i==1) stat1<-stat
  if(stat1==stat){lstall[[length(lstall)+1]]<-lj}else{
    
    brow<-bind_rows(lstall)
    write.csv(brow,paste0(InSitu_dir,"07_FieldCampaign17/02_Station/",stat1,"_combined_ALL.csv"))
    lstall<-list()
    lstall[[length(lstall)+1]]<-lj
    
  }
  if(i==length(lf_full)){
    
    brow<-bind_rows(lstall)
    write.csv(brow,paste0(InSitu_dir,"07_FieldCampaign17/02_Station/",stat1,"_combined_ALL.csv"))
  } 
  
  stat1<-stat
  
  #* 3.7 GPS Operations ----
  
  allISgps[[i]]<-list(gpsIS, gpsLAI,gpsH) %>% .[lapply(.,length)>0] %>% do.call(rbind,.)

}

# Merge the GPS
gps<-do.call(rbind,allISgps)
save(gps,file=paste0(InSitu_dir,"07_FieldCampaign17/02_Station/GPS.RData"))

