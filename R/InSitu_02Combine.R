###########################'
source("R/BaseFunctions.R")

write.outputs=F
suffix<- format.Date(Sys.Date(),"%d%m%y")

# Long Aggregation --------------------------------------------------------
lf<-list.files(paste0(InSitu_dir,"/07_FieldCampaign17/02_Station/"),pattern=".csv",full.names=T)
lf_short<-list.files(paste0(InSitu_dir,"/07_FieldCampaign17/02_Station/"),pattern=".csv")

stats<-do.call(rbind,str_split(lf_short,"_"))[,1]
aggr<-do.call(rbind,lapply(lf,read_csv))
aggr$Date<-as.Date(sprintf("%06s",aggr$Date),"%d%m%y")

if(write.outputs==T) write.RDSCSV(aggr,paste0(dir,"/","InSituMetrics1.csv"))

# Tidy Aggregation --------------------------------------------------------
lf<-list.files(paste0(dirfield,"/03_DaySOS"),pattern=".csv",full.names=T)
dir<-paste0(dirfield,"/","04_Combined/")

list1<-suppressMessages(map(lf,read_csv))
list2<-do.call(rbind,list1) %>% select(-X1)
list2$Date<-as.Date(sprintf("%06s",list2$Date),"%d%m%y")

if(write.outputs==T) write.RDSCSV(list2,paste0(dir,"/","InSituMetrics1_tidy.csv"))

# Combination with ROI -----------------------------------------------
list3<-list2 %>% tidyr::separate(.,OP1,c("OP1","Repetition"),sep="_") 

insitu_raw<-list3 %>% 
  filter(OP2=="NDVI" & OP3==1)  %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2) %>%
  dplyr::summarise(.,Value=mean(Value)) %>% ungroup

set<-insitu_raw %>% filter(Station!="P2") %>% arrange(Station)

n1<-"D,A,B,C"
n2<-"A,B,C,D"
phenoletters<-paste0(n1,",",
                     paste(rep(n2,13),collapse=","),",",
                     n1,",",
                     paste(rep(n2,11),collapse=","))
phenoletters<-str_split(phenoletters,",")[[1]]

set2<-set %>% 
  add_column(ROI=paste0(.$Station,"_",phenoletters)) %>% 
  select(Date,Station,OP1,ROI)

lj1<-left_join(list3,set2,by=c("Date","Station","OP1"))
if(write.outputs==T) write.RDSCSV(lj1,paste0(dir,"/","InSituMetrics1_tidy_combined"))

lj2<-left_join(insitu_raw,set2,by=c("Date","Station","OP1"))
if(write.outputs==T) write.RDSCSV(lj2,paste0(dir,"/","InSituMetrics1_tidy_combined_ndvi"))

# LAI tables --------------------------------------------------------------
insitu.raw.all<-lj1 %>% 
  filter(OP3=="LAI"|OP3=="MTA") %>% 
  group_by(Date,Station,OP1,OP2,OP3) %>% 
  dplyr::summarise(Mean=mean(Value)) %>% 
  filter(Station!="P2") %>% 
  ungroup

if(write.outputs==T) write.RDSCSV(insitu.raw.all,paste0(Workspacedir,"07_Products/InSituLAI2"))
