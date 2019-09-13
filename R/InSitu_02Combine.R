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


# Biomass Tables
insitu.raw.bio<-lj1 %>% 
  filter(Scale2=="Biomass") %>% 
  filter(OP2=="Location"|OP2=="Laboratory") %>% 
  select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value)

if(write.outputs==T) write.RDSCSV(insitu.raw.bio,paste0(Workspacedir,"07_Products/InSituBiomass2017"))




# Paper 2 -----------------------------------------------------------------
# Load Datasets -----------------------------------------------------------
tablespec  <- list.files(paste0(dirhyp,"02_Combined/"),full.names = T,pattern = "rds")
tablespec2 <- bind_rows(lapply(tablespec,readRDS))
tablespec3 <- dplyr::select(tablespec2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value)

tablelai   <- list.files(paste0(dirlai,"02_Combined/"),full.names = T,pattern = "rds")
tablelai2  <- bind_rows(lapply(tablelai,readRDS))
tablelai3  <- dplyr::select(tablelai2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
  filter(Prop2=="Metrics") %>% 
  mutate(Value=as.numeric(Value))

tablebio   <- list.files(paste0(dirbio,"02_Combined/"),full.names = T,pattern = "rds")
tablebio2  <- bind_rows(lapply(tablebio,readRDS))
tablebio3  <- dplyr::select(tablebio2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
  mutate(Value=as.numeric(Value))

tableall   <- bind_rows(tablespec3,tablelai3,tablebio3)


# Plot Bio and LAI --------------------------------------------------------


Harvests<-rbind(c("Vimes1500","2017",yday(as_date("2017-07-03"))),
                c("Vimes1500","2018",yday(as_date("2018-06-23")))) %>% 
  as_tibble %>% 
  setNames(c("Station","Harvest","DOY")) %>% 
  mutate(DOY=as.numeric(DOY))

intervals<-c("2017-01-01" %--% "2017-07-03",
             "2018-01-01" %--% "2018-06-23")


plotdat<-tableall %>%
  filter(Station=="Vimes1500" | Station=="P2") %>% 
  filter((Scale2=="Biomass" & Prop3!="Error") | (Scale2=="Leaf Area" & Prop3=="LAI")) %>% 
  mutate(Tit=paste(Scale2,"-",Prop3))

plotdat2<-plotdat %>% 
  group_by(Station,Date,Scale1,Scale2,Prop3,Tit) %>% 
  dplyr::summarize(Median=median(Value,na.rm = T)) %>% 
  ungroup %>% 
  mutate(GrowingPeriod= ifelse(Date %within% intervals[1],"2017-Grow1",
                               ifelse(Date %within% intervals[2],"2018-Grow1",0))) %>% 
  mutate(DOY=yday(Date))


plotdat2.grow1<-plotdat2 %>% filter(GrowingPeriod!=0)


pdu<-plotdat2.grow1$Tit %>% unique
Harvests<-tidyr::crossing(Harvests,Tit=pdu)

g1<-ggplot(plotdat2.grow1,aes(DOY,Median,col=GrowingPeriod,pch=GrowingPeriod))+ theme_bw() +
  scale_color_manual(values=c("grey70","black"))+
  facet_wrap(Station~Tit,scales="free_y",ncol=4)+
  geom_point()+
  geom_line()+
  geom_vline(data=Harvests,aes(xintercept=DOY,lty=Harvest))+
  labs(title="Biomass sampling during the first growing period in 2017 and 2018")+ 
  theme(legend.position="bottom")

ggsave(g1,filename = "Paper2/BomassLAIplot1.png",width=12,height=7)
