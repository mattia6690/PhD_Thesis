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
tablespec3 <- dplyr::select(tablespec2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
  mutate(Year=lubridate::year(Date))
  

tablelai   <- list.files(paste0(dirlai,"02_Combined/"),full.names = T,pattern = "rds")
tablelai2  <- bind_rows(lapply(tablelai,readRDS))
tablelai3  <- dplyr::select(tablelai2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
  filter(Prop2=="Metrics") %>% 
  mutate(Value=as.numeric(Value)) %>% 
  mutate(Year=lubridate::year(Date))

tablebio   <- list.files(paste0(dirbio,"02_Combined/"),full.names = T,pattern = "rds")
tablebio2  <- bind_rows(lapply(tablebio,readRDS))
tablebio3  <- dplyr::select(tablebio2,Date,Station,Scale1,Scale2,Prop1,Prop2,Prop3,Value) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  mutate(Year=lubridate::year(Date))

tableall   <- bind_rows(tablespec3,tablelai3,tablebio3)


# Plot Bio and LAI --------------------------------------------------------

Harvests17<- tibble::enframe(c("2017-07-03","2017-09-04","2017-09-18"),value="Date",name=NULL) 
Harvests18<- tibble::enframe(c("2018-06-23","2018-09-08"),value="Date",name=NULL) 
Harvests<-bind_rows(Harvests17,Harvests18) %>% 
  mutate(Date=as_date(Date)) %>% 
  mutate(Station="Vimes1500") %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=as.character(year(Date)))
  


# Harvests<-rbind(c("Vimes1500","2017",yday(as_date("2017-07-03"))),
#                 c("Vimes1500","2018",yday(as_date("2018-06-23")))) %>% 
#   as_tibble %>% 
#   setNames(c("Station","Harvest","DOY")) %>% 
#   mutate(DOY=as.numeric(DOY))

intervals<-c(as_date("2017-01-01",tz="MEST") %--% Harvests$Date[1],
             Harvests$Date[1] %--% Harvests$Date[2],
             Harvests$Date[2] %--% Harvests$Date[3],
             Harvests$Date[2] %--% as_date("2017-12-31"),
             as_date("2018-01-01") %--% Harvests$Date[4],
             Harvests$Date[4] %--% Harvests$Date[5],
             Harvests$Date[2] %--% as_date("2018-12-31"))

intervals2<-bind_cols(Inter=intervals,
                      Periods=c(1,2,3,4,1,2,3),
                      Year=c(2017,2017,2017,2017,2018,2018,2018)) %>% 
  mutate(GrowingPeriod=paste0(Year,"-Grow",Periods))


plotdat<-tableall %>%
  filter(Station=="Vimes1500" | Station=="P2") %>% 
  filter((Scale2=="Biomass" & Prop3!="Error") | (Scale2=="Leaf Area" & Prop3=="LAI")) %>% 
  mutate(Tit=paste(Scale2,"-",Prop3))

# plotdat2<-plotdat %>% 
#   group_by(Station,Date,Scale1,Scale2,Prop3,Tit) %>% 
#   dplyr::summarize(Mean=mean(Value,na.rm = T)) %>% 
#   ungroup %>% 
#   mutate(GrowingPeriod= ifelse(Date %within% intervals[1],"2017-Grow1",
#                                ifelse(Date %within% intervals[2],"2018-Grow1",0))) %>% 
#   mutate(DOY=yday(Date))

plotdat.growphase<- plotdat %>%
  group_by(Date,Station,Scale1,Scale2,Prop3,Tit) %>%
  dplyr::summarize(Mean=mean(Value,na.rm=T),
                   Min=min(Value),
                   Max=max(Value)) %>% 
  mutate(GrowingPeriod=map_chr(Date,function(x,i=intervals2){
    
    test<-min(which(x %within% i$Inter))
    ret<-i$GrowingPeriod[test]
    return(ret)
    
  }))


plotdat2.grow1<-plotdat.growphase %>% 
  mutate(Indicator=map2_chr(Scale2,Prop3,function(x,y){
    
    if(y=="Wet") r<-paste(x,"1-",y,"(g)")
    if(y=="Dry") r<-paste(x,"2-",y,"(g)")
    if(y=="Water Percentage") r<-paste(x,"3-","Water Content","(%)")
    if(y=="LAI") r<-paste(x,"-",y)
    return(r)
    
  })) %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=as.character(year(Date))) %>% 
  ungroup


ggsave(g1,filename = "Paper2/BiomassLAIplot2.png",width=10,height=5)
saveRDS(plotdat2.grow1,"rds/BiomassLAI3.rds")



# Correlation LAI, Biomass


plotdat2.grow2<- plotdat2.grow1 %>% 
  mutate(Short=map2_chr(Scale2,Prop3,function(x,y){
    
    if(y=="Water Percentage") y="Water"
    if(y=="LAI") return(x)
    ret<-paste(x,y)
    return(ret)
    
    
  }))

cplotdata<-plotdat2.grow2 %>% 
  dplyr::select(Station,GrowingPeriod,Date,Short,Mean) %>% 
  spread(Short,Mean) %>% 
  select(-Date) %>% 
  group_by(Station,GrowingPeriod) %>% 
  nest %>% 
  mutate(cor=map(data,function(x){
    
    c<-round(cor(x,use="pairwise.complete.obs"),2)
    utree<-get_upper_tri(c)
    utree.melt<-melt(get_upper_tri(c),na.rm=T)
    return(utree.melt)
    
  })) %>% 
  unnest(cor)
cplotdata2<-cplotdata[-which(cplotdata$Var1==cplotdata$Var2),] 

corplot<-ggplot(data = cplotdata2, aes(x=Var2, y=Var1, fill=value)) + 
  facet_grid(vars(Station),vars(GrowingPeriod))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value))+
  scale_fill_gradient2(low = "blue", high = "green", mid = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 25,hjust = 1))+
  coord_fixed()+
  ylab("")+xlab("")+
  theme(legend.position="bottom")

ggsave(corplot,filename="Paper2/Correlation_heatmap_all.png",width = 5,height = 5.5)




# Chack Data --------------------------------------------------------------

#LAI
tlai1<- tablelai3 %>% filter(Prop3=="LAI") %>% filter(Station=="P2"|Station=="Vimes1500")

nlai <- tlai1 %>% group_by(Station,Year) %>% dplyr::summarize(SamplesLAI=n())
dlai <- tlai1 %>% group_by(Date,Station,Year) %>% dplyr::summarize(n=n()) 
ndaylai<- dlai %>% group_by(Station,Year) %>% dplyr::summarize(DaysLAI=n())
startstoplai<-dlai %>% group_by(Station,Year) %>% dplyr::summarize(Start=min(Date),End=max(Date))

jnlai<-left_join(nlai,ndaylai,by = c("Station", "Year")) %>% 
  left_join(.,startstoplai,by = c("Station", "Year")) %>% 
  ungroup 

# Biomass
tbio1 <- tablebio3 %>% filter(Prop3=="Wet")%>% filter(Station=="P2"|Station=="Vimes1500")

nbio <- tbio1 %>% group_by(Station,Year) %>% dplyr::summarize(SamplesBIO=n())
dbio <- tbio1 %>% group_by(Date,Station,Year) %>% dplyr::summarize(n=n()) 
ndaybio<- dbio %>% group_by(Station,Year) %>% dplyr::summarize(DaysBIO=n())
startstopbio<-dbio %>% group_by(Station,Year) %>% dplyr::summarize(Start=min(Date),End=max(Date))

jnbio<-left_join(nbio,ndaybio,by = c("Station", "Year")) %>% 
  left_join(.,startstopbio,by = c("Station", "Year")) %>% 
  ungroup
#Join

tab<-left_join(jnlai,jnbio)

saveRDS(tab,file = "tables/fieldcampaigntable.rds")
