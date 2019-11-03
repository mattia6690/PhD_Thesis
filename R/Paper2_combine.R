
source("R/BaseFunctions.R")

library("tsibble")
library("imputeTS")

# Data Loading ------------------------------------------------------------

# Monalisa
srs       <- readRDS(paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_daily_311019.rds"))
# Phenocam
phenocam  <- readRDS(paste0(PhenocamDir,"Paper2_VIs.rds"))
# Sentinel
s2<-readRDS(file = paste0(WorkspaceDir,"07_Products/Metrics_S2_Station_Selection_011119.rds"))

# Soil Water content
swc       <-readRDS("Paper2/SWC_daily.rds")
# SPI, SPEI
droughtI  <- readRDS("rds/DroughtIndicesVI.rds")
# Biomass and LAI
biolai    <-readRDS("rds/BiomassLAI.rds")

# Optical Data ------------------------------------------------------------
#* Phenocam ----------------------------------------------------------------
phenocam.raw<- phenocam %>% 
  mutate(Date2=as_date(Date)) %>% 
  group_by(Station,Date,Date2) %>% 
  dplyr::summarize(ndvi=mean(ndvi),gcc=mean(gcc),exg=mean(exg)) %>% 
  group_by(Station,Date2) %>%
  dplyr::summarise(NDVI=median(ndvi),GCC=median(gcc),EXG=median(exg)) %>% 
  gather(key=Index,value=Value,NDVI,GCC,EXG)


phenocam1 <- phenocam.raw  %>% 
  filter(Station=="vimes1500" | Station=="LTER_P2")  %>% 
  add_column(Scale1="Proximal") %>% 
  add_column(Scale2="Phenocam") %>%
  add_column(OP3="90Percentile") %>% 
  add_column(OP1="Site")  %>% 
  rename(OP2="Index") %>% 
  dplyr::select(Date=Date2,Station,Scale1,Scale2,OP1,OP2,OP3,Value)

phenocam.final<-phenocam1 %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2) %>% 
  dplyr::summarize(Value=median(Value)) %>% 
  ungroup %>% 
  mutate(OP3="Median") %>%
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value) %>% 
  arrange(Date)


#* SRS ----------------------------------------------------------------
srs.final<-srs %>% 
  filter(Station=="vimes1500"| Station=="P2") %>% 
  rename(Date=Date2) %>% 
  mutate(Scale2="SRS") %>% 
  mutate(Scale1="Decagon") %>% 
  filter(OP2!="PRI")

#* Sentinel ----------------------------------------------------------------
s2.final<-s2 %>% 
  rename(OP2=Product) %>% 
  rename(Value=Mean) %>% 
  add_column(Scale1="Sentinel-2") %>% 
  add_column(Scale2="MSI") %>%
  add_column(OP1="Site") %>% 
  add_column(OP3="Mean") %>% 
  mutate(Value=Value/10000) %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value) 

#* Combine -----------------------------------------------------------------
binder<-bind_rows(phenocam.final,srs.final,s2.final) %>% 
  mutate(Station=map_chr(Station,function(x){
    
    y<-ifelse(x=="LTER_P2","P2",x)
    y<-ifelse(x=="vimes1500","Vimes1500",y)
    return(y)
    
  }))

#* Interpolate -------------------------------------------------------------

gpdb.fill<-binder %>% 
  group_by(Station,Scale1,Scale2,OP1,OP2) %>% 
  nest %>% 
  arrange(Station)

gpdb.ts <- gpdb.fill %>% 
  mutate(ts.approx=map(data,function(i){
    
    df1 <-as.data.frame(i) %>% 
      dplyr::select(Date,Value) %>% 
      group_by(Date) %>% 
      dplyr::summarize(Value=mean(Value)) %>% 
      ungroup
    
    tsib<-as_tsibble(df1,index="Date") %>% 
      fill_gaps() %>% 
      mutate(Approach=map_chr(Value,function(x) {ifelse(is.na(x),"Interpolated","Original")})) %>% 
      mutate(Stineman=na_interpolation(Value,"stine"))
    
    
    return(as_tibble(tsib))
    
  }))

s<-seq(as_date("2017-01-01"),as_date("2018-01-01"),by="month")-1
sdiff<- as.numeric(c(0,diff(s))[1:12])

gapfill.db<-gpdb.ts %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2) %>% 
  dplyr::summarize(Original=mean(Value),
                   Stineman=mean(Stineman)) %>% 
  mutate(Scale3=paste(Scale1,Scale2))

testFF<-gapfill.db %>% 
  tidyr::gather(key = "Interpolation",value="InterpolationValue",Stineman) %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  filter(DOY>90 & DOY<250)

Harvests<-rbind(c("Vimes1500","2017",yday(as_date("2017-07-03"))),
                c("Vimes1500","2018",yday(as_date("2018-06-23")))) %>% 
  as_tibble %>% 
  setNames(c("Station","Harvest","DOY")) %>% 
  mutate(DOY=as.numeric(DOY))

h17<-filter(Harvests,Harvest==2017)
h18<-filter(Harvests,Harvest==2018)


g1<-ggplot(testFF,aes(DOY,InterpolationValue))+
  theme_bw()+
  facet_grid(vars(Scale3,OP2),vars(Year),scales="free")+
  geom_line(aes(col=Station))+
  geom_point(aes(y=Original,col=Station,pch=Station),alpha=.5)+
  scale_color_manual(values=c("grey70","black"))+
  geom_vline(data=h17,aes(xintercept=DOY),col="grey70",lty=2)+
  geom_vline(data=h18,aes(xintercept=DOY),col="black",lty=2)+
  scale_x_continuous(breaks=cumsum(sdiff),labels = month.abb)+
  ylab("Index Values")+
  xlab("Month of the year")


ggsave(g1,filename = "Paper2/OpticalSensors.png",height=14,width=10)



binder.gapfill<-testFF %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  rename(OP3=Interpolation) %>% 
  rename(Value=InterpolationValue)
  dplyr::select(Date,Month,Year,Station,Scale1,Scale2,OP1,OP2,OP3,Value) %>% 
  ungroup
  
saveRDS(binder.gapfill,"Paper2/OpticalSensorGapfillCombine.rds")


# Biophysical Data ------------------------------------------------------------
#* Drought Indices ---------------------------------------------------------

spiSeverity<-read.table(file = "tables/spitab.txt",header = T,sep = " ",stringsAsFactors = F) %>% 
  as_tibble %>% 
  mutate(Index=as.factor(c(1:nrow(.))))

spi4<-

droughtall<-droughtI %>% 
  filter(Date>"2016-12-31") %>% 
  rename(Station=District) %>% 
  mutate(OP1=map_chr(Name2,function(x)str_split(x," ")[[1]][1])) %>% 
  rename(OP2=Indices) %>% 
  mutate(OP3="Absolute") %>% 
  mutate(Scale1="InSitu") %>% 
  mutate(Scale2="Drought") %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value)

#* SWC ---------------------------------------------------------------------
swc.tab<- swc %>% 
  ungroup %>% 
  add_column(Scale1="InSitu") %>% 
  add_column(Scale2="SWC") %>% 
  add_column(OP3="Mean") %>% 
  add_column(OP2="SWC") %>% 
  rename(OP1=Depth) %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value=Mean)

#* Biomass -----------------------------------------------------------------
biolai.tab<- biolai %>% 
  ungroup %>% 
  add_column(OP1="Site") %>% 
  add_column(OP3="Mean") %>% 
  rename(OP2=Prop3) %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value=Median)


# * Combine ---------------------------------------------------------------
drought.binder<-bind_rows(droughtall,swc.tab,biolai.tab) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  dplyr::select(Date,Month,Year,Station,Scale1,Scale2,OP1,OP2,OP3,Value)


saveRDS(binder.gapfill,"Paper2/DroughtIndexCombine.rds")


# STATISTICS --------------------------------------------------------------
# * Drought ---------------------------------------------------------------

spispei<-drought.binder %>% 
  filter(Scale2=="Drought") %>% 
  filter(OP1=="02200MS") %>% 
  select(Month,Year,OP2,Value) %>% 
  rename(Index=OP2) %>% 
  spread(Year,Value) %>% 
  mutate(Difference=`2017`-`2018`) %>% 
  mutate(Min=min(c(`2017`,`2018`)))


jn<-binder.gapfill %>% ungroup %>% 
  left_join(.,spispei,by=c("Month","Year")) 

opticaldrought<-jn %>% 
  mutate(DOY=yday(Date)) %>% 
  select(-Date) %>% 
  mutate(Year2=paste0("Year",Year)) %>% 
  select(-Year) %>%  
  spread(Index,DValue)
  
  spread(Year2,Value)
  mutate(Scale3=paste(Scale1,Scale2)) %>% 
  mutate(Scale4=paste(Scale3,OP2))

lj<-left_join(opticaldrought,spispei,by=c("Month")) %>% 
  mutate(Difference1718=Year2017-Year2018) %>% 
  group_by(Station,Scale4) %>% 
  nest %>% 
  mutate(GGSPI=map(data,function(x){
    
    ggplot(x,aes(DOY,Difference1718,col=Severity))+
      geom_point()+
      facet_grid(vars(Scale3,OP2),scales="free")
    
  }))




