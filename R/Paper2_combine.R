
source("R/BaseFunctions.R")
library("tsibble")
library("imputeTS")
library("forecast")

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
biolai2   <-readRDS("rds/BiomassLAI3.rds") # No group and mean
# Optical Data ------------------------------------------------------------
#* Phenocam ----------------------------------------------------------------

phenocam.raw<- phenocam %>% 
  filter(!(Station=="vimes1500" && Type=="Systematic")) %>% 
  mutate(Date2=as_date(Date)) %>% 
  group_by(Station,Date,Date2) %>% 
  dplyr::summarize(ndvi=mean(ndvi),gcc=mean(gcc),exg=mean(exg)) %>% 
  group_by(Station,Date2) %>%
  dplyr::summarise(NDVI=median(ndvi),GCC=median(gcc),EXG=median(exg)) %>% 
  gather(key=Index,value=Value,NDVI,GCC,EXG) %>% 
  ungroup


phenocam1 <- phenocam.raw  %>% 
  filter(Station=="vimes1500" | Station=="LTER_P2")  %>% 
  add_column(Scale1="Proximal") %>% 
  add_column(Scale2="Phenocam") %>%
  add_column(OP3="90Percentile") %>% 
  add_column(OP1="Site")  %>% 
  dplyr::rename(OP2=Index) %>% 
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
  dplyr::rename(Date=Date2) %>% 
  mutate(Scale2="SRS") %>% 
  mutate(Scale1="Decagon") %>% 
  filter(OP2!="PRI")

#* Sentinel ----------------------------------------------------------------
s2.final<-s2 %>% 
  dplyr::rename(OP2=Product) %>% 
  dplyr::rename(Value=Mean) %>% 
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
sdiff<- cumsum(as.numeric(c(0,diff(s))[1:12]))

gapfill.db<-gpdb.ts %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2) %>% 
  dplyr::summarize(Original=mean(Value),
                   Stineman=mean(Stineman)) %>% 
  mutate(Scale3=paste(Scale1,Scale2)) %>% 
  tidyr::gather(key = "Interpolation",value="InterpolationValue",Stineman) %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  ungroup
  

gapfill.format<- gapfill.db %>% 
  dplyr::rename(OP3=Interpolation) %>% 
  dplyr::rename(Value=InterpolationValue) %>% 
  dplyr::select(Date,DOY,Month,Year,Station,Scale1,Scale2,Scale3,OP1,OP2,OP3,Value) 

binder.gapfill<-gapfill.format %>% filter(DOY>90 & DOY<300)
saveRDS(binder.gapfill,"Paper2/OpticalSensorGapfillCombine.rds")

Harvests17<- tibble::enframe(c("2017-07-03","2017-09-04","2017-09-18"),value="Date",name=NULL) %>%  
  mutate(Station="Vimes1500") %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=year(Date))

Harvests18<- tibble::enframe(c("2018-06-23","2018-09-08"),value="Date",name=NULL) %>%  
  mutate(Station="Vimes1500") %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=year(Date))

Harvests<-bind_rows(Harvests17,Harvests18)

gg.sensors<-ggplot(gapfill.db,aes(DOY,InterpolationValue))+
  theme_bw()+
  facet_grid(vars(Scale3,OP2),vars(Year),scales="free")+
  geom_line(aes(col=Station))+
  geom_point(aes(y=Original,col=Station,pch=Station),alpha=.5)+
  scale_color_manual(values=c("grey70","black"))+
  geom_vline(data=Harvests,aes(xintercept=DOY),col="black",lty=2)+
  scale_x_continuous(breaks=sdiff,labels = month.abb)+
  ylab("Index Values")+
  xlab("Month of the year")

ggsave(gg.sensors,filename = "Paper2/OpticalSensors.png",height=14,width=10)

# Biophysical Data ------------------------------------------------------------
#* Drought Indices ---------------------------------------------------------

spiSeverity<-read.table(file = "tables/spitab.txt",header = T,sep = " ",stringsAsFactors = F) %>% 
  as_tibble %>% 
  mutate(Index=as.factor(c(1:nrow(.))))
allsevs<-spiSeverity$Value %>% unique
cols<-spiSeverity %>% 
  select(Value,Color) %>% 
  dplyr::rename(SYear2018=Value) %>% 
  deframe
cols["near normal"]<-"grey"
cols["moderately dry"]<-"gold"


droughtall<-droughtI %>% 
  filter(Date>"2016-12-31") %>% 
  dplyr::rename(Station=District) %>% 
  mutate(OP1=map_chr(Name2,function(x)str_split(x," ")[[1]][1])) %>% 
  dplyr::rename(OP2=Indices) %>% 
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
  dplyr::rename(OP1=Depth) %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Value=Mean)

#* Biomass -----------------------------------------------------------------
biolai.tab<- biolai2 %>% 
  ungroup %>% 
  add_column(OP1="Site") %>% 
  add_column(OP3="Mean") %>% 
  dplyr::rename(OP2=Prop3) %>% 
  dplyr::select(Date,Station,Scale1,Scale2,OP1,OP2,OP3,Mean)


# * Combine ---------------------------------------------------------------
drought.binder<-bind_rows(droughtall,swc.tab,biolai.tab) %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  dplyr::select(Date,DOY,Month,Year,Station,Scale1,Scale2,OP1,OP2,OP3,Value)


saveRDS(drought.binder,"Paper2/DroughtIndexCombine.rds")


# STATISTICS --------------------------------------------------------------
# Optical vs. -------------------------------------------------------------

#* Meteo -------------------------------------------------------------------


spispei<-drought.binder %>% 
  filter(Scale2=="Drought") %>% 
  filter(OP1=="02200MS"| OP1=="02500MS") %>% 
  group_by(Month,Year) %>% 
  dplyr::summarize(Value=mean(Value)) %>% 
  mutate(Severity=map_chr(Value,function(x,v=spiSeverity){
    
    ret<-as.character(v[which(x>v$From & x<v$To),"Value"])
    
  })) %>%  
  ungroup %>% 
  mutate(Year2=paste0("SYear",Year)) %>% 
  select(-c(Value,Year)) %>% 
  mutate(Severity=factor(Severity,levels=allsevs)) %>%
  spread(Year2,Severity)

swcredux<- drought.binder %>% filter(Scale2=="SWC") %>% 
  select(DOY,Month,Station,SWC=Value)


# Optical Drought Year

opticaldrought.year<-binder.gapfill %>% 
  ungroup %>% 
  mutate(DOY=yday(Date)) %>% 
  select(-Date) %>% 
  mutate(Year2=paste0("OYear",Year)) %>% 
  select(-Year) %>%  
  spread(Year2,Value) %>% 
  mutate(Scale3=paste(Scale1,Scale2)) %>% 
  mutate(Scale4=paste(Scale3,OP2)) %>% 
  arrange(Scale4)

lj.year<-left_join(opticaldrought.year,spispei,by=c("Month")) %>% 
  mutate(Difference1718=OYear2017-OYear2018) %>% 
  left_join(.,swcredux,by=c("DOY","Month","Station"))



gdry<-ggplot(lj.year)+
  scale_color_manual(values=cols)+
  theme_bw() +
  geom_point(aes(DOY,Difference1718,col=SYear2018,pch=SYear2017))+
  geom_line(aes(x=DOY,y=SWC),col="blue")+
  facet_grid(vars(Scale3,OP2),vars(Station),scales="free")+
  labs(pch="Severity 2017",col="Severity 2018")+
  geom_vline(data=Harvests,aes(xintercept=DOY),lty=2)+
  ylab("Difference optical signal by year (2017 - 2018)")


ggsave(gdry,filename = "Paper2/Results_OpticalvsDroughtYear.png",height=14,width=10)


# Optical Drought Station

opticaldrought.station<-binder.gapfill %>% 
  ungroup %>% 
  mutate(DOY=yday(Date)) %>% 
  select(-Date) %>% 
  spread(Station,Value) %>% 
  mutate(Scale3=paste(Scale1,Scale2)) %>% 
  mutate(Scale4=paste(Scale3,OP2)) %>% 
  arrange(Scale4)

lj.station<-left_join(opticaldrought.station,spispei,by=c("Month")) %>% 
  mutate(Difference1718=P2-Vimes1500)

gdry2<-ggplot(lj.station,aes(DOY,Difference1718,col=SYear2018,pch=SYear2017))+
  scale_color_manual(values=cols)+
  theme_bw() +
  geom_point()+
  facet_grid(vars(Scale3,OP2),vars(Year),scales="free")+
  labs(pch="Severity 2017",col="Severity 2018")+
  geom_vline(data=Harvests,aes(xintercept=DOY),lty=2)+
  ylab("Difference pptical signal by station (P2 - Vimes1500)")

ggsave(gdry2,filename = "Paper2/Results_OpticalvsDroughtStation.png",height=14,width=10)



# * Agri ----------------------------------------------------

mnt<-bind_cols(Month=seq(1:length(month.abb)),Month.name=factor(month.abb,levels=month.abb))

jn<-left_join(gapfill.format,swc,by = c("Date", "Station")) %>% 
  mutate(Year=as.character(Year)) %>% 
  filter(Month>3 & Month<10) %>% 
  left_join(.,mnt,by="Month") %>% 
  na.omit %>% 
  group_by(Station,Year,Month,Month.name,OP2,Depth,Scale2) %>% 
  nest %>% 
  mutate(CCF=map(data,function(x){
    
    x1<-as.numeric(x$Mean)
    x2<-as.numeric(x$Value)
    
    c<-Ccf(x1,x2,plot=F,lag.max = 10)
    d<-cbind.data.frame(c[[4]],c[[1]])
    names(d)<-c("Lags","R")
    return(d)
    
    
  })) %>% unnest(CCF) %>% 
  filter(Month==4|Month==5|Month==6|Month==7)

jn0<-filter(jn,Lags==0)

ggauto<-ggplot(jn,aes(Lags,R,col=Station,linetype=Depth))+ theme_bw()+
  geom_line()+
  scale_color_manual(values=c("grey70","black"))+
  geom_point(data=jn0,aes(Lags,R,col=Station))+
  facet_grid(vars(Year,Month.name),vars(Scale2,OP2))+
  ylab("Pearson Correlation Coefficient (SWC vs. VIs)")+
  xlab("Lag (in Days)") +
  ylim(-1,1)

ggsave(ggauto,filename = "Paper2/SWC_Autocorrelation_Optical_v201912.png",height=11,width=10)


# Biophysical vs. ---------------------------------------------------------
# * Meteo -----------------------------------------------------------------

bmlai<-biolai2 %>% filter(Scale2=="Biomass" | Scale2=="Leaf Area") %>% 
  mutate(Month=month(Date))

spispei2<-drought.binder %>% 
  filter(Scale2=="Drought") %>% 
  filter(OP1=="02200MS"| OP1=="02500MS") %>% 
  group_by(Month,Year) %>% 
  dplyr::summarize(Value=mean(Value)) %>% 
  mutate(Severity=map_chr(Value,function(x,v=spiSeverity){
    
    ret<-as.character(v[which(x>v$From & x<v$To),"Value"])
    
  })) %>% 
  mutate(Year=as.character(Year)) %>% 
  mutate(Severity=factor(Severity,levels=allsevs)) 

bmlai2<-left_join(bmlai,spispei2,by = c("Year", "Month")) %>%  
  tidyr::separate(col = Indicator, into=c("Indicator1","Indicator2"),sep="-") %>% 
  mutate(Indicator2=map_chr(Indicator2,function(x) str_replace(x," \\(in %\\)",""))) 

h2<-Harvests %>% mutate(Year=as.character(Year))

g1<-ggplot(bmlai2,aes(DOY,Mean,linetype=Year))+ theme_bw()+
  scale_color_manual(values=cols)+
  geom_line()+
  geom_point(aes(col=Severity,pch=Severity),cex=2)+
  geom_errorbar(aes(ymin=Min,ymax=Max,col=Severity))+
  scale_linetype_manual(values=c(1,2))+
  facet_grid(vars(Indicator1,Indicator2),vars(Station,),scales="free")+
  geom_vline(data=h2,aes(xintercept=DOY,lty=Year))+
  theme(legend.position="bottom")

ggsave(g1,filename = "Paper2/BiomassLAI_Drought.png",height=6.5,width=9)


bmlaiMean<-bmlai2 %>% group_by(Year,Station,Scale2,Prop3) %>% 
  dplyr::summarise(mean=mean(Mean),sd=sd(Mean))


# * Agri -----------------------------------------------------------------


s<-seq(as_date("2017-01-01"),as_date("2018-01-01"),by="month")-1
sdiff<- cumsum(as.numeric(c(0,diff(s))[1:12]))

bmlai.agri<-bmlai2 %>% 
  select(-Value) %>% 
  rename(Value=Mean) %>% 
  distinct %>% 
  group_by(Year,Station,Scale1,Indicator1,Indicator2) %>% 
  nest %>% 
  arrange(Station)
  

bmlai.ts <- bmlai.agri %>% 
  mutate(ts.approx=map(data,function(i){
    
    df1 <-as.data.frame(i) %>% 
      dplyr::select(Date,Value) %>% 
      group_by(Date) %>% 
      dplyr::summarize(Value=mean(Value)) %>% 
      ungroup
    
    tsib<-as_tsibble(df1,index="Date") %>% 
      fill_gaps() %>% 
      mutate(Approach=map_chr(Value,function(x) {ifelse(is.na(x),"Interpolated","Original")})) %>% 
      mutate(Interpolation=na_interpolation(Value,"stine"))
    
    
    return(as_tibble(tsib))
    
  }))




bmlai.db<-bmlai.ts %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  group_by(Date,Station,Scale1,Indicator1,Indicator2) %>% 
  dplyr::summarize(Original=mean(Value),
                   Interpol=mean(Interpolation)) %>% 
  tidyr::gather(key = "Interpolation",value="InterpolationValue",Interpol) %>% 
  mutate(DOY=yday(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Year=year(Date)) %>% 
  ungroup


bmlai.format<- bmlai.db %>% 
  dplyr::rename(OP3=Interpolation) %>% 
  dplyr::rename(Value=InterpolationValue) %>% 
  dplyr::select(Date,DOY,Month,Year,Station,Scale1,Indicator1,Indicator2,Value) 



mnt<-bind_cols(Month=seq(1:length(month.abb)),Month.name=factor(month.abb,levels=month.abb))

jn.bio<-left_join(bmlai.format,swc,by = c("Date", "Station")) %>% 
  mutate(Year=as.character(Year)) %>% 
  filter(Month>3 & Month<10) %>% 
  left_join(.,mnt,by="Month") %>% 
  na.omit %>% 
  group_by(Station,Year,Month,Month.name,Indicator1,Indicator2,Depth) %>% 
  nest %>% 
  mutate(CCF=map(data,function(x){
    
    x1<-as.numeric(x$Mean)
    x2<-as.numeric(x$Value)
    
    c<-Ccf(x1,x2,plot=F,lag.max = 20)
    d<-cbind.data.frame(c[[4]],c[[1]])
    names(d)<-c("Lags","R")
    return(d)
    
    
  })) %>% 
  unnest(CCF) %>% 
  filter(!(Month==4 & Year=="2018")) %>% 
  filter(Month==4|Month==5|Month==6|Month==7)

jn0<-filter(jn.bio,Lags==0)
  

ggauto.bio<-ggplot(jn.bio,aes(Lags,R,col=Station,linetype=Depth))+ theme_bw()+
  geom_line()+
  scale_color_manual(values=c("grey70","black"))+
  geom_point(data=jn0,aes(Lags,R,col=Station,pch=Station))+
  facet_grid(vars(Year,Month.name),vars(Indicator1,Indicator2))+
  ylab("Pearson Correlation Coefficient (SWC vs. Biophysical Data)")+
  xlab("Lag (in Days)") +
  ylim(-1,1)

ggsave(ggauto.bio,filename = "Paper2/SWC_Autocorrelation_Biophysical_v201912.png",height=8,width=10)


