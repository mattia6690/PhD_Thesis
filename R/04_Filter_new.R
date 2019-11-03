# Analyzing EURAC PhenoCams
# Mattia Rossi, Jeroen Staab
# VI Computation


# Environment -------------------------------
lf<-list.files(paste0(getwd(),"/MattiaCode/Functions"),full.names=T)
sapply(lf,source)

# Requirements ------------------------------
data.VIs<-readRDS(paste0(combi_DIR,"VIs.rds"))
t.win<-4

path <- "//abz01sat.eurac.edu/SAT2/ProjectData/MONALISA/Pillar2/PhenoCam/Phenocam/"
stations<- list.files(path)
stations.full<- list.files(path,full.names = T)
viname<-"Paper2"


# Load VI Data

data.VIs<-map_dfr(stations.full,function(x){
  
  vipath <- dget(paste0(x,"/config.txt"))$path.vi
  file   <- list.files(vipath,pattern=paste0(viname,"_metrix_VIs"),full.names = T)
  file   <- readRDS(file)
  
})


# Outlier Removal ---------------------------
# * Daily Outliers --------------------------
print("Intra-Day Filtering")
roifilter.day<- data.VIs  %>% mutate(Date2=as.Date(Date)) %>% group_by(Station,Date2) %>% nest

filter.day<-roifilter.day %>% mutate(error.day=map(data,function(dat) {
  
  if(nrow(dat) < 3) return(rep(1,nrow(dat)))
  sel<-dat %>% select(ndvi) %>% unlist 
  if(all(is.nan(sel))) return(rep(1,nrow(dat)))
  
  limits<-.getDensityLimits(sel,method="max")
  
  err<-(sel<limits[1] | sel>limits[3] | sel>limits[4])*1
  if(length(err)==0) err<-rep(0,nrow(dat))
  return(err)
  
}))

error.day<- unnest(filter.day) %>% filter(error.day==0) 
saveRDS(error.day,file = paste0(combi_DIR,"Filter/01_IntraDay.rds"))


# * Moving Window Outliers ------------------
print("Moving Window Filtering")
stats<- enframe(stations) %>% setNames(c("ID","station"))
dates<- error.day$Date2 
mw.combi<-stats %>% 
  mutate(dates=map(station,function(x,d=dates,w=t.win){
    
    seq1<- seq(min(d),max(d)-w,1) %>% as.tibble
    seq2<- seq(min(d)+w,max(d),1) %>% as.tibble
    bc<-bind_cols(seq1,seq2) %>% setNames(c("start","end"))
    
  })) %>% unnest


mw.filter<-mw.combi %>% 
  mutate(data=pmap(list(station,start,end),function(x,y,z,inp=error.day){
    
    filter(inp, station==x & Date2>=y & Date2<=z )
    
}))

mw.stat<-mw.filter %>% mutate(error=map(data,function(dat) {
  
  if(nrow(dat) < 3) return(rep(1,nrow(dat)))
  sel<-dat %>% select(ndvi) %>% unlist 
  if(all(is.nan(sel))) return(rep(1,nrow(dat)))
  
  limits<-.getDensityLimits(sel,method="max")
  
  err<-(sel<limits[1] | sel>limits[3] | sel>limits[4])*1
  if(length(err)==0) err<-rep(0,nrow(dat))
  return(err)
  
}))

mw.comb<-mw.stat %>% 
  mutate(error.mw=map2(data,error,function(x,y) bind_cols(select(x,Date),as.tibble(y)))) %>% 
  select(station,error.mw) %>% 
  unnest %>% 
  group_by(station,Date) %>% 
  dplyr::summarise(error.mw=sum(value)) %>% 
  ungroup

error.mw<-left_join(error.day,mw.comb) %>% filter(error.mw<20)
saveRDS(error.mw,file = paste0(combi_DIR,"Filter/Paper2_Phenocam_MovingWindow.rds"))

# Daily Statistics --------------------------
metrix_day1<-error.mw %>% 
  mutate(Date2=as.Date(Date)) %>% 
  group_by(station,Date2,Type,ROI) %>% 
  dplyr::summarise(NDVI=quantile(ndvi,.9),
                   GCC=quantile(gcc,.9),
                   RCC=quantile(rcc,.9),
                   BCC=quantile(bcc,.9),
                   EXG=quantile(exg,.9)) %>% 
  ungroup

saveRDS(metrix_day1,file = paste0(combi_DIR,"Filter/Paper2_Phenocam_MovingWindow_byday.rds"))

metrix_day2<-metrix_day1 %>% 
  separate(ROI,into=c("ROI1","ROI2"),sep="_") %>% 
  mutate(ROI_fin=map2_chr(ROI1,ROI2,function(x,y) {if(is.na(y)) x else y})) %>% 
  filter(!is.na(ROI_fin))

# Plotting --------------------------

# Daily Outliesr ----------------------------

# Type either error.day or error.mw
file<-error.mw %>% 
  dplyr::select(Station,Date,ndvi,rcc,gcc,bcc,exg) %>% 
  filter(Station=="vimes1500"| Station=="LTER_P2") %>% 
  group_by(Station,Date) %>% dplyr::summarize(ndvi=mean(ndvi,na.rm=T),
                                       rcc=mean(rcc,na.rm=T),
                                       gcc=mean(gcc,na.rm=T),
                                       bcc=mean(bcc,na.rm=T),
                                       exg=mean(exg,na.rm=T)) %>% 
  gather(key="Index",value = "Value",ndvi,rcc,gcc,bcc,exg)

ggplot(file,aes(Date,Value))+
  facet_wrap(Station~Index,ncol = 5,scales = "free")+
  theme_bw()+
  geom_point()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title="Phenocam Optical Indices for Vimes1500 and P2")

# Daily Statistics





