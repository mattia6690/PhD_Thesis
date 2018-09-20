# Analyzing EURAC Decagon Data
# Mattia Rossi
# Combination and Filtering

# Environment -------------------------------
source("R/BaseFunctions.R")
ndvidir<-paste0(MonalisaDir,"01_Data/Monalisa/")

# Requirements ------------------------------
lf<-list.files(ndvidir,full.names = T)
lf_short<-list.files(ndvidir,full.names = F)

timerange<-c("10:30:00","15:00:00")
daterange<-c("2017-03-01","2017-11-01")
dir<-paste0(Monalisa17Dir,"02_Tables/")
dircheckup(dir)

# Generate Table ----------------------------------------------------------
stations<-str_split(lf_short,"_") %>% 
  map(function(i) i[1]) %>% 
  unlist
names<-c("Date","Station","Scale1","Scale2","OP1","OP2","OP3","Value")

# bit oldschool
lf.t1<-lf.t2<-list()
for (i in 1:length(lf)){
  
  tab        <-suppressMessages(read_csv(lf[i],col_names = F))
  lf.t1[[1]] <-tab %>% slice(-(1:4)) %>% select(1) %>% as.tibble
  lf.t1[[2]] <-rep(stations[i],nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[3]] <-rep("MONALISA",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[4]] <-rep("DECAGON",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[5]] <-rep("Point",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[6]] <-rep("NDVI",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[7]] <-rep("Avg",nrow(lf.t1[[1]])) %>% as.tibble
  lf.t1[[8]] <-tab[-(1:4),] %>% select(which(tab[2,]=="NDVI_Avg"))
  lf.t2[[i]] <-do.call(cbind.data.frame, lf.t1) %>% setNames(names)
  
}

mnls.tidy.raw <-lf.t2 %>% 
  do.call(rbind.data.frame,.) %>% 
  as.tibble %>%
  mutate(Date=as.POSIXct(Date,format="%Y-%m-%d %H:%M")) %>% 
  separate(Date,c("Date","Time")," ") %>% 
  mutate(Date=as_date(Date)) %>%
  mutate(Time=as.hms(Time)) %>% 
  mutate(Value=as.numeric(Value)) %>% 
  filter(!is.nan(Value))

saveRDS(mnls.tidy.raw,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_raw.rds"))
if(writeCSV==T) write.csv(mnls.tidy.raw,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_raw.csv"))


# Filter Time -------------------------------------------------------------
mnls.raw.time<-mnls.tidy.raw %>% 
  filter(Time>=as.hms(timerange[1]) & Time<=as.hms(timerange[2])) %>% 
  filter(Date>=as.Date(daterange[1]) & Date<=as.Date(daterange[2]))

mnls.raw.group<-mnls.raw.time %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% nest

# Filter Date -------------------------------------------------------------
mnls.outlier.date<- mnls.raw.group %>% 
  mutate(data=map(data,function(i){
    
    gDens <-.getDensityLimits(i$Value)
    lof   <- i %>% select(Value) %>% lofactor(., k=3)
    
    dat1   <- i %>% 
      add_column(.,Lower=gDens[1]) %>% 
      add_column(.,Upper=gDens[4]) %>% 
      add_column(.,LOF=lof)
    
    return(cbind(i,dat1))
    
  }))

mnls.outlier.date1<-mnls.outlier.date %>% unnest %>% 
  filter(Value>Lower) %>% 
  filter(Value<Upper) %>% 
  filter(LOF<1.5)

g.f1<-ggplot(mnls.outlier.date1,aes(Date,Value))+geom_point()+facet_wrap(.~Station)

# Filter Moving Window -------------------------------------------------------------
print("Moving Window Filtering")

t.win<-4
input<-mnls.outlier.date1
stats<- input$Station %>% unique %>% as.character %>% as.tibble %>% setNames("station")
dates<- input$Date 

# Create Moving Window Timespans
mw.combi<-stats %>% 
  mutate(dates=map(station,function(x,d=dates,w=t.win){
    
    seq1<- seq(min(d),max(d)-w,1) %>% as.tibble
    seq2<- seq(min(d)+w,max(d),1) %>% as.tibble
    bc<-bind_cols(seq1,seq2) %>% setNames(c("start","end"))
    
  })) %>% unnest

# Filter Station and Dates per Moving Window Iteration
mw.filter<-mw.combi %>% 
  mutate(data=pmap(list(station,start,end),function(x,y,z,i=input){

    i %>% filter(Station==x) %>% filter(Date>=y) %>% filter(Date<=z) %>% return()

  }))

# Filter for Outliers in Density
mw.stat<-mw.filter %>% mutate(error=map(data,function(dat) {
  
  if(nrow(dat) < 3) return(rep(1,nrow(dat)))
  sel<-dat %>% select(Value) %>% unlist 
  if(all(is.nan(sel))) return(rep(1,nrow(dat)))
  
  limits<-.getDensityLimits(sel,method="max")
  
  err<-(sel<limits[1] | sel>limits[3] | sel>limits[4])*1
  if(length(err)==0) err<-rep(0,nrow(dat))
  return(err)
  
}))

# Unnest the Data
mw.comb1<-mw.stat %>% 
  mutate(error.mw=map2(data,error,function(x,y) bind_cols(select(x,Date),as.tibble(y)))) %>% 
  select(data,error.mw) %>% 
  unnest

# Calculate the Sum of Iterations classified as outlier
mw.comb2<-mw.comb1 %>% 
  group_by(Date,Time,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  dplyr::summarise(error.mw=sum(value)) %>% 
  ungroup

# Apply the Treshold when an observation is considered an outlier
error.mw<-left_join(input,mw.comb2) %>% filter(error.mw==4)
g.mw1<-ggplot(error.mw,aes(Date,Value))+geom_point()+facet_wrap(.~Station)

# 90 Percentile NDVI ------------------------------------------------------
mnls.percentile<- error.mw %>% 
  group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  dplyr::summarise(Value=quantile(Value,.9))

g.f2<-ggplot(mnls.percentile,aes(Date,Value))+geom_point()+facet_wrap(.~Station)
saveRDS(mnls.percentile,paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_100818.rds"))
