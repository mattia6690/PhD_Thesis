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




data2<-lapply(lf,function(i){
  
  print(i)
  # Tidy the data
  station    <- strsplit(basename(i),"_")[[1]][1]
  tab        <- suppressMessages(read_csv(i,skip=1,col_names = F))
  tab_slice  <- tab %>% slice(-(1:3))
  
  data <- tab_slice  %>% 
    dplyr::select(Date=1)%>% 
    mutate(Station=station) %>% 
    mutate(Scale1="MONALISA") %>% 
    mutate(Scale2="Decagon") %>% 
    mutate(OP1="Point")
  
  
  #* Add NDVI ----
  dataN<- mutate(data,OP2="NDVI",OP3="Avg")
  
  if(any(unlist(tab[1,])=="NDVI_Avg")){
    
    ndvi<- tab_slice %>% dplyr::select(which(tab[1,]=="NDVI_Avg"))
    
  } else {
    
    redUp   <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="NDVI_UpRed_Avg")) %>% unlist %>% as.numeric
    redDown <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="NDVI_Down_Red_Avg")) %>% unlist %>%as.numeric
    nirUp   <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="NDVI_UpNIR_Avg")) %>% unlist %>%as.numeric
    nirDown <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="NDVI_Down_NIR_Avg")) %>% unlist %>%as.numeric
    
    if(length(redUp)==0) {
      
      parUp <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="PAR_Up_Avg")) %>% unlist%>% as.numeric
      redUp <- nirUp <-parUp
      
      if(length(redUp)==0) redUp<-nirUp <- rep(1,length(redDown))
      
    }
    
    ndvi <- ((nirDown/nirUp)-(redDown/redUp))/((redDown/redUp)+(nirDown/nirUp))
    
  }
  
  #* Add PRI ----
  dataP<- mutate(data,OP2="PRI",OP3="Avg")
  if(any(unlist(tab[1,])=="PRI_Avg")){
    
    pri<- tab_slice %>% dplyr::select(which(tab[1,]=="PRI_Avg"))
    
  } else {
    
    redUp   <- tab_slice %>% dplyr::select(which(tab[1,]=="PRI_Up_Green_Avg")) %>% unlist %>%as.numeric
    redDown <- tab_slice %>% dplyr::select(which(tab[1,]=="PRI_Down_Green_Avg")) %>% unlist %>%as.numeric
    nirUp   <- tab_slice %>% dplyr::select(which(tab[1,]=="PRI_Up_Yellow_Avg")) %>% unlist %>%as.numeric
    nirDown <- tab_slice %>% dplyr::select(which(tab[1,]=="PRI_Down_Yellow_Avg")) %>% unlist %>%as.numeric
    
    if(length(redUp)==0) {
      
      parUp <- tab_slice %>% dplyr::select(which(unlist(tab[1,])=="PAR_Up_Avg")) %>% unlist %>%as.numeric
      redUp <- nirUp <-parUp
      
      if(length(redUp)==0) redUp<-nirUp <- rep(1,length(redDown))
      
    }
    
    pri <- ((nirDown/nirUp)-(redDown/redUp))/((redDown/redUp)+(nirDown/nirUp))
    
  }
  
  # Add NDVI and PRI to data frame
  data.pri  <- mutate(dataP,Value=as.numeric(unlist(pri)))
  data.ndvi <- mutate(dataN,Value=as.numeric(unlist(ndvi)))
  
  final<-bind_rows(data.pri,data.ndvi)
  return(final)
  
})


tableall<-do.call(bind_rows,data2) %>%
  filter(!is.nan(Value)) %>% 
  mutate(Date=as.POSIXct(Date,format="%Y-%m-%d %H:%M")) %>% 
  mutate(Date2=as_date(Date)) %>% 
  mutate(Time=as_hms(Date)) %>% 
  filter(Time>=as_hms(timerange[1]) & Time<=as_hms(timerange[2]))

# ggplot(tableall,aes(Date,Value,color=Station))+
#   geom_line()+
#   facet_grid(vars(OP2),vars(Station))

saveRDS(tableall,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_Paper2.rds"))
if(writeCSV==T) write.csv(mnls.tidy.raw,paste0(MonalisaDir,"02_Tables/Monalisa_NDVI_Paper2.csv"))


mnls.raw.group<-tableall %>% 
  group_by(Date2,Station,Scale1,Scale2,OP1,OP2,OP3) %>% nest

# Filter Date -------------------------------------------------------------
mnls.outlier.date<- mnls.raw.group %>% 
  mutate(data=map(data,function(i){
    
    val<-na.omit(i$Value)
    
    if(length(val)>5){
      
      gDens <- .getDensityLimits(val)
      lof   <- lofactor(val, k=3)
      
      dat1   <- i %>% 
        add_column(.,Lower=gDens[1]) %>% 
        add_column(.,Upper=gDens[4]) %>% 
        add_column(.,LOF=lof)
    } else{
          
      dat1 <- NA
      
    }
    
    return(cbind(i,dat1))
    
  }))

mnls.outlier.date1<-mnls.outlier.date %>% 
  unnest %>% 
  filter(Value>Lower) %>% 
  filter(Value<Upper) %>% 
  filter(LOF<1.5)

g.f1<-ggplot(mnls.outlier.date1,aes(Date2,Value))+geom_line()+facet_grid(vars(OP2),vars(Station))

# Filter Moving Window -------------------------------------------------------------
print("Moving Window Filtering")

t.win<-4
input<-mnls.outlier.date1
stats<- input$Station %>% unique %>% as.character %>% as.tibble %>% setNames("station")
dates<- input$Date2 


seq1<- seq(min(dates),max(dates)-t.win,1)
seq2<- seq(min(dates)+t.win,max(dates),1)

# Create Moving Window Timespans
mw.combi<-stats %>% 
  
  mutate(dates=map(station,function(x,d=dates,w=t.win){
    
    seq1<- seq(min(d),max(d)-w,1) %>% as.tibble
    seq2<- seq(min(d)+w,max(d),1) %>% as.tibble
    bc<-bind_cols(seq1,seq2) %>% setNames(c("start","end"))
    
  })) %>% unnest

# Filter Station and Dates per Moving Window Iteration
mw.data<-input %>% group_by(Station,Scale1,Scale2,OP1,OP2,OP3) %>% nest

# Go through the grouped stuff
mw.filter<- mw.data %>% 
  mutate(error=map(data,function(x){

    # Do the moving window
    lp<-map2(seq1,seq2,function(s1,s2){
      
      if(is.element(s2,x$Date2)){
        
        dat<-filter(x,Date2>=s1 & Date2<=s2)
        
        if(nrow(dat)   < 3) return(rep(1,nrow(dat)))
        if(length(dat) < 3) return(rep(1,nrow(dat)))
        
        
        sel<-dat %>% select(Value) %>% unlist 
        if(all(is.nan(sel))) return(rep(1,nrow(dat)))
        
        limits<-.getDensityLimits(sel,method="max")
        err<-(sel<limits[1] | sel>limits[3] | sel>limits[4])*1
        if(length(err)==0) err<-rep(0,nrow(dat))
        
        dat$MWLOF<-err
        
        return(dat)
        
      }
    })
    
    y<-do.call(rbind,lp)
    return(y)
    
  }))


mw.filter2<- mw.filter %>% 
  select(-data) %>% 
  unnest %>% 
  group_by(Date,Date2,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  dplyr::summarise(Value=quantile(Value,.9),MWLOF=sum(MWLOF)) %>% 
  ungroup %>% 
  arrange(Date) %>% 
  filter(MWLOF<4)


g.mw1<-ggplot(mw.filter2,aes(Date,Value))+geom_point()+facet_grid(vars(OP2),vars(Station))
saveRDS(mw.filter2,paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_311019.rds"))

mw.filter2.day<-mw.filter2 %>% 
  mutate(Date2=as_date(Date)) %>% 
  group_by(Date2,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
  dplyr::summarize(Value=mean(Value)) %>% 
  ungroup


saveRDS(mw.filter2.day,paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_daily_311019.rds"))
# 
# # Filter for Outliers in Density
# mw.stat<-mw.filter %>% mutate(error=map(data,function(dat) {
#   
#   if(nrow(dat) < 3) return(rep(1,nrow(dat)))
#   sel<-dat %>% select(Value) %>% unlist 
#   if(all(is.nan(sel))) return(rep(1,nrow(dat)))
#   
#   limits<-.getDensityLimits(sel,method="max")
#   
#   err<-(sel<limits[1] | sel>limits[3] | sel>limits[4])*1
#   if(length(err)==0) err<-rep(0,nrow(dat))
#   return(err)
#   
# }))
# 
# # Unnest the Data
# mw.comb1<-mw.stat %>% 
#   mutate(error.mw=map2(data,error,function(x,y) bind_cols(select(x,Date),as.tibble(y)))) %>% 
#   select(data,error.mw) %>% 
#   unnest
# 
# # Calculate the Sum of Iterations classified as outlier
# mw.comb2<-mw.comb1 %>% 
#   group_by(Date,Time,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
#   dplyr::summarise(error.mw=sum(value)) %>% 
#   ungroup
# 
# # Apply the Treshold when an observation is considered an outlier
# error.mw<-left_join(input,mw.comb2) %>% filter(error.mw==4)
# g.mw1<-ggplot(error.mw,aes(Date,Value))+geom_point()+facet_wrap(.~Station)
# 
# # 90 Percentile NDVI ------------------------------------------------------
# mnls.percentile<- error.mw %>% 
#   group_by(Date,Station,Scale1,Scale2,OP1,OP2,OP3) %>% 
#   dplyr::summarise(Value=quantile(Value,.9))
# 
# g.f2<-ggplot(mnls.percentile,aes(Date,Value))+geom_point()+facet_wrap(.~Station)
# saveRDS(mnls.percentile,paste0(Monalisa17Dir,"Monalisa_NDVI_filtered_MovingWindow_311019.rds"))
