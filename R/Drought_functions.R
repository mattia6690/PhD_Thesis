# Function necessary for the Computation of the Second paper




sfRectBuff<-function(sfO,distx,disty){
  
  b<- st_bbox(sfO)
  c<- st_bbox(c(b[1]-distx,b[2]-disty,b[3]+distx,b[4]+disty),crs=st_crs(b))
  d<- st_as_sfc(c)
  return(d)
}



# Function to Tidy Alpenv Data
readSWCAlpEnv<-function(table,Station="",headRows,headNames,TimeCol,StartCol){
  
  Time    <- table[headRows+1:length(table$X1),TimeCol]
  TimeCHR <- map(Time,function(x) as_datetime(as.character(x),format="%Y-%m-%d %H:%M"))
  
  data.seqC<-c(1+StartCol:ncol(table)-1)
  data.seqR<-c(headRows+1:nrow(table))
  
  head<- table[1:headRows,startcol:ncol(table)]%>% 
    t %>% 
    as_tibble %>% 
    setNames(headNames) %>% 
    add_column(ID=c(1:nrow(.)),.before = T)
  
  
  data<- head %>% 
    mutate(Data=map(ID,function(x,t=table,hr=data.seqR,sc=data.seqC,time=TimeCHR){
      
      ret <- t[hr,sc[x]]
      ret <- as_tibble(cbind(time,ret))
      ret <- setNames(ret,c("TimeStamp","Value"))
      ret <- mutate(ret,Value=as.numeric(Value))
      return(ret)
      
    }))
  
  meta<- data %>% 
    add_column(Station=Station,.before = T) %>% 
    unnest
  
  return(meta)
  
}