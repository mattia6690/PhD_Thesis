
# Input -------------------------------------------------------------------
source("R/BaseFunctions.R")
library("xts")

suffix<-"110119"
suffix.out<- "161019"
subfolder<-"01_Combination"
indir<-paste0(MetricsDir,subfolder,"/")

db<-readRDS(paste0(indir,"AllSensorData",suffix,".rds"))
db.all<-readRDS(paste0(indir,"AllSensorData",suffix,"_filtered.rds"))
db.mean<-readRDS(paste0(indir,"AllSensorData",suffix,"_meanDay.rds"))

subfolder<-"02_Gapfill"
outdir<-paste0(MetricsDir,subfolder,"/")

# Linear Gap Filling ----------------------------------------------------------

gpdb.fill<-db.all %>% 
  mutate(Date=as.Date(Date)) %>% 
  group_by(Station,Plot,Scale1,Sensor,OP2) %>% nest %>% 
  arrange(Station,Plot) 

gpdb.ts <- gpdb.fill %>% 
  mutate(ts.approx=map(data,function(i){
    
    df1<-as.data.frame(i) %>% dplyr::select(Date,Value)
    df1.zoo<-xts(df1[,-1],df1[,1]) 
    ts<-zoo(,seq(start(df1.zoo),end(df1.zoo),by=1))
    df2 <- merge(df1.zoo,ts, all=TRUE) %>% setNames("Value")
    df3<-xts(na.approx(df2$Value)) %>%  data.frame(Date=index(.))
    
  }))

# gapfill.db<-gpdb.ts %>% 
#   dplyr::select(-data) %>% unnest %>% 
#   dplyr::select(Date,Station,Plot,Scale1,Sensor,OP2,Value) %>% 
#   filter((Station!="Vimes1500") & (Date>"2017-07-17" | Date<"2017-07-03"))

gapfill.db<-gpdb.ts %>% 
  dplyr::select(-data) %>% unnest %>% 
  dplyr::select(Date,Station,Plot,Scale1,Sensor,OP2,Value)

# wh<-which(gapfill.db$Station=="Vimes1500" & gapfill.db$Date>"2017-07-03" & gapfill.db$Date<"2017-07-17")
# gapfill.db<-slice(gapfill.db,-wh)

saveRDS(gapfill.db,file = paste0(outdir,"AllSensorData",suffix.out,"_gapFill.rds"))
