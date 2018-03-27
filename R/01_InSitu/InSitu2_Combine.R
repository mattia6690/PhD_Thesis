###########################'
source("R/BaseFunctions.R")


# 1. Combine all the Information in one dataframe

lf<-list.files(paste0(dirfield,"/03_DaySOS"),pattern=".csv",full.names=T)

list1<-suppressMessages(map(lf,read_csv))
list2<-do.call(rbind,list1) %>% select(-X1)

write.csv(list2,file=paste0(MetricsDir,"/","CombinedInSituMetricsSOS.csv"))
saveRDS(list2,file=paste0(MetricsDir,"/","CombinedInSituMetricsSOS.rds"))

# 2. Aggregation of the previous result ----
# MAKE IT TIDY
lf<-list.files(paste0(InSitu_dir,"/07_FieldCampaign17/02_Station/"),pattern=".csv",full.names=T)
lf_short<-list.files(paste0(InSitu_dir,"/07_FieldCampaign17/02_Station/"),pattern=".csv")

stats<-do.call(rbind,str_split(lf_short,"_"))[,1]
oi<-do.call(rbind,lapply(lf,read_csv))
oi$Date<-as.Date(sprintf("%06s",oi$Date),"%d%m%y")

aggr<-oi %>% group_by(FOI,Date,SubID)
aggr2<-ungroup(aggr)

write.csv(aggr2,file = paste0(MetricsDir,"/InSituMetrics.csv"))

