###########################'
source("R/BaseFunctions.R")


# Combine all the Information in one dataframe

lf<-list.files(paste0(dirfield,"/03_DaySOS"),pattern=".csv",full.names=T)
list1<-list()

for(i in 1:length(lf)) list1[[i]]<-read_csv(lf[i])

list2<-do.call(rbind,list1) %>% select(-X1)
write.csv(list2,file=paste0(MetricsDir,"/","CombinedInSituMetricsSOS.csv"))
saveRDS(list2,file=paste0(MetricsDir,"/","CombinedInSituMetricsSOS.rds"))


