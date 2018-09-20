
source("R/BaseFunctions.R")

# Management of Vimes1500
station<-"Vimes1500"
Mgm<-c("2017-03-23","2017-03-26","2017-03-31",
       "2017-04-07","2017-04-14","2017-04-20",
       "2017-05-18","2017-05-29",
       "2017-06-11","2017-06-23",
       "2017-07-03",
       "2017-09-04","2017-09-07","2017-09-18",
       "2017-10-15")
Mgm2<-c("Livestock","Livestock","Livestock",
        "Fertilization","Fertilization","Livestock",
        "Irrigation","Irrigation",
        "Irrigation","Irrigation",
        "Harvest",
        "Harvest","Hay","Harvest",
        "Livestock")

Manage1<-cbind.data.frame(as.Date(Mgm),station,Mgm2)
colnames(Manage1)<-c("Date","Station","Value")


station<-"Domef1500"
Mgm<-c("2017-06-18","2017-06-21","2017-08-23","2017-08-25")
Mgm2<-c("Harvest","Harvest","Harvest","Harvest")

Manage2<-cbind.data.frame(as.Date(Mgm),station,Mgm2)
colnames(Manage2)<-c("Date","Station","Value")

station<-"Vimef2000"
Mgm<-c("2017-07-06","2017-08-02","2017-08-13","2017-09-21","2017-09-26")
Mgm2<-c("Harvest","Harvest","Harvest","Harvest","Harvest")

Manage3<-cbind.data.frame(as.Date(Mgm),station,Mgm2)
colnames(Manage3)<-c("Date","Station","Value")

station<-"Domef2000"
Mgm<-c("2017-08-08","2017-08-09")
Mgm2<-c("Harvest","Harvest")

Manage4<-cbind.data.frame(as.Date(Mgm),station,Mgm2)
colnames(Manage4)<-c("Date","Station","Value")

Manage.all<-rbind(Manage1,Manage2,Manage3,Manage4)
saveRDS(Manage.all,file = paste0(DataDir,"/Management/Complete_Management_2017.rds"))



# Management from Phenocam Evaluation

loc<-"C:/Users/MRossi/Documents/03_Data/Management/VIMES1500_OccurencesPhenoCam_2017.xlsx"
sheets<-loc %>%
  excel_sheets %>% as.tibble %>% 
  mutate(Management=map(value,function(x,l=loc) read_xlsx(l,sheet=x))) %>% unnest

saveRDS(sheets,file = paste0(DataDir,"/Management/Complete_Management_2017.rds"))




