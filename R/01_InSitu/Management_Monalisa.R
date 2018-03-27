
source("R/BaseFunctions.R")

# Management of Vimes1500
station<-"vimes1500"
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

Manage<-cbind.data.frame(as.Date(Mgm),station,Mgm2)
colnames(Manage)<-c("Date","Station","Value")

saveRDS(Manage,file = paste0(DataDir,"/Management/",station,"_Management_2017.rds"))
