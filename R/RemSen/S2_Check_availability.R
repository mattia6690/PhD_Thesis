# 1. Initialization ----
source("R/00_BaseFunctions.R")


# 2. List and Tidy ----


lf<-list.files(SAO_NDVIdir,recursive=T,pattern="tif")
df<-Create_availability_table_SA(lf,sen2names)

write.csv(df,paste0(RemSenFolder1,"Sentinel/Sentinel_2A/Availabe_SAO_Products_List.csv"))

df_prod<-df %>% select(matches("Product")) %>% unique %>% as.character
df_date<-df %>% filter(Platform=="S2A") %>% select(matches("AcqDate")) %>% 
  as.matrix %>% as.Date(.,"%Y%m%d") %>% table %>% data.frame %>% as.tibble %>% setNames(.,c("Date","Freq"))

# 2. Plot ----


g1<-ggplot(df_date,aes(y=Freq,x=as.Date(Date)))+
  geom_bar(stat="identity")+
  ggtitle(paste("Number of available Sentinel 2A",df_prod,"Scenes per Date"))+
  ylab("No acquisitions")+ xlab("Date")+
  scale_y_continuous(breaks=seq(1,10,1))

ggsave(g1,filename = paste0(RemSenFolder1,"/Sentinel_2A/Product_availability per date.png"))

