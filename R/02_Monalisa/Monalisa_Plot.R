


savePlot==T
lf_deca<-readRDS(lf_deca,paste0(MetricsDir,"Monalisa_NDVI.rds"))

#* Plot the Raw Decagon NDVIs ----

g1<-ggplot(lf_deca,aes(x=Date,y=Value))+
  geom_point()+
  ggtitle("Decagon NDVI of 2017 - Unfiltered")+
  facet_grid(Station~.)

name.g1<-paste0(Monalisa17Dir,"Decagon_NDVI_2017_Unfiltered_All.png")
if(savePlot==T) ggsave(g1, filename = name.g1, device = "png", width = 7, height=11)


# NDVI at Daytime ----

time<-"1030"
time.intable<-paste0(hour(lf_deca$Date),minute(lf_deca$Date))
wh<-which(time.intable==time)
lf.fil.time<-lf_deca %>% slice(wh)


g2<-ggplot(lf.fil.time,aes(x=Date,y=Value))+
  geom_point()+
  ggtitle(paste0("Decagon NDVI of 2017 at ",time))+
  facet_grid(Station~.)

name.g2<-paste0(Monalisa17Dir,"Decagon_NDVI_2017_at,",time,",.png")
if(savePlot==T) ggsave(g2, filename = name.g2, device = "png", width = 7, height=11)


# No. Acquisitions ----

lf_acquisitions<-lf_deca
lf_acquisitions$Date<-as_date(lf_acquisitions$Date)
lf_acquisitions<-lf_acquisitions %>%
  dplyr::count(Date,Station) %>%
  mutate(prop = prop.table(n))

g3<-ggplot(lf_acquisitions,aes(x=Date,y=n))+
  geom_area(fill="grey53",color="gray29")+
  ggtitle(paste0("Decagon Acquisitions per Day 2017"))+
  ylab("Number of Acquisitions")+
  facet_grid(Station~.)

name.g3<- paste0(Monalisa17Dir,"Decagon_Acquisitions_perDay.png")
if(savePlot==T) ggsave(g3,filename = name.g3, device = "png", width = 7, height=11)

# Five Dates

dates.day<-list(c(1:5),c(91:95),c(198:202),c(274:278))

times<-lf6 %>% separate(.,Date,c("Date","Time")," ") %>% select(Time) %>% unlist %>% as.hms() 
lf10<-lf8 %>% add_column(Time=times,.before = T) %>% group_by(Date) %>% nest %>% slice(dates.day[[4]]) %>% unnest

f<-min(lf10$Date) %>% str_replace_all(.,"-","")
t<-max(lf10$Date) %>% str_replace_all(.,"-","")

lf10$Date<-as.character(lf10$Date)
g4<-ggplot(lf10,aes(x=Time,y=Value,color=Date))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  scale_x_time(limits=c(3*3600,18*3600))+
  facet_grid(Station~.)

ggsave(g4,filename = paste0(Monalisa17Dir,"Decagon_NDVI_",f,"_to_",t,".png"),device = "png",width = 7,height=11)



step1<-filter.data1 
step2<-filter.data2

step11<-step1 %>%  group_by(Date) %>% nest %>% slice(dates.day[[2]]) %>% unnest
step21<-step1 %>%  group_by(Date) %>% nest %>% slice(dates.day[[2]]) %>% unnest

f<-min(step11$Date) %>% str_replace_all(.,"-","")
t<-max(step11$Date) %>% str_replace_all(.,"-","")

g4<-ggplot(step11,aes(x=Time,y=Value,color=Date))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  scale_x_time(limits=c(3*3600,18*3600))+
  facet_grid(Station~.)

g5<-ggplot(step21,aes(x=Time,y=Value,color=Date))+
  geom_point()+
  ggtitle(paste0("DECAGON NDVI from ",f," to ",t))+
  ylim(c(-1,1))+
  scale_x_time(limits=c(3*3600,18*3600))+
  facet_grid(Station~.)

ggsave(g4,filename = paste0(Monalisa17Dir,"Decagon_NDVI_",f,"_to_",t,"Filter1.png"),device = "png",width = 7,height=11)
ggsave(g5,filename = paste0(Monalisa17Dir,"Decagon_NDVI_",f,"_to_",t,"Filter2.png"),device = "png",width = 7,height=11)



