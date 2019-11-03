# Drought Computation Alpine Environment

headRows<-3
timecol <-1
startcol<-2
headNames<-c("Key","Unit","Statistic")

b2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/05_SWC/B2_2017-2018.csv",skip = 1,col_names = F)
p2<-read_csv("C:/Users/MRossi/Documents/03_Data/03_InSitu/05_SWC/P2_2017-2018.csv",skip = 1,col_names = F)

t1<-readSWCAlpEnv(b2,Station = "Vimes1500",
                  headRows = 3, headNames = c("Key","Unit","Statistic"),
                  TimeCol = 1, StartCol = 2)

t2<-readSWCAlpEnv(p2,Station = "P2",
                  headRows = 3, headNames = c("Key","Unit","Statistic"),
                  TimeCol = 1, StartCol = 2)

swc.rect<-tibble(x1=c(as.Date("2017-04-01"),as.Date("2018-04-01")),
                 x2=c(as.Date("2017-10-01"),as.Date("2018-10-01")),
                 y1=c(0,0),
                 y2=c(.5,.5))



complete<-rbind(t1,t2)

atib<-complete %>% 
  mutate(Date=date(TimeStamp)) %>% 
  mutate(Year=year(Date)) %>% 
  mutate(Month=month(Date)) %>% 
  mutate(Day=day(Date)) %>% 
  mutate(Value=as.numeric(Value))

atib.gr1<- atib %>% 
  group_by(Date,Station,Key,Statistic, Unit) %>% 
  summarize(Mean=mean(Value,na.rm=T),
            Max=max(Value),
            Min=min(Value),
            Stdev=sd(Value),
            Sum=sum(Value,na.rm=T)) %>% 
  ungroup

atib.all<- atib.gr1 %>% 
  mutate(MetricS=lapply(Key,function(x){
    
    sp <- strsplit(x,"_")[[1]]
    bind_cols(MetricS = sp[1], Plot = sp[3], Depth = paste(sp[4],"cm"), Statistic = sp[5])
    
  })) %>% 
  unnest %>% 
  mutate(MetricL=sapply(MetricS,function(x) ifelse(x=="SWC","Soil Water Content","Soil Water Potential")))



# Plot both Soil Water Content and Soil Water Potential not in the same plot
# Soil Water Content
swc.tab<-atib.all %>% 
  filter(MetricS=="SWC" & Statistic=="Avg") %>% 
  filter(Depth!="50 cm")
ylab<-paste(unique(swc.tab$MetricL),"in",unique(swc.tab$Unit))

swc.tab.week<- swc.tab %>% mutate(Week=paste(year(Date),"-",week(Date)))

# Daily Interpolated
gg.swc<-ggplot()+ theme_light() +
  facet_grid(vars(Station),vars(Depth))+
  geom_rect(data=swc.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill=NA,col="black")+
  ylab(ylab)+
  labs(title="Soil Water Content in the Research Area",
       subtitle= "Spline Interpolated Time Series 2017 and 2018")+
  geom_smooth(data=swc.tab,aes(Date,Mean,col=Plot,lty=Plot),method = lm, formula = y ~ splines::bs(x, 35), se = FALSE)+
  scale_x_date(breaks = "2 months", date_labels= "%Y-%m")+
  ylim(c(0,0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(gg.swc,filename="Paper2/SWCplot_Spline35.png",device="png",height=6,width=12,units="in")


gg.swc<-ggplot()+ theme_light() +
  geom_line(data=swc.tab,aes(Date,Mean,col=Plot,lty=Plot))+
  facet_grid(vars(Station),vars(Depth))+
  geom_rect(data=swc.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill=NA,col="black")+
  ylab(ylab)+
  labs(title="Soil Water Content in the Research Area",
       subtitle= "Spline Interpolated Time Series 2017 and 2018")+
  scale_x_date(breaks = "2 months", date_labels= "%Y-%m")+
  ylim(c(0,0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(gg.swc,filename="Paper2/SWCplot_Lines.png",device="png",height=6,width=12,units="in")





# Delete 20cm Vimes1500 & Mean --------------------------------------------



swc.tab.v2<-swc.tab %>% 
  filter(!(Station=="Vimes1500" & Depth=="20 cm" & Plot=="C")) %>% 
  group_by(Date,Station,Statistic,Unit,MetricS,Depth) %>% 
  dplyr::summarize(Mean=mean(Mean,na.rm=T))

saveRDS(swc.tab.v2,"Paper2/SWC_daily.rds")

swc.interpol<- swc.tab.v2 %>% 
  group_by(Station,Depth) %>% 
  nest

gg.swc2<-ggplot()+ theme_light() +
  geom_rect(data=swc.rect,aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2),fill="grey80",col=NA,alpha=.6)+
  geom_point(data=swc.tab.v2,aes(Date,Mean),alpha=.1)+
  geom_smooth(data=swc.tab.v2,aes(Date,Mean),method = lm, formula = y ~ splines::bs(x, 35), se = FALSE)+
  facet_grid(vars(Station),vars(Depth))+
  ylab(ylab)+
  labs(title="Soil Water Content in the Research Area",
       subtitle= "Spline Interpolated Time Series 2017 and 2018")+
  scale_x_date(breaks = "2 months", date_labels= "%Y-%m")+
  ylim(c(0,0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(gg.swc2,filename="Paper2/SWCplot_Lines_day.png",device="png",height=6,width=12,units="in")



