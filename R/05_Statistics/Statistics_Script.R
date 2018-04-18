##############################-
#####*** RANDOM FORESTS ***----
##############################-

##### Initialization ----

source("R/BaseFunctions.R")
station="vimes1500"
ndvi.tab<-readRDS(file = paste0(MetricsDir,"NDVI_Table_vimes1500_2017.rds"))
insitu_raw<-readRDS(paste0(MetricsDir,"CombinedInSituMetricsSOS.rds"))

laiwettab<-insitu_raw %>% 
  filter(Station==simpleCap(station)) %>% 
  filter(OP3=="LAI"|OP3=="BioWet") %>% 
  mutate(Date= .$Date %>% as.Date(format="%d%m%y")) %>% 
  group_by(Date,OP3) %>% 
  dplyr::summarize(Mean=mean(Value)) %>% 
  spread(OP3,Mean) %>% 
  ungroup

alltab4<-left_join(ndvi.tab,laiwettab) %>% 
  dplyr::distinct() %>% 
  rename(Spectrometer=Ground,Decagon=Monalisa)

##### Preprocessing ----
###* Input ----
Sensor<-c("LAI","BioWet")
Sensor.long<-c("Leaf Area","Wet Biomass")
Sensor.unit<-c("","(g)")
sensor<-rbind(Sensor,Sensor.long,Sensor.unit)

select<-sensor[,1]

###* Minimize NA ----
s2<-alltab4 %>% filter(!is.na(Sentinel))
fieldC<-alltab4 %>% filter(!is.na(get(sensor[1,1])),
                           !is.na(get(sensor[1,2])))

s2<-alltab4 %>% filter(!is.na(Sentinel))
pheno<-alltab4 %>% filter(!is.na(Phenocam))
spec<-alltab4 %>% filter(!is.na(Spectrometer))
deca<-alltab4 %>% filter(!is.na(Decagon))

fieldC<-addNear(s2,fieldC,col="Sentinel")
fieldC<-addNear(pheno,fieldC,col="Phenocam")
fieldC<-addNear(spec,fieldC,col="Spectrometer")
fieldC<-addNear(deca,fieldC,col="Decagon")

###* Generate Input Tables ----
statdat<-gather(fieldC,key=Scale2,value= y,Decagon, Sentinel, Phenocam, Spectrometer) %>% 
  select(Date,Scale2,y,eval(select[1])) %>% 
  setNames(c("Date","Scale2","y","x")) %>% 
  add_column(Scale1=select[1]) %>% 
  na.omit
ggdat<-statdat %>% select(-1) %>% dplyr::rename(.,Scale = Scale2)

#### Linear Regression ----
###* ggplot ----
gg.lin<-ggplot(ggdat,aes(x,y,color=Scale))+
  geom_point()+
  stat_smooth(method = "lm",se=F)+
  ylim(0.3,1)+
  ggtitle(paste("Correlation between",select[2],"and NDVI during the first growing period"))+
  ylab("NDVI Value")+
  xlab(paste(select[2],select[3]))

ggsave(gg.lin,
       filename = paste0(MetricsDir,"PolyCorrelation/Linear_Correlation_",select[1],"_gg.png"),
       device = "png")

###* Table ----
ggnest<-statdat %>% group_by(Scale1,Scale2) %>% nest
mp.lin<-ggnest$data %>% map(., function(i){
  
  y<-i$y
  x<-i$x
  reslm<-lm(y~x)
  r1<-round(rmse(reslm,i),4)    # RMSE
  r2<-round(rsquare(reslm,i),4) # R2
  r3<-round(coef(summary(reslm))[1,4],4)  # P-Value
  
  s1<-c("RMSE","R Squared","p-Value","N")
  s2<-c(r1,r2,r3,nrow(i))
  mt<-bind_cols(Stat=s1,Values=s2)
  return(mt)
  
})

ggnest$metrics<-mp.lin
stat.tab.quad <- ggnest %>% select(-data) %>% unnest %>% ungroup %>% spread(Stat,Values)
table.png(stat.tab.quad,
          name=paste0("Linear_Correlation_",select[1]),
          dir=paste0(MetricsDir,"PolyCorrelation/"),
          res=200)


#### Polynomial Regression ----
###* ggplot ----
gg.quad<-ggplot(ggdat,aes(x,y,color=Scale))+
  geom_point()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,se=F)+
  ylim(0.3,1)+
  ggtitle(paste("Correlation between",select[2],"and NDVI during the first growing period"))+
  ylab("NDVI Value")+
  xlab(paste(select[2],select[3]))

ggsave(gg.quad,
       filename = paste0(MetricsDir,"PolyCorrelation/Quadratic_Correlation_",select[1],"_gg.png"),
       device = "png")

###* Table ----
ggnest<-statdat %>% group_by(Scale1,Scale2) %>% nest
mp.quad<-ggnest$data %>% map(., function(i){
  
  y<-i$y
  x<-i$x
  reslm<-lm(y~I(x^2)*I(x))
  r1<-round(rmse(reslm,i),4)    # RMSE
  r2<-round(rsquare(reslm,i),4) # R2
  r3<-round(coef(summary(reslm))[1,4],4)  # P-Value
  
  s1<-c("RMSE","R Squared","p-Value","N")
  s2<-c(r1,r2,r3,nrow(i))
  mt<-bind_cols(Stat=s1,Values=s2)
  return(mt)
  
})

ggnest$metrics<-mp.quad
stat.tab.quad <- ggnest %>% select(-data) %>% unnest %>% ungroup %>% spread(Stat,Values)
table.png(stat.tab.quad,
          name=paste0("Quadratic_Correlation_",select[1]),
          dir=paste0(MetricsDir,"PolyCorrelation/"),
          res=200)

##### Random Forests ----
####* Input Generation ----

ground_resp<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP3==eval(select[1])) %>% 
  mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
  separate(OP1,c("OP11","OP12"),sep="_") %>% 
  group_by(Date,Station,Scale1,Scale2,OP11) %>% 
  dplyr::summarise(y=median(Value)) %>% ungroup %>% 
  select(-Scale2)

ground_ndvi<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP2=="NDVI") %>% 
  mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
  separate(OP1,c("OP11","OP12"),sep="_") %>% 
  group_by(Date,Station,Scale1,Scale2,OP11) %>% 
  dplyr::summarise(NDVI=median(Value)) %>% ungroup %>% 
  select(-Scale2)

###* Joins ----
join<-left_join(ground_resp,ground_ndvi)
statdat_rf<-fieldC %>% select(-LAI,-BioWet)
join2<-left_join(join,statdat_rf,by="Date") %>% filter(!is.na(Period)) %>% na.omit

###* Testing Data ----
set.seed(20)
smp<-sample(1:nrow(join2),10)
testset<-join2[smp,]
trainset<-join2[-smp,]

###* Random Forest ----

a<-tuneRF(select(trainset,c(6:10)),trainset$y,ntreeTry = 500,
          stepFactor = 2,plot=F)
a1<-a[which(a[,2]==min(a[,2])),]
mtry_val<-as.numeric(a1[1])

rf<-randomForest(y ~ Spectrometer+Decagon+Sentinel+Phenocam,
                 data = trainset,
                 importance=T,
                 mtry=1)

oob.df<-bind_cols(Predicted=predict(rf),
                         Original=trainset$y,
                         GrowPeriod=as.character(trainset$Period)) %>% 
  add_column(Dataset="OOB")
test.df<-bind_cols(Predicted=predict(rf,testset),
                          Original=testset$y,
                          GrowPeriod=as.character(testset$Period)) %>% 
  add_column(Dataset="Test")

all.df<-bind_rows(oob.df,test.df)

g2<-ggplot(all.df,aes(Predicted,Original))+
  geom_point()+
  geom_smooth(method = "lm",se=T)+
  ggtitle(paste("Random Forest Prediction vs. Observed",select[2]))

ggsave(g2,filename = paste0(MetricsDir,"RandomForest/Correlation_",select[1],"_gg.png"),device = "png")

r2.oob<-mean(rf$rsq)
rmse.oob<-rf$mse %>% sqrt %>% mean

r2.test<-test.df %>% select(c(Predicted,Original)) %>% lm %>% summary %>% .$r.squared
rmse.test<-test.df %>% select(c(Predicted,Original)) %>% rmse(lm(.),data=.)

rb<-rbind(c(r2.oob,rmse.oob),c(r2.test,rmse.test))
rownames(rb)<-c("OOB Dataset","Test Dataset")
colnames(rb)<-c("R Squared","RMSE")

table.png(rb,name=paste0("RF_Metrics_",select[1]),dir=paste0(MetricsDir,"RandomForest/"),res=200)


imp<-rf$importance
table.png(imp,name=paste0("Correlation_",select[1]),dir=paste0(MetricsDir,"RandomForest/"),res=200)


###* Growing Phases ----
g3<-ggplot(all.df,aes(Predicted,Original,color=GrowPeriod))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  ggtitle(paste("Random Forest Prediction vs. Observed",select[2]),"- by Growth Period")+
  theme(legend.position=c(.93,.12))

ggsave(g3,filename = paste0(MetricsDir,"RandomForest/Correlation_",select[1],"_ggPeriod.png"),device = "png")

gpmap<-map(1:3,function(i){
  
  grow.df<-all.df %>% filter(GrowPeriod==i) %>% select(c(Predicted,Original))
  r2<- grow.df %>% lm %>% summary %>% .$r.squared
  rmse<-grow.df %>% rmse(lm(.),data=.)
  
  c(i,r2,rmse)
})

grbind<-do.call(rbind.data.frame,gpmap) %>% setNames(c("Growth Period","R Squared","RMSE"))
table.png(grbind,name=paste0("RF_Metrics_GrowingPeriod_",select[1]),dir=paste0(MetricsDir,"RandomForest/"),res=200)


# Approximate the NAs

alltab5<-alltab4

alltab5$Sentinel <- zoo(alltab4$Sentinel,alltab5$Date) %>% na.approx %>% as.data.frame %>% .[["."]]
alltab5$Spectrometer <- zoo(alltab4$Spectrometer,alltab5$Date) %>% na.approx %>% as.data.frame %>% .[["."]]
alltab5$Phenocam <- zoo(alltab4$Phenocam,alltab5$Date) %>% na.approx %>% as.data.frame %>% .[["."]]
alltab5$Decagon <- zoo(alltab4$Decagon,alltab5$Date) %>% na.approx %>% as.data.frame %>% .[["."]]

zcS2 <- zoo(alltab5$Sentinel,alltab5$Date)
zcIS <- zoo(alltab5$Sentinel,alltab5$Date)



# map(1:3,function(i){
#   pred.df %>% filter(GrowPeriod==i) %>% lm(Predicted~Original,data=.) %>% summary})
# 

# 
# alltab2<-alltab[-(1:(min-10)),]
# alltab3<-alltab2 %>% select(Date,Monalisa) 
# 
# interp1<-loess(c(1:nrow(alltab3))~Monalisa,alltab3) %>% predict
# 
# fit.ex3 <- tslm(Monalisa ~ BioWet, data=as.ts(alltab2))
# 
# plot(Monalisa ~ BioWet, data=as.ts(alltab2))
# abline(fit.ex3)
# 
# ls<-loess(alltab$Monalisa~as.numeric(alltab$Date),span=.1) %>% predict()
# cbind.data.frame(alltab$Date,alltab$Monalisa,ls)

## Biomass Correlation OLD 
# s2<-alltab4 %>% filter(!is.na(Sentinel))
# fieldC<-alltab4 %>% filter(!is.na(BioWet))
# s2Date2Ground(s2,fieldC)
# 
# 
# dat1<-alltab4 %>% select(Monalisa,BioWet) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Monalisa",.before = T) %>% add_column(Scale1="WetBiomass",.before = T)
# dat2<-alltab4 %>% select(Ground,BioWet) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Ground",.before = T) %>% add_column(Scale1="WetBiomass",.before = T)
# dat3<-alltab4 %>% select(Phenocam,BioWet) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Phenocam",.before = T) %>% add_column(Scale1="WetBiomass",.before = T)
# dat4<-fieldC %>% select(Sentinel,BioWet) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Sentinel",.before = T) %>% add_column(Scale1="WetBiomass",.before = T)
# 
# statdat<-bind_rows(dat1,dat2,dat3,dat4)
# ggdat<-statdat %>% select(-1) %>% dplyr::rename(.,Scale = Scale2)
# 
# 
# g1<-ggplot(ggdat,aes(x,y,color=Scale))+
#   geom_point()+
#   stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,se=F)+
#   ylim(0.5,1)+
#   ggtitle("Correlation between Wet Biomass and NDVI during the first growing period")+
#   ylab("NDVI Value")+
#   xlab("Wet Biomass (g)")
# 
# ggsave(g1,filename = paste0(MetricsDir,"PolyCorrelation/Correlation_BioWet_gg.png"),device = "png")
# 
# ggnest<-statdat %>% group_by(Scale1,Scale2) %>% nest
# mp<-ggnest$data %>% map(., function(i){
#   
#   y<-i$y
#   x<-i$x
#   reslm<-lm(y~I(x^2)*I(x))
#   r1<-round(rmse(reslm,i),4)    # RMSE
#   r2<-round(rsquare(reslm,i),4) # R2
#   r3<-round(coef(summary(reslm))[1,4],4)  # P-Value
#   
#   s1<-c("RMSE","Multiple R Squared","p-Value","N")
#   s2<-c(r1,r2,r3,nrow(i))
#   mt<-bind_cols(Stat=s1,Values=s2)
#   return(mt)
#   
# })
# 
# ggnest$metrics<-mp
# stat.tab.bio <- ggnest %>% select(-data) %>% unnest %>% ungroup %>% spread(Stat,Values)
# table.png(stat.tab.bio,name="Correlation_BioWet",dir=paste0(MetricsDir,"PolyCorrelation/"),res=200)
# 

## Former Statdat

# dat1<-alltab4 %>% select(Monalisa,LAI) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Monalisa",.before = T) %>% add_column(Scale1=select[1],.before = T)
# dat2<-alltab4 %>% select(Ground,LAI) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Ground",.before = T) %>% add_column(Scale1=select[1],.before = T)
# dat3<-alltab4 %>% select(Phenocam,LAI) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Phenocam",.before = T) %>% add_column(Scale1=select[1],.before = T)
# dat4<-fieldC %>% select(Sentinel,LAI) %>% setNames(c("y","x")) %>% .[complete.cases(.),] %>% arrange(x) %>% 
#   add_column(Scale2="Sentinel",.before = T) %>% add_column(Scale1=select[1],.before = T)
# 
# statdat<-bind_rows(dat1,dat2,dat3,dat4)

# min<-c(min(which(!is.na(alltab$Monalisa))),
#        min(which(!is.na(alltab$Sentinel))),
#        min(which(!is.na(alltab$Phenocam))),
#        min(which(!is.na(alltab$Ground)))) %>% max()
# 

