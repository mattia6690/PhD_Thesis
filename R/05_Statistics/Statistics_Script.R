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

saveRDS(alltab4,file = paste0(MetricsDir,"MetricsTabAll.rds"))

##### Preprocessing ----
###* Input ----
Sensor<-c("LAI","BioWet")
Sensor.long<-c("Leaf Area","Wet Biomass")

x1 = bquote(.(Sensor.long[1])~"("~m^{2}~"*"~m^{-2}~")")
x2 = bquote(.(Sensor.long[2])~"(g)")
Sensor.unit<-c(x1,x2)

sensor<-list()
sensor[[1]]<-list(S1<-Sensor[1],S2<-Sensor.long[1],S3<-Sensor.unit[1])
sensor[[2]]<-list(S1<-Sensor[2],S2<-Sensor.long[2],S3<-Sensor.unit[2])

#sensor<-rbind(Sensor,Sensor.long,Sensor.unit)


###* Minimize NA ----
s2<-alltab4 %>% filter(!is.na(Sentinel))
fieldC<-alltab4 %>% filter(!is.na(get(Sensor[1])),
                           !is.na(get(Sensor[2])))

s2<-alltab4 %>% filter(!is.na(Sentinel))
pheno<-alltab4 %>% filter(!is.na(Phenocam))
spec<-alltab4 %>% filter(!is.na(Spectrometer))
deca<-alltab4 %>% filter(!is.na(Decagon))

fieldC<-addNear(s2,fieldC,col="Sentinel")
fieldC<-addNear(pheno,fieldC,col="Phenocam")
fieldC<-addNear(spec,fieldC,col="Spectrometer")
fieldC<-addNear(deca,fieldC,col="Decagon")

saveRDS(fieldC,file = paste0(MetricsDir,"MetricsJonedNear.rds"))

cortable<-list()
for(i in 1:length(sensor)){
  
  select<-sensor[[i]]
  xlabs<-select[[3]][[1]]
  
  ###* Generate Input Tables ----
  fieldC2<- fieldC %>% 
    setNames(c("Date","Decagon SRS","Sentinel-2 MSI","Phenocam","Spectrometer",
               "Growing Period","BioWet","LAI"))
  
  statdat<-gather(fieldC2,key=Scale2,value= y,"Decagon SRS", "Sentinel-2 MSI", Phenocam, Spectrometer) %>% 
    select(Date,Scale2,y,eval(select[[1]])) %>% 
    setNames(c("Date","Scale2","y","x")) %>% 
    add_column(Scale1=select[[1]]) %>% 
    na.omit
  ggdat<-statdat %>% select(-1) %>% dplyr::rename(.,Scale = Scale2)
  
  #### Linear Regression ----
  ###* ggplot ----
  gg.lin<-ggplot(ggdat,aes(x,y,color=Scale))+
    geom_point()+
    stat_smooth(method = "lm",se=F)+
    ylim(0.3,1)+
    ggtitle(paste("Correlation between",select[[2]],"and NDVI during the first growing period"))+
    labs(y="NDVI Value",x=xlabs)
  
  ggsave(gg.lin,
         filename = paste0(MetricsDir,"PolyCorrelation/Linear_Correlation_",select[[1]],"_gg.png"),
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
  stat.tab.lin <- ggnest %>% select(-data) %>% unnest %>% ungroup %>% spread(Stat,Values)
  stat.tab.lin$Regression<-"Linear"
  table.png(stat.tab.lin,
            name=paste0("Linear_Correlation_",select[[1]]),
            dir=paste0(MetricsDir,"PolyCorrelation/"),
            res=200)
  
  
  #### Polynomial Regression ----
  ###* ggplot ----
  gg.quad<-ggplot(ggdat,aes(x,y,color=Scale))+
    geom_point()+
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1,se=F)+
    ylim(0.3,1)+
    ggtitle(paste("Correlation between",select[[2]],"and NDVI during the first growing period"))+
    labs(y="NDVI Value",x=xlabs)
  
  ggsave(gg.quad,
         filename = paste0(MetricsDir,"PolyCorrelation/Quadratic_Correlation_",select[[1]],"_gg.png"),
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
  stat.tab.quad$Regression<-"Quadratic"
  table.png(stat.tab.quad,
            name=paste0("Quadratic_Correlation_",select[[1]]),
            dir=paste0(MetricsDir,"PolyCorrelation/"),
            res=200)
  
  ##### Regression Combination ----
  
  d1<-paste0(MetricsDir,"PolyCorrelation/FacetPlots/")
  
  gg.all<-ggplot(ggdat,aes(x,y))+ theme_bw()+
    facet_wrap(~Scale)+
    geom_point(pch=2)+
    stat_smooth(method = "lm",se=F,color="black",linetype=2)+
    stat_smooth(method = "lm",formula = y ~ x + I(x^2), size = 1,se=F,color="black",linetype=3)+
    ylim(0,1)+
    labs(y="NDVI Value",x=xlabs)
  
  ggsave(gg.all,
         filename = paste0(d1,"Complete_Correlation_",select[[1]],"_gg_facet.png"),
         device = "png")
  
  
  # Unique Table
  
  cortable[[i]]<-rbind(stat.tab.lin,stat.tab.quad)
}

cortable.comb<-do.call(rbind,cortable)%>% 
  setNames(c("Response","Input","N","PValue","R2","RMSE","Regression Type"))

table.png(cortable.comb,
          name=paste0("Complete_Regression_Table"),
          dir=paste0(MetricsDir,"PolyCorrelation/"),
          res=200)

##### Random Forests ----
####* Input Generation ----

ground_ndvi<-insitu_raw %>% 
  filter(Station=="Vimes1500") %>% 
  filter(OP2=="NDVI") %>% 
  mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
  separate(OP1,c("OP11","OP12"),sep="_") %>% 
  group_by(Date,Station,Scale1,Scale2,OP11) %>% 
  dplyr::summarise(NDVI=median(Value)) %>% ungroup %>% 
  select(-Scale2)

for(i in 1:length(sensor)){

  select<-sensor[[i]]
  
  ground_resp<-insitu_raw %>% 
    filter(Station=="Vimes1500") %>% 
    filter(OP3==eval(select[[1]])) %>% 
    mutate(Date=as.Date(Date,format="%d%m%y")) %>% 
    separate(OP1,c("OP11","OP12"),sep="_") %>% 
    group_by(Date,Station,Scale1,Scale2,OP11) %>% 
    dplyr::summarise(y=median(Value)) %>% ungroup %>% 
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
    add_column(Dataset="Out-of-Bag Estimation")
  
  test.df<-bind_cols(Predicted=predict(rf,testset),
                     Original=testset$y,
                     GrowPeriod=as.character(testset$Period)) %>% 
    add_column(Dataset="Test Dataset Estimation")
  
  all.df<-bind_rows(oob.df,test.df)
  
  all.df2<-all.df %>% select(-GrowPeriod)
  g2<-ggplot(all.df,aes(Predicted,Original))+ theme_bw()+
    facet_wrap(~Dataset)+
    geom_point()+
    geom_smooth(method = "lm",se=T,fill="gray80",linetype=2)+
    ggtitle(paste("Random Forest Predicted vs. Observed",select[2]))
  
  ggsave(g2,filename = paste0(MetricsDir,"RandomForest/Correlation_",select[1],"_gg2.png"),device = "png")
  
  r2.oob<-mean(rf$rsq)
  rmse.oob<-rf$mse %>% sqrt %>% mean
  
  r2.test<-test.df %>% select(c(Predicted,Original)) %>% lm %>% summary %>% .$r.squared
  rmse.test<-test.df %>% select(c(Predicted,Original)) %>% rmse(lm(.),data=.)
  
  rb<-rbind(c(r2.oob,rmse.oob),c(r2.test,rmse.test))
  rownames(rb)<-c("OOB Dataset","Test Dataset")
  colnames(rb)<-c("R Squared","RMSE")
  
  table.png(rb,name=paste0("RF_Metrics_",select[1],"_combined"),dir=paste0(MetricsDir,"RandomForest/"),res=200)
  
  
  imp<-rf$importance
  table.png(imp,name=paste0("Correlation_",select[1],"_combined"),dir=paste0(MetricsDir,"RandomForest/"),res=200)
  
}

imp2<-importance(rf)%>% 
  as.data.frame %>% 
  rownames_to_column(.) %>% 
  setNames(c("Sensor","incMSE","NodePurity")) %>% 
  as.tibble %>%
  gather(key=Test,value= Value,incMSE,NodePurity,-Sensor)

gg.i1<-ggplot(imp2 %>% filter(Test=="incMSE"),aes(y=Sensor,x=Value))+ 
  theme_bw()+ geom_point()+
  ggtitle("%incMSE")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Residual Sum of Squares")+
  xlab("MSE increase after permutation")

gg.i2<-ggplot(imp2 %>% filter(Test=="NodePurity"),aes(y=Sensor,x=Value))+ 
  theme_bw()+ geom_point()+
  ggtitle("Node impurity")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Residual Sum of Squares")
grid.arrange(gg.i1, gg.i2, ncol=2)

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


