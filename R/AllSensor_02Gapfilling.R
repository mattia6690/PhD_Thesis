
# Input -------------------------------------------------------------------
source("R/BaseFunctions.R")
library("imputeTS")

suffix<-"110119"
suffix.out<- "241019"
subfolder<-"01_Combination"
indir<-paste0(MetricsDir,subfolder,"/")

db     <-readRDS(paste0(indir,"AllSensorData",suffix,".rds"))
db.all <-readRDS(paste0(indir,"AllSensorData",suffix,"_filtered.rds"))
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
    
    df1 <-as.data.frame(i) %>% 
      dplyr::select(Date,Value) %>% 
      group_by(Date) %>% 
      dplyr::summarize(Value=mean(Value)) %>% 
      ungroup
    
    tsib<-as_tsibble(df1,index="Date") %>% 
      fill_gaps() %>% 
      mutate(Approach=map_chr(Value,function(x) {ifelse(is.na(x),"Interpolated","Original")})) %>% 
      mutate(Linear=na_interpolation(Value,"linear")) %>% 
      mutate(Spline=na_interpolation(Value,"spline")) %>% 
      mutate(Stineman=na_interpolation(Value,"stine"))
      
      
    return(as_tibble(tsib))
    
  }))
# gapfill.db<-gpdb.ts %>% 
#   dplyr::select(-data) %>% unnest %>% 
#   dplyr::select(Date,Station,Plot,Scale1,Sensor,OP2,Value) %>% 
#   filter((Station!="Vimes1500") & (Date>"2017-07-17" | Date<"2017-07-03"))

gapfill.db<-gpdb.ts %>% 
  dplyr::select(-data) %>% 
  unnest %>% 
  group_by(Date,Station,Scale1,Sensor,OP2) %>% 
  dplyr::summarize(Original=mean(Value),
            Linear=mean(Linear),
            Spline=mean(Spline),
            Stineman=mean(Stineman))


testFF<-gapfill.db %>% tidyr::gather(key = "Interpolation",value="InterpolationValue",Linear,Spline,Stineman)

ggplot(testFF,aes(Date,InterpolationValue))+
  facet_grid(vars(Station),vars(Sensor))+
  geom_line(aes(col=Interpolation))+
  geom_point(aes(Date,Original))


dplyr::select(Date,Station,Plot,Scale1,Sensor,OP2,Approach,Value) %>% 
  mutate(Value=as.numeric(Value))



# wh<-which(gapfill.db$Station=="Vimes1500" & gapfill.db$Date>"2017-07-03" & gapfill.db$Date<"2017-07-17")
# gapfill.db<-slice(gapfill.db,-wh)







saveRDS(gapfill.db,file = paste0(outdir,"AllSensorData",suffix.out,"_gapFill.rds"))


ts1<-ts(df1)
ces(ts1)



# Plot --------------------------------------------------------------------

# Plot the Interpolated Time series

ggplot(gapfill.db,aes(Date,Value,col=Interpolated)) +
  facet_wrap(.~Sensor)+
  geom_point()





