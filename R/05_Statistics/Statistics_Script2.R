
alltab4<-readRDS(file = paste0(MetricsDir,"MetricsTabAll.rds"))

#### 1.Interpolation ----

####* Input ----
sent <- zoo(alltab4$Sentinel,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","Sentinel")) %>% 
  mutate(Date=as_date(Date))
spec <- zoo(alltab4$Spectrometer,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","Spectrometer")) %>% 
  mutate(Date=as_date(Date))
phen <- zoo(alltab4$Phenocam,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","Phenocam")) %>% 
  mutate(Date=as_date(Date))
deca <- zoo(alltab4$Decagon,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","Decagon")) %>% 
  mutate(Date=as_date(Date))

fj.input<-full_join(sent,spec,by="Date") %>% 
  full_join(.,phen,by="Date") %>% 
  full_join(.,deca,by="Date")

fj2.input<-fj.input %>% gather(key=Scale,Value,2:5)

alltab.input <- alltab4 %>% gather(key=Scale,Value,2:5)

ggplot(fj2.input,aes(Date,Value))+geom_line()+
  geom_point(data=alltab5.input,aes(Date,Value))+
  facet_grid(Scale~.)

####* Response ----

predLAI<-deca <- zoo(alltab4$LAI,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","LAI")) %>% 
  mutate(Date=as_date(Date))

predBio<-deca <- zoo(alltab4$BioWet,alltab4$Date) %>% 
  na.approx %>% 
  as.tibble %>% 
  rownames_to_column() %>% 
  setNames(c("Date","BioWet")) %>% 
  mutate(Date=as_date(Date))

fj.response<-full_join(predLAI,predBio,by="Date")

ggplot(fj2.input,aes(Date,Value))+geom_line()+
  geom_point(data=alltab5.input,aes(Date,Value))+
  facet_grid(Scale~.)
