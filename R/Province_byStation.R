
# Load the climate Data
climate<-as.tibble(dt.clim)

# Structure
params<-climate %>% select(param) %>% unique
stations<-climate %>% select(st.id) %>% unique %>% unlist(use.names = F)

pd1<-climate %>% filter(st.id=="8512") %>% mutate(Station="Domef1500") %>% mutate(Region="Dolomites")
pd2<-climate %>% filter(st.id=="7350") %>% mutate(Station="Domef2000") %>% mutate(Region="Dolomites")
pd3<-climate %>% filter(st.id=="1580") %>% mutate(Station="Vimef2000") %>% mutate(Region="Venosta")
pd4<-climate %>% filter(st.id=="0250") %>% mutate(Station="Vimes1500") %>% mutate(Region="Venosta")
pd.bind<-rbind(pd1,pd2,pd3,pd4)

# Precipitation

prec.group<-pd.bind %>% filter(param=="pre") %>% group_by(year,Station,Region) %>% filter(year>1990)
prec.sum.yr<-prec.group %>% summarise(sumprec=sum(value),n=length(value)) %>% filter(n>360) %>%  ungroup
prec.sum.all <- prec.sum.yr %>% group_by(Station,Region) %>% summarise(sumprec=mean(sumprec)) 
prec.days <- pd.bind %>% group_by(Station,Region) %>% filter(year>1990) %>% dplyr::summarize(nod=sum(value>0))

ggplot(prec.sum.yr,aes(year,sumprec,linetype=Station,color=Station,pch=Region))+
  geom_line()+geom_point()+
  scale_color_manual(values=c("blue","cornflowerblue","firebrick","firebrick1"))+
  theme_bw()
  
# Temperatures

tmean.gr<-pd.bind %>% filter(param=="tmean") %>% group_by(year,Station,Region) %>% filter(year>1990)
tmean.yr<-tmean.gr %>% summarise(sumprec=mean(value),n=length(value)) %>% filter(sumprec>365) %>% ungroup
