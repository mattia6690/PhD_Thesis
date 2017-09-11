# 
# # Program for the Analysis of Provence Stations
# loadandinstall("rgdal")
# loadandinstall("raster")
# loadandinstall("ggmap")
# loadandinstall("ggplot2")
# loadandinstall("rgeos")
# loadandinstall("gridExtra")
# 
# lf<-list.files("Stations/",full.names=T,pattern=".csv")
# 
# statvz<-read.csv(lf[1])
# paravz<-read.csv(lf[2])
# 
# params_interesting<-c("LT","Ta","N")
# earlyest_end<-"01/01/2017 00:00"
# start       <-"01/01/2014 00:00"
# 
# # Extract interesting stations
# s1<-statvz[statvz$PARAMETER%in%params_interesting,] # by parameters
# s2<-s1[which(as.POSIXct(strptime(s1$DATEN_BIS, "%m/%d/%Y %H:%M"))>
#                as.POSIXct(strptime(earlyest_end, "%m/%d/%Y %H:%M"))),] # By Date
# 
# s2$DATEN_AB<-start
# s2$DATEN_BIS<-format(Sys.time(),"%m/%d/%Y %H:%M")
# 
# proj1="+proj=utm +zone=32 +north +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
# coords<-cbind(s2$X_UTM,s2$Y_UTM)
# sp<-SpatialPointsDataFrame(coords=coords,data=s2,proj4string = CRS(proj1))
# 
# grass<-readOGR("Stations/MONALISA","MONALISA_grasslands")
# grass<-grass[1:10,]
# orch <-readOGR("Stations/MONALISA","MONALISA_orchards")
# bound<-readOGR("Stations/MONALISA","SouthTyrol")
# sp2<-spTransform(sp,CRS=projection(grass))
# 
# 
# .STMeteo_buffer(grass)
# 
# 
# g_buff<-gBuffer(spTransform(grass,CRS=proj1),width = 7000)
# g_buff_ll<-spTransform(g_buff,CRS=projection(grass))
# 
# o_buff<-gBuffer(spTransform(orch,CRS=proj1),width = 7000)
# o_buff_ll<-spTransform(o_buff,CRS=projection(grass))
# 
# 
# plot(bound,axes=T,col="bisque1",border="bisque4",
#      bg="gray97",main="The Distribution of Provence and MONALISA Stations")
# #plot(sp2,pch=17,col="darkred",add=T)
# plot(grass,pch=16,col="darkgreen",add=T)
# #plot(orch,pch=16,col="violet",add=T)
# plot(g_buff_ll,pch=1,border="darkgreen",add=T)
# #plot(o_buff_ll,pch=1,border="violet",add=T)
# 
# g_sp2_in<-sp2[which(over(sp2,g_buff_ll)==1),]
# owh1<-which(g_sp2_in@data$HOEHE<1000)
# owh2<-which(g_sp2_in@data$HOEHE>2600)
# owh<-c(owh1,owh2)
# g_sp2_in<-g_sp2_in[-owh,]
# #o_sp2_in<-sp2[which(over(sp2,o_buff_ll)==1),]
# 
# legend("bottomright",c("MONALISA Grassland","7km Buffer","Province Stations (Operational)"),
#        pch=c(16,1,17),col=c("darkgreen","darkgreen","darkred"))
# 
# plot(g_sp2_in,pch=17,col="darkred",add=T)
# #plot(o_sp2_in,pch=15,add=T)
# 
# write.csv(g_sp2_in,"Stations/Result/Green_Stations3.csv")
# 
# 
# ##### WHISKI DATA #### STATION PROVENCE ###
# load("Stations/clim series wiski v2 - daily.rda") # dt.clim
# 
# wh_pre <-which(dt.clim$param=="pre")
# wh_temp<-which(dt.clim$param=="tmean")
# wh_all <-c(wh_pre,wh_temp)
# dt.clim_pre<-dt.clim[wh_all,]
# 
# start<-"2014-01-01"
# end  <-"2017-01-01"
# 
# g_sp2_nr<-as.character(unique(g_sp2_in$NUMMER))
# g_sp2_nr_s<-substr(g_sp2_nr,1,4)
# 
# sta<-g_sp2_nr_s[g_sp2_nr_s%in%dt.clim_pre$st.id]
# 
# dt.clim_pre1<-dt.clim_pre[which(dt.clim_pre$st.id%in%sta),]  # Extract rel Stations
# dt.clim_pre2<-dt.clim_pre1[-which(dt.clim_pre1$date<start),] # Extract rel Dates
# 
# unq<-sort(unique(dt.clim_pre2$st.id))
# unq2<-g_sp2_nr[which(g_sp2_nr_s%in%unq)]
# unq_sD<-unique(g_sp2$STATION_D)
# unq_sI<-unique(g_sp2$STATION_I)
# 
# for (i in 1:length(unq)){
#   
#   ä<-which(dt.clim_pre2$st.id==unq[i])
#   dt.clim_pre2$st.id[ä]<-unq2[i]
#   
# }
# 
# write.csv(dt.clim_pre2,"Stations/Result/ClimateVariables_ProvinceST_MONALISA_20142017.csv")


# 
# ggplot()+
#   geom_polygon(data=bound,aes(x=long,y=lat,group=group))+
#   geom_point(data=sp2,aes(x=longitude,y=latitude),color="darkred")




# 
# for(i in 1:length(unq)){
#   
#   wh1<-which(dt.clim_pre2$st.id==unq[i])
#   itab<-dt.clim_pre2[wh1,]
#   
#   byear_stat <- itab %>% 
#     group_by(year,month) %>% 
#     summarise(mean=mean(value),sd=sd(value),min=min(value),max=max(value),
#               sum=sum(value),count=n())
#   
#   byear_stat <-round(byear_stat,2)
#   
#   pt<-tableGrob(byear_stat)
#   
#   g1<-ggplot(itab,aes(date,value))+
#     geom_line()
#   
#   grid.arrange(g1,pt,as.table=T,nrow=2)
#   
# }
# 
# 
# 
# 
# 
