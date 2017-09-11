
# Raster Transformation

rasterNA<-function(scene,range,proc.time=F){
  
  start<-Sys.time()
  r1<- raster(scene)
  t5<- values(r1)
  wh_ras<-which(t5<range[1]|t5>range[2])
  t5[wh_ras]<-NA
  r2<-setValues(r1,as.numeric(t5))
  rm(r1)
  end<-Sys.time()
  if(proc.time==T) print(end-start)
  
  return(r2)

}