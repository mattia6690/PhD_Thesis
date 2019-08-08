
#' Function for Calculating Indices from spectrometer Data
#' Input is the spectrometer Data
#' Wavel1 is a c() of the two wavelength ranges to integrate and Wavel2 the second c() with Wavelength ranges
#' stat can be used to process predefined Spectral indices. More may be integrated in the function
hypindices<-function(input,wavel1,wavel2,stat="NDVI"){
  
  hyp1<- mean(input[between(input$Wavel,wavel1[1],wavel1[2]),]$Refl)
  hyp2<- mean(input[between(input$Wavel,wavel2[1],wavel2[2]),]$Refl)
  if(stat=="PRI")  hyp <- round((hyp1-hyp2)/(hyp1+hyp2),3)
  if(stat=="NDVI") hyp <- round((hyp2-hyp1)/(hyp2+hyp1),3)
  return(hyp)
  
}


# Here are some examples on how to compute NDVI and PRI indices
library("purrr")

dt<-hy.data$Data
PRI_1  <-map_dbl(dt,function(x) hypindices(x,c(530,534),c(568,572),stat="PRI"))
PRI_2  <-map_dbl(dt,function(x) hypindices(x,c(520,544),c(558,582),stat="PRI"))
NDVI_1 <-map_dbl(dt,function(x) hypindices(x,c(648,652),c(808,812),stat="NDVI"))
NDVI_2 <-map_dbl(dt,function(x) hypindices(x,c(638,662),c(798,822),stat="NDVI"))
NDVI_3 <-map_dbl(dt,function(x) hypindices(x,c(635,695),c(727,957),stat="NDVI"))
