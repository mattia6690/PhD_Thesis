
#################
# Required Functions and Dependencies
#################

library("devtools")
install_bitbucket("mattia6690/Mrfunctions");library(MRFunctions)
loadandinstall("readr")
loadandinstall("tidyverse")
loadandinstall("stringr")
loadandinstall("plotrix")
loadandinstall("gridExtra")
loadandinstall("grid")
loadandinstall("lattice")
loadandinstall("sf")

setwd("C:/Users/MRossi/Documents/03_Data/03_InSitu/")

# Standard Error Function
se<-function(x){se<-sd(x,na.rm=T)/length(which(!is.na(x)));return(se)}

unfac<-function(x){unf<-as.numeric(as.character(x));return(unf)}

hypindices<-function(input,wavel1,wavel2,stat="NDVI"){
  
  hyp1<- mean(input[between(input$Wavel,wavel1[1],wavel1[2]),]$Refl)
  hyp2<- mean(input[between(input$Wavel,wavel2[1],wavel2[2]),]$Refl)
  if(stat=="PRI")  hyp <- round((hyp1-hyp2)/(hyp1+hyp2),3)
  if(stat=="NDVI") hyp <- round((hyp2-hyp1)/(hyp2+hyp1),3)
  return(hyp)
  
}
