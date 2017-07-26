###########################
#' 00. In Situ Plot 1 Script
#'
#' This script allows plot the outputs of the InSituReader.R script in different ways:
#' * reprentation of the GPS values
#' * 
#' 
###########################


### 01. Plots 2 GPS ####

load("07_FieldCampaign17/02_Station/GPS.RData") # gps

gps %>% filter(Stat=="Domef1500")