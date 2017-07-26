# Initialize IT
install.packages("devtools");library(devtools)
install_bitbucket("mattia6690/mrfunctions")

loadandinstall("dplyr")
loadandinstall("ggplot2")
loadandinstall("lubridate")

setwd("C:/Users/MRossi/Documents/03_Data/01_RemSen/")

# Read IT
a<-read.csv("Orbits Sentinel_Landsat.csv",header=T)
b<-tidyr::gather(a)
b$value<-as.Date(b$value,format="%m/%d/%Y")

# Plot IT
g<-ggplot(b, aes(x=value,y=key))+theme_bw()+
  geom_point()+
  scale_x_date(date_breaks="1 week",date_labels = "%d-%m-%y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Date")+
  ylab("Sensor and Tile")+
  ggtitle("Scheduled Sentinel 2A and Landsat 8 OLI Overpass")

# Save IT
ggsave(g,filename = "Sen2A_Ls8_Schedules_2017.png",width=15,height=10)
