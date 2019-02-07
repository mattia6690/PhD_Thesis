
library(sf)
library(dplyr)
library(magrittr)

tempdir<-"C:/Users/MRossi/Documents/Temp/"

lac<-st_read("C:/Users/MRossi/Documents/03_Data/Shapes/00_General","AC-perimeter_ifuplan_16072007")
tilefile<-paste0(tempdir,"S2_Tiles.kml")
Tiles<-st_read(tilefile)
lac2<-st_transform(lac,as.character(st_crs(Tiles)[2]))
Tiles2<-st_bind_cols(Tile=Tiles[[1]],Tiles[[3]])
inter1<-st_intersection(lac2,Tiles2)
inter2<-inter1 %>% group_by(Tile) %>% dplyr::summarize() %>% st_zm

outname<-paste0(tempdir,"S2_Tiles_lac_v2.shp")
st_write(inter2,outname,driver = "ESRI Shapefile")

colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))
tilesplot<-inter2 %>% select(Tile) %>% arrange(Tile)
plot(tilesplot,axes=T,
     col = colfunc(length(tilesplot$Tile)), 
     key.pos = 1,
     main="Sentinel 2 Tiles in the Large Alpine Convention")


