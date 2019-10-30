
library("magrittr")
library("dplyr")

c1<-c(-3.5,seq(-2,2,by=0.5)) %>% .[-which(.>-1 & .<1)]
c2<-c(seq(-2,2,by=0.5),3.5) %>% .[-which(.>-1 & .<1)]
c3<-c("extremely wet","very wet","moderately wet","near normal","moderately dry","very dry","extremely dry")

cols<-c("red","orange","yellow","white","seagreen3","deepskyblue2","darkviolet")

spitab<-bind_cols(From=rev(c1),To=rev(c2),Value=c3,Color=rev(cols))
write.table(spitab,file = "spitab.txt")



# SPITAB Plot


prob<-c(2.3,4.4,9.2,68.2,9.2,4.4,2.3)
freq<-c(2.5,5,10,"-",10,5,2.5)

s2<-spitab %>% 
  mutate(Probability=prob) %>% 
  mutate(Frequency=freq)
  

write.table(s2,file = "spitab2.txt")



