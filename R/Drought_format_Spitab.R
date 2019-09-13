

c1<-c(-3.5,seq(-2,2,by=0.5)) %>% .[-which(.==0)]
c2<-c(seq(-2,2,by=0.5),3.5) %>% .[-which(.==0)]
c3<-c("extremely wet","very wet","moderately wet","slightly wet","near normal","slightly dry","moderately dry","very dry","extremely dry")

cols<-c("darkred","red","orange","yellow","white","plum","palevioletred","darkviolet","darkslateblue")

spitab<-bind_cols(From=rev(c1),To=rev(c2),Value=c3,Color=rev(cols))
write.table(spitab,file = "spitab.txt")
