

2.0+,extremely wet1.5 to 1.99very wet1.0 to 1.49moderately wet-.99 to .99near normal-1.0 to -1.49moderately dry-1.5 to -1.99severely dry-2 and lessextremely dry


cbind(c(2,10,"Extremely Wet"),
      c(1.5,2,"Very Wet"),
      c(1,)
      )


c1<-c(-10,seq(-2,2,by=0.5))
c2<-c(seq(-2,2,by=0.5),10)
c3<-c("extremely wet","very wet","moderately wet","slightly wet","near normal","near normal","slightly dry","moderately dry","severely dry","extremely dry")

spitab<-bind_cols(From=rev(c1),To=rev(c2),Value=rev(c3))
write.table(spitab,file = "spitab.txt")
