
plot(r1)

plot(r2,ext=extent(oi2_shape[6,])*2,main="Vimes1500 NDVI on 20150627")
plot(oi2_shape[6,],add=T)
plot(oi2[5,],add=T)

ex<-extract2(r2,oi2_shape[6,],narm=T,sd=T)
ex
