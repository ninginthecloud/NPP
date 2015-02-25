coord.matrix=coord.US
index=3

coord.plant<-c(NPP$long[index],NPP$lat[index])
distance.centroid<-spDistsN1(coord.matrix,coord.plant,longlat=TRUE)
geoid<-rownames(coord.matrix)[distance.centroid<=8]


reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
