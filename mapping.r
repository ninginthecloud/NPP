mainDir="E:/Dropbox/NPP/working_R"
plot.factor<-c("pop","pop.density","white","employed","AFF","income")
path0="E:/UW/summer2014/USNPP/shapefile2010"
#1 Alabama Browns Ferry

system.time(
  for(i in 1:67)
  {
    npp.draw(myindex=i,plot.factor,path0,mainDir)
  }
)

#histogram
for(myindex in 1:67)
{
  mainDir="E:/Dropbox/NPP/working_R"
  plot.factor<-c("pop","pop.density","white","employed","AFF","income")
  path0="E:/UW/summer2014/USNPP/shapefile2010"
  
  reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
  shape.frame<-npp.shapeframe(path=shape_path,index=myindex,geoid.NPP,neighbour,statename)
  mapdata<-npp.mapdata(reg.data,shape.frame)
  #draw pictures
  for(i in plot.factor)
  {
    filename<-paste(NPP[myindex,"State"],"_",NPP[myindex,"Name"],"_",i,"_histogram.pdf",sep="")
    
    dir.create(file.path(mainDir, "histogram",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "histogram",i))
    histogram<-npp.hist(reg.data,i,NPP,myindex)
    pdf<-pdf(file=filename,width=10, height=8)
    print(histogram)
    dev.off()
  }
}

#plot
for(myindex in 1:67)
{
  mainDir="E:/Dropbox/NPP/working_R"
  plot.factor<-c("pop","pop.density","white","employed","AFF","income")
  path0="E:/UW/summer2014/USNPP/shapefile2010"
  
  reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
  #draw pictures
  for(i in plot.factor)
  {
    filename2<-paste(NPP[myindex,"State"],"_",NPP[myindex,"Name"],"_",i,"_plot.pdf",sep="")
    
    dir.create(file.path(mainDir, "plot",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "plot",i))
    plotting<-npp.plot(wkdata=reg.data,interest=i,index=myindex)
    pdf<-pdf(file=filename2,width=10, height=8)
    print(plotting)
    dev.off()
  }
}

#mapping
for(myindex in 1:67)
{
  mainDir="E:/Dropbox/NPP/working_R"
  plot.factor<-c("pop","pop.density","white","employed","AFF","income")
  path0="E:/UW/summer2014/USNPP/shapefile2010"
  
  reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
  #draw pictures
  for(i in plot.factor)
  {
    filename<-paste(NPP[myindex,"State"],"_",NPP[myindex,"Name"],"_",i,"_mapping.pdf",sep="")
    dir.create(file.path(mainDir, "mapping",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "mapping",i))
    mapping<-npp.mapping(regdata=reg.data,mapdata,NPP,index=myindex,factor.interest=plot.factor)
    pdf<-pdf(file=filename,width=10, height=8)
    print(mapping)
    dev.off()
  }
}