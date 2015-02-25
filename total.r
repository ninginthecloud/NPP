###########################
# FINAL
###########################

#install necessary packages
source("library.R")
source("function definition.R")
load("NPP.RData")
#######################################
#TODO
#######################################
#download shapefile from FTP
#need to modify according to different years
url0="ftp://ftp2.census.gov/geo/pvs/tiger2010st/"
path="E:/UW/summer2014/USNPP"
statename<-read.csv("E:/UW/summer2014/USNPP/other/statename.csv",header=T,colClasses="character")
url.list<-paste(url0,statename[,2],"_",statename[,1],"/",statename[,2],"/tl_2010_",statename[,2],"_tract10.zip",sep="")
download.list<-paste(path,"/rawshapefile/",statename[,1],".zip",sep="")
shapefilename<-paste(path,"/rawshapefile/tl_2010_",statename[,2],"_tract10.shp",sep="");
#######################################
for(i in 1:dim(statename)[1])
{
  download.file(url=url.list[i], destfile=download.list[i])
}

sapply(download.list,FUN=unzip,exdir=paste(path,"/rawshapefile",sep=""))
#save shapefiles in order for later loading
print("choose directory to save all these shapefile")
savedirect<-choose.dir()
for(i in 1:length(shapefilename))
  npp.save.shapefile(shapefilename,savedirect,statename[i,1])


#get coordinates of entire united states shapefiles
path="E:/UW/summer2014/USNPP/"
filename<-paste(path,"/shapefile2010/",statename[,1],".RData",sep="")
coord.US<-NULL
for(i in 1:length(filename))
{
  coord.US<-rbind(coord.US,npp.coord.matrix(filename[i]))
}

geoid.NPP<-lapply(c(1:dim(NPP)[1]),FUN=npp.selectone,coord.matrix=coord.US,distance=50)
str(geoid.NPP)
########################
#get shape.dataframe
#and save them
########################

#todo path


neighbour<-lapply(1:67,FUN=npp.neighbour,geoid=geoid.NPP)
########################
#read in characteristic data
########################
myindex=6
short<-npp.read(path="E:/Dropbox/NPP/NPP",Ychar="0812")
reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
path0="E:/UW/summer2014/USNPP/shapefile2010"
shape.frame<-npp.shapeframe(path=path0,index=myindex,geoid.NPP,neighbour,statename)

mapdata<-npp.mapdata(reg.data,shape.frame)

#mapping

mapping.pop<-npp.mapping(regdata=reg.data,mapdata,NPP,index=myindex,factor.interest="pop")
mapping.pop
plot.pop<-npp.plot(wkdata=reg.data,interest="pop")
plot.pop

#regression





