####################################
#function definition
####################################
######################
######################################
#download shapefile from FTP and unzip
log_fun<-function(x){return(log((x+1)/(101-x)))}
#read and save all shapfile in RData

npp.save.shapefile<-function(filename,directory,statename)
{
  shapefile<-readShapeSpatial(filename,proj4string=CRS("+proj=longlat +datum=WGS84"))
  save(shapefile,file=paste(directory,"/",statename,".RData",sep=""))
}

#get coordinates from shapefile
#argument requires including path	
npp.coord.matrix<-function(filename)
{
  load(filename)
  coord.matrix<-as.matrix(cbind(as.numeric(as.character(shapefile$INTPTLON10)),as.numeric(as.character(shapefile$INTPTLAT10))),ncol=2);
  coord.id<-as.character(shapefile$GEOID10);
  coord.matrix<-coord.matrix[order(coord.id),]
  rownames(coord.matrix)<-sort(coord.id)
  colnames(coord.matrix)<-c("long0","lat0")
  return(coord.matrix)
}


#select useful centroids census tract id
#get centroids candidates for specific power plants[index]
#coord.matrix is the coordinates of the whole US 
npp.selectone<-function(coord.matrix,distance=50,index)
{
  if(!"NPP"%in%objects())
  {
    load("NPP.RData")
  }
  coord.plant<-c(NPP$long[index],NPP$lat[index])
  distance.centroid<-spDistsN1(coord.matrix,coord.plant,longlat=TRUE)
  geoid<-rownames(coord.matrix)[distance.centroid<=distance]
  #return values
  if(identical(geoid, character(0)))
  {
    geoid<-rownames(coord.matrix)[which.min(distance.centroid)]
    return(list(geoid=geoid,distance=distance.centroid[which.min(distance.centroid)],coord.matrix=matrix(coord.matrix[which.min(distance.centroid),],ncol=2,dimnames=list(c(geoid),c("long0","lat0")))))
  }
  else{
    return(list(geoid=geoid,distance=distance.centroid[distance.centroid<=distance],coord.matrix=matrix(coord.matrix[distance.centroid<=distance,],ncol=2,dimnames=list(c(geoid),c("long0","lat0")))))
  }
}
#get NPP neighbour states
#geoid is a list, index represents the NPP 
npp.neighbour<-function(index,geoid)
{
  return(unique(substring(geoid[[index]][[1]],1,2)))
}



#generate shape dataframe for specific power plant (index)
npp.shapeframe<-function(path,index,geoid.NPP,neighbour,statename)
{
  
  shape.frame<-NULL;
  for(i in as.numeric(neighbour[[index]]))
  {
    load(paste(path,"/",statename[as.numeric(statename[,2])==i,1],".RData",sep=""))
    shape.frame<-rbind(shape.frame,fortify(shapefile,region="GEOID10"))
  }
  shape.frame<-shape.frame[(shape.frame$id)%in%geoid.NPP[[index]][[1]],]
  return(shape.frame)
}

#read csv file
#read csv file
npp.read<-function(path=choose.dir(),statename=NULL,Ychar)
{
  #read the whole us census data or read a single state census data
  if(is.null(statename))
  {
    filename=paste(path,"/US_Census_Data",Ychar,".csv",sep="");
    first<-read.csv(file=filename,header=T,stringsAsFactors=F)
    first<-first[-1,]
  }
  else
  {
    filename=paste(path,"/",statename,"_Census_Data",Ychar,".csv",sep="");
    first<-read.csv(file=filename,header=T,stringsAsFactors=F)
    first<-first[-1,]
  }
  #read description
  path0="E:/Dropbox/NPP/NPP"
  interest<-read.csv(file=paste(path0,"/description.csv",sep=""),header=T,stringsAsFactors=F)
  short<-first[,!is.na(interest[,3])];
  colnames(short)<-as.character(interest[,4][!is.na(interest[,3])])
  for(i in 2:dim(short)[2])
  {
    short[,i]<-as.numeric(short[,i])
  }
  
  return(short);
}


npp.regdata<-function(rawdata,geoid.NPP,index)
{
  location<-which(rawdata$id%in%geoid.NPP[[index]][[1]]==1)
  wkdata<-rawdata[location,];
  wkdata$logincome<-log(wkdata[,"income"])
  ##age group 18-24,25-44,45-64,65-74,75+
  wkdata$age1<-wkdata[,"ratio.18to24"];
  wkdata$age2<-wkdata[,"ratio.25to34"]+wkdata[,"ratio.35to44"]
  wkdata$age3<-wkdata[,"ratio.45to54"]+wkdata[,"ratio.55to64"]
  wkdata$age4<-wkdata[,"ratio.65to74"];
  wkdata$age5<-wkdata[,"ratio.75to84"]+wkdata[,"ratio.85"]
  ##edu
  wkdata$edu5<-wkdata$edu5.1+wkdata$edu5.2
  wkdata$lat0<-geoid.NPP[[index]][[3]][,"lat0"]
  wkdata$long0<-geoid.NPP[[index]][[3]][,"long0"]
  wkdata$distance<-geoid.NPP[[index]][[2]]
  #transformation	
  wkdata$transwhite<-log_fun(wkdata$white)
  wkdata$transemployed<-log_fun(wkdata$employed)
  wkdata$transAFF<-log_fun(wkdata$AFF)
  
  return(wkdata)
}


npp.mapdata<-function(wkdata,shape.frame)
{
  d<-join(wkdata,shape.frame,by="id")
  polygon.matrix<-matrix(c(d$long,d$lat),ncol=2);
  grid<-latlong2grid(polygon.matrix)
  d$long.km=as.numeric(grid$x)
  d$lat.km=as.numeric(grid$y)
  mapdata<-d[!is.na(d$income),]
  return(mapdata)
}

######################################################################################
#circle data
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100)
{
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circle<-function(distance,index)
{
  circle.data<-circleFun(center=c(NPP[index,"long.km"],NPP[index,"lat.km"]),diameter=distance*2,npoints=500)
  circle.data$group<-rep(0,500)
  return(circle.data)
}

#mapping factors interested
#categorate them and draw
npp.mapping<-function(regdata,mapdata,NPP,index,factor.interest)
{
  mycolor<-brewer.pal(7,"Oranges")[3:7]
  quant<-classIntervals(regdata[,factor.interest],n=5,style="fisher")$brks
  #quant<-quantile(1:dim(regdata[!is.na(regdata[,factor.interest]),])[1]);
  color<-cut(mapdata[,factor.interest],c(quant[1],quant[2],quant[3],quant[4],quant[5],quant[6]),dig.lab=6,include.lowest=TRUE)
  mapdata$color<-color
  
  circle.data50<-circle(50,index)
  
  map<- ggplot()+
    geom_polygon(data=mapdata, aes(x=long.km, y=lat.km,group=group,fill =color))+
    scale_fill_manual(values=mycolor,name=factor.interest)+
    geom_path(data=circle.data50,aes(x=x,y=y,group=group),color="dark grey")+
    geom_point(data=NPP[index,],aes(x=long.km,y=lat.km,group=group),color="red",size=4)+
    geom_text(data = NPP[index,], x = NPP[index,"long.km"], y = NPP[index,"lat.km"]-5, label = NPP[index,"Name"])+
    ggtitle(paste(factor.interest,"mapping in",NPP[index,"State"]))
  return(map)
}

npp.plot<-function(wkdata,interest,ylab0=NULL,title0=NULL,index)
{
  if(is.null(ylab0)) {ylab0=interest}
  if(is.null(title0)) {title0=paste(interest,"plot in",NPP[index,"State"],NPP[index,"Name"])}
  colnames(wkdata)[colnames(wkdata)=="distance"]="X"
  colnames(wkdata)[colnames(wkdata)==interest]="Y"
  plot<-ggplot(data=wkdata,aes(x=X,y=Y))+
    stat_smooth(geom="smooth",position="identity",method="gam",formula=y~s(x),colour="red",data=wkdata)+
    geom_point()+
    ylab(ylab0)+
    xlab("distance(km)")+
    ggtitle(title0)
  
  return(plot)
}

npp.hist<-function(wkdata,interest,NPP,index)
{
  colnames(wkdata)[colnames(wkdata)==interest]="X"
  
  plot<-ggplot(data=wkdata)+
    geom_histogram(aes(x=X,y=..density..),color="black",fill="lightblue")+
    xlab(interest)+
    ggtitle(paste("histogram of",interest,"in",NPP[index,"State"],NPP[index,"Name"]))
  
  return(plot)
}
##############################
#complex function definition
##############################
npp.draw<-function(myindex,plot.factor,shape_path,mainDir)
{
  reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
  
  shape.frame<-npp.shapeframe(path=shape_path,index=myindex,geoid.NPP,neighbour,statename)
  mapdata<-npp.mapdata(reg.data,shape.frame)
  #draw pictures
  for(i in plot.factor)
  {
    filename1<-paste(c(unlist(strsplit(c(NPP[myindex,"State"],NPP[myindex,"Name"])," ")),i,"mapping.pdf"),sep="_",collapse="_")
    filename2<-paste(c(unlist(strsplit(c(NPP[myindex,"State"],NPP[myindex,"Name"])," ")),i,"plot.pdf"),sep="_",collapse="_")
    
    dir.create(file.path(mainDir, "mapping",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "mapping",i))
    mapping<-npp.mapping(regdata=reg.data,mapdata,NPP,index=myindex,factor.interest=i)
    pdf<-pdf(file=filename1,width=10, height=8)
    print(mapping)
    dev.off()
    
    dir.create(file.path(mainDir, "plot",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "plot",i))
    plotting<-npp.plot(wkdata=reg.data,interest=i,index=myindex)
    pdf<-pdf(file=filename2,width=10, height=8)
    print(plotting)
    dev.off()
  }
}