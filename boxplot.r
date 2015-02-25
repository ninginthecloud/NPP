#boxplot
short<-npp.read(path="E:/Dropbox/NPP/NPP",Ychar="0812")

#distance 8km
geoid_8<-lapply(c(1:dim(NPP)[1]),FUN=npp.selectone,coord.matrix=coord.US,distance=8)
str(geoid_8)
neighbour_8<-lapply(1:67,FUN=npp.neighbour,geoid=geoid_8)
#distance 50km
geoid_50<-lapply(c(1:dim(NPP)[1]),FUN=npp.selectone,coord.matrix=coord.US,distance=50)
str(geoid_50)
neighbour_50<-lapply(1:67,FUN=npp.neighbour,geoid=geoid_50)
#distance between 8 and 50km

geoid_between<-geoid_50
for(i in 1:67)
{
  location<-which(geoid_50[[i]][[1]]%in%geoid_8[[i]][[1]]!=1)
  geoid_between[[i]][[1]]<-geoid_50[[i]][[1]][location]
  geoid_between[[i]][[2]]<-geoid_50[[i]][[2]][location]
  geoid_between[[i]][[3]]<-geoid_50[[i]][[3]][location,]
}

number_8<-NULL
for(i in 1:67){
  number_8<-c(number_8,length(geoid_8[[i]][[1]]))
}


#poverty ratio poor
poverty<-NULL
for(myindex in which(number_8>1)){
  reg.data_8<-npp.regdata(short,geoid_8,index=myindex)
  reg.data_between<-npp.regdata(short,geoid_between,index=myindex)
  poverty<-rbind(poverty,c(median(reg.data_8$poverty1,na.rm=T),median(reg.data_between$poverty1,na.rm=T)))
  #boxplot(reg.data_8$poverty1,reg.data_between$poverty1)
  #invisible(readline(prompt="Press [enter] to continue"))
}

#income
income<-NULL
for(myindex in which(number_8>1)){
  reg.data_8<-npp.regdata(short,geoid_8,index=myindex)
  reg.data_between<-npp.regdata(short,geoid_between,index=myindex)
  income<-rbind(income,c(median(reg.data_8$income,na.rm=T),median(reg.data_between$income,na.rm=T)))
  #boxplot(reg.data_8$income1,reg.data_between$income1)
  #invisible(readline(prompt="Press [enter] to continue"))
}
t.test(income[,1],income[,2])#39 2 

#edu
edu<-NULL
for(myindex in which(number_8>1)){
  reg.data_8<-npp.regdata(short,geoid_8,index=myindex)
  reg.data_between<-npp.regdata(short,geoid_between,index=myindex)
  edu<-rbind(edu,c(median(reg.data_8$edu5,na.rm=T),median(reg.data_between$edu5,na.rm=T)))
  #boxplot(reg.data_8$edu1,reg.data_between$edu1)
  #invisible(readline(prompt="Press [enter] to continue"))
}




#
employed<-NULL
n<-NULL
for(myindex in which(number_8>1)){
  reg.data_8<-npp.regdata(short,geoid_8,index=myindex)
  reg.data_between<-npp.regdata(short,geoid_between,index=myindex)
  employed<-rbind(employed,c(median(reg.data_8$employed,na.rm=T),median(reg.data_between$employed,na.rm=T)))
  n<-rbind(n,c(sum(reg.data_8$pop,na.rm=T),sum(reg.data_between$pop,na.rm=T)))
  #boxplot(reg.data_8$employed,reg.data_between$employed)
  #invisible(readline(prompt="Press [enter] to continue"))
}
test<-NULL
for(i in 1:39)
{
  test<-c(test,(prop.test(employed[i,]*n[i,]/100,n[i,]))$p.value)
}
test
sum(test<0.05)#35



t.test(employed[,1],employed[,2])

#
log_fun<-function(x){return(log((x+1)/(101-x)))}

for(myindex in 1:67)
{
  mainDir="E:/Dropbox/NPP/working_R"
  plot.factor<-c("transwhite","transemployed","transAFF")
  path0="E:/UW/summer2014/USNPP/shapefile2010"
  
  reg.data<-npp.regdata(short,geoid.NPP,index=myindex)
  reg.data$transwhite<-log_fun(reg.data$white)
  reg.data$transemployed<-log_fun(reg.data$employed)
  reg.data$transAFF<-log_fun(reg.data$AFF)
  #draw pictures
  for(i in plot.factor)
  {
    filename2<-paste(NPP[myindex,"State"],"_",NPP[myindex,"Name"],"_",i,"_plot.pdf",sep="")
    
    dir.create(file.path(mainDir, "transplot",i), showWarnings = FALSE)
    setwd(file.path(mainDir, "transplot",i))
    plotting<-npp.plot(wkdata=reg.data,interest=i,index=myindex)
    pdf<-pdf(file=filename2,width=10, height=8)
    print(plotting)
    dev.off()
  }
}


#generate dataframe for boxplot
box.data<-reg.data[-1:-dim(reg.data)[1],];
for(myindex in which(number_8>1))
{
  reg.data_8<-npp.regdata(short,geoid_8,index=myindex)
  reg.data_8$area<-factor(rep("within8",dim(reg.data_8)[1]))
  reg.data_8$name<-factor(rep(NPP[myindex,"Name"],dim(reg.data_8)[1]))
  reg.data_8$state<-factor(rep(NPP[myindex,"State"],dim(reg.data_8)[1]))
  
  reg.data_between<-npp.regdata(short,geoid_between,index=myindex)
  reg.data_between$area<-factor(rep("out8",dim(reg.data_between)[1]))
  reg.data_between$name<-factor(rep(NPP[myindex,"Name"],dim(reg.data_between)[1]))
  reg.data_between$state<-factor(rep(NPP[myindex,"State"],dim(reg.data_between)[1]))
  
  box.data<-rbind(box.data,reg.data_8)
  box.data<-rbind(box.data,reg.data_between)
}

p <- ggplot(box.data, aes(name, income))+
  geom_boxplot(aes(fill = area))+
  coord_flip()
p
pdf<-pdf(file="E:/Dropbox/NPP/working_R/boxplot_income2.pdf",width=10, height=8)
p
dev.off()


write.table(box.data,file="boxdata.txt")
