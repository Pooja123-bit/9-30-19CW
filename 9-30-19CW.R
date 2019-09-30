load("fish_data.Rdata")
f<-fish

#practice conditions
#subsetting
fd<-f[f$depth_fac=="Deep",]
fd2<-subset(x=f, depth_fac=="Deep")

#shallow tows
fd3<-subset(x=f, depth_fac=="shallow tow")

#east
fd4<-f[f$area_fac=="East",]

#patches
fd5<-f[f$type_fac=="Patch",]

fd6<-subset(x=f, depth_fac=="Deep", delect=c("transect.id","area_fac"))

#filter
library(dplyr)
fd3<-filter(.data=f, depth_fac=="Deep")

#which
fd5<-f[which(f$depth_fac=='Deep' & f$area_fac=="East"),]

fd6<-f[which(f$depth_fac=='Deep' & f$area_fac=="East" & f$yr_fac=="2014"),]

#subset & then combine using rowbind (rbind function)----
d1<- f[which(f$depth_fac=='Deep' & f$area_fac=="East"),]

d2<- f[which(f$depth_fac=='shallow' & f$area_fac=="West"),]

#combine d1 and d2 back into a single data frame
d3<-rbind(d1,d2)
nrow(d3)

#combine data frames with separate columns into a single data frame
c1<-subset(x=f, depth_fac=="Deep",select=c("transect.id","area_fac"))
c2<-subset(x=f, depth_fac=="Deep",select=c("depth_fac","parcel.length.m","group"))
c3<-cbind(c1,c2)

#merging two data frames, ensuring that observations from  one data frame are connected
#with observations in the second data frame correctly

m1<-subset(x=f, depth_fac=="Deep", delect=c("transect.id","area_fac"))
m1$seq<-seq(1,nrow(m1),1)
head(m1)
m2<-subset(x=f, depth_fac=="Deep", delect=c("transect.id","
                                            depth_fac","parcel.length.m","group"))
#create a sequence of data
m2$seq<-seq(1,nrow(m2),1)
m2$seq<-seq(from=1, to=nrow(m2),by=1)

v<-seq(5,20,0.5)

vc<-cut(x=v,breaks=seq(5,20,1),include.lowest=T)

#merge
mt<-merge(x=m1,y=m2,by=c("transect.id","seq"),all.x = T,no.dups = T)
nrow(mt)
nrow(m1)+nrow(m2)

#join
library(dplyr)
mj<-dplyr::right_join(x=m1,y=m2,by=c('transect.id'))

