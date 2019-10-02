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


02/10/19 CW:
  
#Summarizing data:

library(tidyverse)
install.packages("nutshell")
library(nutshell)

#Data to be used today:
data("batting.2008")
d<-batting.2008

#tapply---(tidyverse ufnction)
#find sum of all home runs
hr<-tapply(X=d$HR, INDEX=list(d$teamID), FUN=sum)

#find quantile values for home runs by team
#fivenum gives you:min, lower-hinge, median,upper-hinge & max value
hr.q<-tapply(X=d$HR, INDEX=list(d$teamID), FUN=fivenum)
lg.q<-tapply(X=(d$H/d$AB), INDEX=list(d$lgID), FUN=fivenum)

#summary
summary(d[d$lgID=="AL",]$H/d[d$lgID=="AL",]$AB)
OR
summary(al.hits/al.bats)
head(al.hits)

#two category summarize
bats<-tapply(X=d$HR, INDEX=list(d$lgID,d$bats), FUN=mean)

#Three category summarize(crazy array)
bats.team<-tapply(X=d$HR, INDEX=list(d$lgID,d$bats), FUN=mean)

#aggregate----
team.stats.sum<-aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID,FUN=sum)
team.stats.mean<-aggregate(x=d[,c("AB","H","BB","2B","HR")], by=list(d$teamID,FUN=mean)

#tidyverse summarize()----
team.sum=d%>% group_by(teamID)%>% summarize(ABsum=sum(AB), 
              ABmean=mean(AB),ABsd=sd(AB),ABcount=n())

lg.team.sum=d%>% group_by(teamID)%>% summarize(ABsum=sum(AB), 
              ABmean=mean(AB),ABsd=sd(AB),ABcount=n())

str(team.sum)

#rowsum----
#when you just want to add up the values in each row
rs<-rowsum(d[,c("AB","H","HR","2B","3B")], group=d$teamID)

#counting variables----
#use the function "tabulate"

HR.cnts<-tabulate(d$HR)
names(HR.cnts)<-0:(length(HR.cnts)-1)

length(d$teamID)
length(unique(d$teamID))

#aside about the 'names' function--
m<-matrix(nrow=4,ncol=3)
colnames(m)<-c("one","two","three")
rownames(m)<-c("apple","pear","orange","berry")

#table
table(d$bats)
table(d[,c("bats","throws")])

#reshaping your data----
n<-matrix(1:10,nrow=5)
t(n)

v<-1:10
v
t(v)
str(t(v))

#unstack and stack----
names(d)

s<-d[,c("lgID","teamID","AB","HR","throws")]
s.un<-unstack(x=s,form=teamID~HR)
s.un<-unstack(x=s,form=HR~AB)

#melt and cast----
library(reshape2)

#use the "cast" function to change data frame from the long to wide format
s.wide<-dcast(data=s, value.var="HR",formula="lgID"~"teamID",
              fun.aggregate=mean)
                         
                         
                         
                         
                         

                           