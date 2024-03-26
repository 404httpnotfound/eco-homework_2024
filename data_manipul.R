##################
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#-importing and saving data
#-inspecting data structure
#-checking whether there is missing data
#-extracting/selecting/adding a column
#-transforming a wide format df to long format df
#-visualizing data.
#data was downloaded from https://www.dataanalytics.org.uk/wp-content/uploads/2019/08/CERE-Support-Files.zip
##################
library(xlsx)
library(tidyverse)
library(ggplot2)
#reading data(both xlsx and csv format)
moss_wide<-read.xlsx("data/Bryophyte biol.xlsx",1)
butterfly<-read.csv("data/Butterfly and year data.csv")
head(moss_wide)
head(butterfly)
#displaying structures of input data
typeof(moss_wide)
typeof(butterfly)
class(moss_wide)
class(butterfly)
str(moss_wide)
str(butterfly)
#checking if there is any NaN
moss.hasna=sum(is.na(moss_wide))|0
butterfly.hasna=sum(is.na(butterfly))|0
#checking where are the NaNs, if any at all
{
  if(!moss.hasna){
  moss.napos<-data.frame(matrix(ncol=2,nrow=0))
  }
  else {
    rownames<-rownames(moss.wide)
    colnames<-colnames(moss.wide)
    pos<-which(is.na(moss.wide),arr.ind=TRUE)
    moss.napos<-data.frame(rownames[pos[,1]],colnames[pos[,2]])
  }
  names(moss.napos)<-c("row","column")
}
moss.napos
{
  if(!butterfly.hasna){
    butterfly.napos<-data.frame(matrix(ncol=2,nrow=0))
  }
  else {
    rownames<-rownames(buttefly)
    colnames<-colnames(butterfly)
    pos<-which(is.na(butterfly),arr.ind=TRUE)
    butterfly.napos<-data.frame(rownames[pos[,1]],colnames[pos[,2]])
  }
  names(butterfly.napos)<-c("row","column")
}
butterfly.napos
#filtering out NaNs
if(moss.hasna){
  colnames=colnames(moss_wide)
  for(colname in colnames){
    col=as.name(colname)
    moss.wide=filter(moss_wide,is.na(col))
  }
}
head(moss_wide)
if(butterfly.hasna){
  colnames=colnames(butterfly)
  for(colname in colnames){
    col=as.name(colname)
    moss.wide=filter(butterfly,is.na(col))
  }
}
head(butterfly)
#three ways of extracting column
butterfly.Qty1<-butterfly[3]
butterfly.Qty1
class(butterfly.Qty1)
butterfly.Qty2<-butterfly[,3]
butterfly.Qty2
class(butterfly.Qty2)
butterfly.Qty3<-butterfly[,"Qty"]
butterfly.Qty3
class(butterfly.Qty3)
#adding a column
moss.core<-moss_wide
moss.core<-moss.core[-1]
moss.sums<-apply(moss.core,1,sum)
moss.withsum<-moss_wide%>%
  mutate(sum=moss.sums[Stand])
moss.withsum
#selecting column
moss.withsum
moss.some<-moss_wide%>%
  select(Lop.hete:Thi.tama)
moss.some
#transforming a wide df to a long df. and vice-versa
moss_long<-moss_wide%>%
  gather(key=species,value=counts,-Stand)
moss_long
tail(moss_long)
butterfly_wide<-butterfly%>%
  spread(key=Spp,value=Qty)
butterfly_wide
#visualising data
Com.blu.plot=ggplot(data=butterfly_wide,aes(x=Yr,y=Com.blu))
Com.blu.plot=Com.blu.plot+geom_point(alpha=0.5,color="blue")+theme_bw()+theme(panel.grid = element_line(),plot.background=element_blank())+labs(title="common blue butterfly",x="year",y="observations")+xlim(1995,2005)
Com.blu.plot
all_butterfly.plot.merged=ggplot(data=butterfly,aes(x=Yr,y=Qty,color=Spp))
all_butterfly.plot.merged=all_butterfly.plot.merged+geom_point(alpha=0.5)+theme_bw()+theme(panel.grid = element_line(),plot.background=element_blank())+labs(title="common blue butterfly",x="year",y="observations")+xlim(1995,2005)
all_butterfly.plot.merged
all_butterfly.plot.sep=all_butterfly.plot.merged+facet_wrap(~Spp)+geom_line()
all_butterfly.plot.sep
#saving data and plots
write.csv(moss.withsum,"data/moss_withsum.csv")
write.xlsx(moss_long,"data/moss_long.xlsx")
write.xlsx(butterfly_wide,"data/butterfly_wide.xlsx")
ggsave("plots/Com_blue.jpg",Com.blu.plot)
ggsave("plots/All_butterflies_merged.jpg",all_butterfly.plot.merged)
ggsave("plots/All_butterflies_sep.jpg",all_butterfly.plot.sep)
