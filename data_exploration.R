##################
#Author: Xin Ming
#created on:2024-04-18
#xm123456@mail.ustc.edu.cn
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#removing missing values from a dataset
#standardizing data for ordination analysis
#checking for colinearity and removing colinear variables
#ordination analysis of data(pca analysis to reveal structure of data, and rda analysis to reveal relationship between variables)
#visualizing the results of ordination
##################
#loading packages and downloading data
library(tidyverse)
library(vegan)
library(grid)
library(ggplotify)
env_raw<-read.csv("https://raw.githubusercontent.com/QCBSRworkshops/workshop10/main/pres-en/data/doubsenv.csv")
fish_raw<-read.csv("https://raw.githubusercontent.com/QCBSRworkshops/workshop10/main/pres-en/data/doubsspe.csv")
env_raw
fish_raw
env<-env_raw[-1]
fish<-fish_raw[-1]
sites<-fish_raw[1]

##removing missing data(no fish caught or NA present)
#recording number of rows and columns for downstream processsing.
fish_ncols=ncol(fish)
env_ncols=ncol(env)
nrows=nrow(env_raw)
env_colnames<-colnames(env)
removed<-c()
for(n in 1:nrows){
  if(0|sum(is.na(fish[n,]))|sum(is.na(env_raw[n,]))|(!sum(fish[n,]))){#check if a row has NAs, or has no fish caught
    removed<-c(removed,n)
  }
}
fish<-fish[-removed,]
env<-env[-removed,]
sites<-data.frame(X=sites[-removed,])
paste("site(s)",paste(as.character(removed),sep=","),"was(were) removed due to missing data")
sites
fish
env

##detecting and removing colinear environmental factors
#standardizing data(hellinger transform fish data and normalize env data)
fish_hellinger<-decostand(fish,method="hellinger",MARGIN=2)
env_stand<-decostand(env,method="standardize",MARGIN=2)
#computing correlation and inflation factor matrices
corrs<-matrix(0,env_ncols,env_ncols)
n=1
for(n in 1:env_ncols){
  for(m in 1:env_ncols){
    corrs[n,m]<-cor(env_stand[,m],env_stand[,n],method="pearson")
  }
}
VIFs<-1/(1-corrs^2)
#displaying the matrixes
corrs_df=data.frame(corrs)
rownames(corrs_df)<-colnames(env)
colnames(corrs_df)<-colnames(env)
VIFs_df=data.frame(VIFs)
rownames(VIFs_df)<-colnames(env)
colnames(VIFs_df)<-colnames(env)
corrs_df
VIFs_df
#plotting colinearity plots
myplotlist<-list()
plotid<-0
for(n in 1:env_ncols){
  for(m in 1:env_ncols){
    if(m<n){#subdiagonal:scatterplot with trendline
      plotcell<-ggplot(data=env_stand,aes_string(x=env_colnames[m],y=env_colnames[n]))+geom_point()+ geom_smooth(se=FALSE)+theme_bw()+theme_bw()+theme(panel.grid = element_blank(),plot.background=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",plot.margin=unit(c(0,0,0,0), "cm"))
    }
    else if(m==n){#diagonal:histogram with trendline
      plotcell<-ggplot(data=env_stand[n],aes_string(x=env_colnames[n]))+geom_histogram(aes(y=..density..),color="lightblue2",fill="lightblue2",bins=10)+geom_density()+theme(panel.grid = element_blank(),plot.background=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",plot.margin=unit(c(0,0,0,0), "cm"))
    }
    else{#supdiagonal:displaying R and VIF
      corr<-corrs[n,m]
      labs<-c("",paste("R",as.character(round(corr,3)),sep="="),paste("VIF",as.character(round(VIFs[n,m],3)),sep="="),"")
      labdat<-data.frame(X=c(0,0,0,0),Y=c(0,1,3,4),labs=labs)
      if(corr>=0){
        mycolor=rgb(1,1-corr,1-corr)
      }
      else{
        mycolor=rgb(1+corr,1+corr,1)
      }
      plotcell<-ggplot(data=labdat,aes(x=X,y=Y,label=labs))+ theme(panel.background = element_rect(fill = mycolor, color = "black"),panel.grid = element_blank(),plot.background=element_blank(),axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none",plot.margin=unit(c(0,0,0,0), "cm"))+geom_text()
    }
    plotid<-plotid+1
    myplotlist[[plotid]]<-plotcell
  }
}
#using ggarrange() to assemble different plots
colinearity_plot<-ggpubr::ggarrange(plotlist=myplotlist,ncol=env_ncols,nrow=env_ncols,align="hv")
colinearity_plot
ggpubr::annotate_figure(p=colinearity_plot,left=grid.text(label=env_colnames,y=unit(((env_ncols:1)-0.5)/(env_ncols+0.5),"npc"),rot=90),top=grid.text(label=env_colnames,x=unit(((1:env_ncols)-0.5)/(env_ncols),"npc")))
#selectinbg and removing highly colinear variables.
vif_treshold=8#feel free to tune this number
determine<-(VIFs>vif_treshold)
var_indexes<-1:env_ncols
while(sum(determine)>ncol(determine)){#removing variables one by one, each step removing the one that correlates to the most number of other vars
  counts=apply(determine,2,sum)
  removed_local_index<-which.max(counts)
  var_indexes<-var_indexes[-removed_local_index]
  determine<-determine[-removed_local_index,]
  determine<-determine[,-removed_local_index]
}
env_stand_remaining<-env_stand[var_indexes]
env_colnames_remaining<-env_colnames[var_indexes]
env_colnames_remaining

##investigating the relationship between environmental factors and fishes
#principal component anlysis of environmental data.
env_stand_pca<-rda(env_stand)
env_stand_remaining_pca<-rda(env_stand_remaining)
env_stand_pca
env_stand_remaining_pca
pca_plot_env_ori_1<-as.grob(~biplot(env_stand_pca,scaling=1))
pca_plot_env_ori_2<-as.grob(~biplot(env_stand_pca,scaling=2))
pca_plot_env_rem_1<-as.grob(~biplot(env_stand_remaining_pca,scaling=1))
pca_plot_env_rem_2<-as.grob(~biplot(env_stand_remaining_pca,scaling=2))
pca_plot_env_plotlist<-list(pca_plot_env_ori_1,pca_plot_env_ori_2,pca_plot_env_rem_1,pca_plot_env_rem_2)
pca_plot_env<-ggpubr::ggarrange(plotlist=pca_plot_env_plotlist,ncol=2,nrow=2,align="hv")
pca_plot_env
ggpubr::annotate_figure(p=pca_plot_env,left=grid.text(label=c("all env factors","highly colinear removed"),x=c(-0.05,-0.05),y=unit(c(0.75,0.25),"npc"),rot=90),bottom=grid.text(label=c("scaling=1","scaling=2"),x=c(0.25,0.75),y=unit(c(0.05,0.05),"npc")))
#principal component analysis of fish occurance
fish_hellinger_pca<-rda(fish_hellinger)
fish_hellinger_pca
pca_pot_fish_hellinger_1<-as.grob(~biplot(fish_hellinger_pca,scaling=1))
pca_pot_fish_hellinger_2<-as.grob(~biplot(fish_hellinger_pca,scaling=2))
pca_plot_fish_plotlist=list(pca_pot_fish_hellinger_1,pca_pot_fish_hellinger_2)
pca_plot_fish<-ggpubr::ggarrange(plotlist=pca_plot_fish_plotlist,ncol=2,nrow=1,align="hv")
pca_plot_fish
ggpubr::annotate_figure(p=pca_plot_fish,bottom=grid.text(label=c("scaling=1","scaling=2"),x=c(0.25,0.75),y=unit(c(0.5,0.5),"npc")))
#rda analysis of how environmental factors effect fish occurance
fish_env_rda_ori<-rda(fish_hellinger~.,data=env_stand)
fish_env_rda_rem<-rda(fish_hellinger~.,data=env_stand_remaining)
fish_env_rda_ori
fish_env_rda_rem
rda_plot_ori_1<-as.grob(~ordiplot(fish_env_rda_ori,scaling=1,type="text"))
rda_plot_ori_2<-as.grob(~ordiplot(fish_env_rda_ori,scaling=2,type="text"))
rda_plot_rem_1<-as.grob(~ordiplot(fish_env_rda_rem,scaling=1,type="text"))
rda_plot_rem_2<-as.grob(~ordiplot(fish_env_rda_rem,scaling=2,type="text"))
rda_plot_plotlist=list(rda_plot_ori_1,rda_plot_ori_2,rda_plot_rem_1,rda_plot_rem_2)
rda_plot<-ggpubr::ggarrange(plotlist=rda_plot_plotlist,ncol=2,nrow=2,align="hv")
rda_plot
ggpubr::annotate_figure(p=rda_plot,left=grid.text(label=c("all env factors","highly colinear removed"),x=c(-0.05,-0.05),y=unit(c(0.75,0.25),"npc"),rot=90),bottom=grid.text(label=c("scaling=1","scaling=2"),x=c(0.25,0.75),y=unit(c(0.05,0.05),"npc")))
#selection of significant variables
selection_ori<-ordiR2step(rda(fish_hellinger~1,data=env_stand),scope=formula(fish_env_rda_ori),direction="forward",R2scope=TRUE,pstep=1000,trace=FALSE)
selection_rem<-ordiR2step(rda(fish_hellinger~1,data=env_stand_remaining),scope=formula(fish_env_rda_rem),direction="forward",R2scope=TRUE,pstep=1000,trace=FALSE)
selected_ori<-selection_ori[["call"]][["formula"]]
selected_rem<-selection_rem[["call"]][["formula"]]
selected_ori
selected_rem
#redoing RDA analysis with selected variables
fish_env_rda_signif_ori<-rda(selected_ori,data=env_stand)
fish_env_rda_signif_rem<-rda(selected_rem,data=env_stand)
fish_env_rda_signif_ori
fish_env_rda_signif_rem
rda_plot_ori_signif_1<-as.grob(~ordiplot(fish_env_rda_signif_ori,scaling=1,type="text"))
rda_plot_ori_signif_2<-as.grob(~ordiplot(fish_env_rda_signif_ori,scaling=2,type="text"))
rda_plot_rem_signif_1<-as.grob(~ordiplot(fish_env_rda_signif_rem,scaling=1,type="text"))
rda_plot_rem_signif_2<-as.grob(~ordiplot(fish_env_rda_signif_rem,scaling=2,type="text"))
rda_plot_signif_plotlist=list(rda_plot_ori_signif_1,rda_plot_ori_signif_2,rda_plot_rem_signif_1,rda_plot_rem_signif_2)
rda_plot_signif<-ggpubr::ggarrange(plotlist=rda_plot_signif_plotlist,ncol=2,nrow=2,align="hv")
rda_plot_signif
ggpubr::annotate_figure(p=rda_plot_signif,left=grid.text(label=c("all env factors","highly colinear removed"),x=c(-0.05,-0.05),y=unit(c(0.75,0.25),"npc"),rot=90),bottom=grid.text(label=c("scaling=1","scaling=2"),x=c(0.25,0.75),y=unit(c(0.05,0.05),"npc")))
#evaluating significance of the models fitted
anova.cca(fish_env_rda_signif_ori)
anova.cca(fish_env_rda_signif_rem)
