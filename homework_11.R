##################
#Author: Xin Ming
#created on:2024.05.29
#xm123456@mail.ustc.edu.cn
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#constructing a network from pyrosequcning OTU abundance data, and saving such network
#analyzing network properties at node, edge and whole network level
##################
##constructing network edge list from sequencing data
#loading in packages:
library(igraph)
library(tidyverse)
library(coin)
#downloading and loading in data:
download.file(url="http://129.15.40.240/MENA/download/Control.txt",destfile="D:/eco-homework_2024/otu.txt")
raw_ab<-read.csv("D:/eco-homework_2024/otu.txt",sep="\t")
head(raw_ab)
#dealing with NAs: therev appears to be too many NAs, we will remove all OTUs with no less than than half NAs
NA_remove_fraction<-0.5
NA_remove_threshold<-(ncol(raw_ab)-1)*NA_remove_fraction
ab_rm<-raw_ab%>%
  is.na()%>%
  apply(MARGIN=1,FUN=sum)%>%
  cbind(nas=.,raw_ab)%>%
  filter(nas<NA_remove_threshold)%>%
  .[-1]
#since NAs most likely represent too low DNA concentration for detection under a given sequencing depth, we can assign a value of 0 for the remaining NAs
ab_fill<-ab_rm
naid<-which(is.na(ab_fill),arr.ind = T)
ab_fill[naid]<-0
head(ab_fill)
#re-nromalizing relative abundance
totals<-apply(ab_fill[-1],FUN=sum,MARGIN=2)/2000#looking at the sums of original data, the sum is 1958~2000
nsamples<-ncol(ab_fill)-1
ab_norm<-ab_fill
for(n in 1:nsamples){
  ab_norm[1+n]<-ab_norm[1+n]/totals[n]
}
head(ab_norm)
#doing speraman correlation test and hence deducing which OTUs are connected(p<=0.05)
OTUs<-ab_norm[1]
ab_norm_val<-ab_norm[-1]
alpha<-0.05
n_OTUS<-nrow(OTUs)
edgelist_vec<-c()
for(n in 2:n_OTUS){
  x<-unlist(ab_norm_val[n,])
  OTUn<-OTUs[n,]
  for(m in 1:(n-1)){
    y<-unlist(ab_norm_val[m,])
    OTUm<-OTUs[m,]
    set.seed(96)
    p<-as.numeric(pvalue(spearman_test(x~y,distribution = approximate(nresample  = 9999))))
    if(p<=alpha){
      edgelist_vec<-c(edgelist_vec,OTUn,OTUm)
    }
  }
}
edge_list<-matrix(edgelist_vec,byrow=T,ncol=2)
#creating a graph and saving graph and edgelist
set.seed(96)
graph<-graph_from_edgelist(edge_list,directed=F)
write.table(edge_list,file="D:/eco-homework_2024/edge_list.txt",sep=" ")
write_graph(graph,file="D:/eco-homework_2024/graph.txt",format="edgelist")#apparently, this method erases all the node names
set.seed(96)
plot(graph,vertex.size = 4,vertex.label.cex = 0.5, xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8))
#converting the data to adjacency matrix
set.seed(96)
adj<-as_adjacency_matrix(graph,sparse=F)
adj
##exploring network properties
#analysing vertex properties
degrees<-apply(adj,FUN=sum,MARGIN=1)
degrees
min<-min(degrees)
max<-max(degrees)
bin_width<-2
n_bins<-ceiling((max-min)/2+0.5)
boundaries<-min+((0:n_bins)-0.5)*bin_width
deg_histo_data<-c()
for(n in 1:n_bins){
  deg_histo_data[n]<-sum((degrees>boundaries[n])&(degrees<=boundaries[n+1]))
}
deg_val<-min+(0:(n_bins-1))*bin_width
#fitting for curve kx^(-gamma) to find the gamma constant for degree distribution(algorithm:gradient descend least square)
k<-mean(deg_histo_data)
gamma<-1
eps<-0.001
going<-T
lambda<-0.001#tune this number if encountering convergence issues
zero_id<-which(deg_histo_data==0)
if(sum(deg_histo_data==0)>0){
 deg_histo_data<-deg_histo_data[-zero_id]
 deg_val<-deg_val[-zero_id]
}
while(going){
  diff<-deg_histo_data-k*deg_val^(-gamma)
  partial_k_terms<--deg_val^(-gamma)
  partial_gamm_terms<--log(deg_val)*partial_k_terms
  partial_k<-2*partial_k_terms%*%diff
  partial_gamma<-2*partial_gamm_terms%*%diff
  k<-k-lambda*partial_k
  gamma<-gamma-lambda*partial_gamma
  modulus<-sqrt(partial_k^2+partial_gamma^2)
  if(modulus<=eps){
    going<-F
  }
}#using cumulative distribution could potentially yield a better fit, the math is a a tiny bit more complicated, but the idea is the same
c(k=k,gamma=gamma)
curv_fun<-function(x){
  return(k*(x^(-gamma)))
}
degree_plot<-ggplot(data.frame(degrees), aes(x = degrees))+geom_histogram(binwidth = 2, fill = "skyblue", color = "skyblue",center=1) +stat_function(fun = curv_fun, color = "red")+labs(title = "Degre distribution",x = "Degree",y = "Frequency")+annotate("text",x=(min+max)/2,y=0.5*max(deg_histo_data),label=paste0("Freq=",as.character(round(k,3)),"Deg^-",as.character(round(gamma,3))))
degree_plot
#writing a color mapping function
colormap<-function(t){
  t<-max(t,0)%>%
    min(1)
  if(t<0.5){
    color<-(t*c(1,1,1)+(0.5-t)*c(0,0,1))*2
  }
  else{
    color<-((t-0.5)*c(1,0,0)+(1-t)*c(1,1,1))*2
  }
  color<-rgb(color[1],color[2],color[3])
  return(color)
}#red is highets, blue id lowest
#adding degree property onto the graph plot
deg_color<-(degrees-min)/(max-min)
deg_color<-lapply(deg_color,FUN=colormap)%>%
  unlist()
set.seed(96)
plot(graph,vertex.size = 4,vertex.label.cex = 0.5, xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8),vertex.color=deg_color)
#analyzing closeness and betweeness properties
between<-betweenness(graph)
between
between_plot<-ggplot(data.frame(between), aes(x = between))+geom_histogram(binwidth = 20, fill = "skyblue", color = "skyblue",center=10) +labs(title = "Betweenness distribution",x = "Betweenness",y = "Frequency")
between_plot
between_color<-(between-min(between))/(max(between)-min(between))
between_color<-lapply(between_color,FUN=colormap)%>%
  unlist()
set.seed(96)
plot(graph,vertex.size = 4,vertex.label.cex = 0.5, xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8),vertex.color=between_color)
closeness<-closeness(graph)
closeness
closeness_plot<-ggplot(data.frame(closeness), aes(x = closeness))+geom_histogram(binwidth = 0.0001, fill = "skyblue", color = "skyblue",center=0.00005) +labs(title = "Closeness distribution",x = "Closeness",y = "Frequency")
closeness_plot
closeness_color<-(closeness-min(closeness))/(max(closeness)-min(closeness))
closeness_color<-lapply(closeness_color,FUN=colormap)%>%
  unlist()
set.seed(96)
plot(graph,vertex.size = 4,vertex.label.cex = 0.5, xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8),vertex.color=closeness_color)
#analysing edge properties
#distribution of lenghts of shortest paths
dt<-distance_table(graph)
dt
dt<-dt[[1]]
distances<-1:length(dt)
dt_df<-data.frame(distance=distances,frequency=dt)
dt_plot<-ggplot(dt_df,aes(x=distance,y=frequency))+geom_bar(stat="identity",fill = "skyblue", color = "skyblue",width = 1)
dt_plot
#calculating edge betweenness centrailty
ebc<-edge_betweenness(graph)
ebc
ebc_plot<-ggplot(data.frame(ebc), aes(x = ebc))+geom_histogram(binwidth = 4, fill = "skyblue", color = "skyblue",center=2) +labs(title = "Edg_bet_cent distribution",x = "Edge betweeness centrality",y = "Frequency")
ebc_plot
ebc_width<-3*ebc/(max(ebc))
set.seed(96)
plot(graph,vertex.size = 4,vertex.label.cex = 0.5, xlim = c(-0.8, 0.8),ylim = c(-0.8, 0.8),edge.width=ebc_width)
#evaluating network level properties
#rank and degree of the graph
order<-length(unique(edgelist_vec))
degree<-sum(degrees)/2
av_deg<-degree/order
sfc<-gamma
connectivity<-degree/(0.5*order*(order-1))
summary<-c(order=order,degree=degree,average_degree=av_deg,scale_free_constant=sfc,connectivity=connectivity)
summary
