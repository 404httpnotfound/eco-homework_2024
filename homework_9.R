##################
#Author: Xin Ming
#created on:2024-05-04
#xm123456@mail.ustc.edu.cn
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#which conditions of Moran’s I and Geary’s C indexes correspond to hotspots and outliers
#writing weight matrices for rook and queen contiguity
##################

##question1:
# 1.Local estimates of Moran’s I and Geary’s C are often used to identify spatial clustering/hotspots) 
#and outliers in attribute values. Please point out which conditions indicate hotspots and which ones 
#indicate outliers.

#Moran's I is a measure of spatial data similarity, it uses spacial auto-covariance of attribute values to
#to measure the similarity of neighbouring points. The higher the Moran's I index, the higher the spatial 
#autocorrelation is. The expected value of Moran's I is -1/(N-1), typically, numbers higher than this indicates
#a hotspot(ie similar numbers are close to each other), while lower than this indicates the presence of many
#outliers.
#Geary's C index measures spatial dissimilarity between points, via the use of weighted sum of difference of squares.
#the higher the Geary's C index, the less the spatial autocorrelation is. The ecpected value of Geary's C index is
#1, higher than one indicates strong dissimilarity(presence of outliers) and lower than 1 indicates presence of hotspots.

##question2:
#When working with lattice data, there are typically two forms of filters used for the weight matrix. 
#Please points the following filters, and give their spatial weights matrices, respectively. Also, 
#please write a line code for create the weighting matrix according to any one of the weight filters.

#the first one is a rook continguity filter, with the following weight matrix
# 0 1 0 1 0 0 0 0 0
# 1 0 1 0 1 0 0 0 0
# 0 1 0 0 0 1 0 0 0
# 1 0 0 0 1 0 1 0 0
# 0 1 0 1 0 1 0 1 0
# 0 0 1 0 1 0 0 0 1
# 0 0 0 1 0 0 0 1 0
# 0 0 0 0 1 0 1 0 1
# 0 0 0 0 0 1 0 1 0

#the second one is a queen continguity filter, with the following weight matrix
# 0 1 0 1 1 0 0 0 0
# 1 0 1 1 1 1 0 0 0
# 0 1 0 0 1 1 0 0 0
# 1 1 0 0 1 0 1 1 0
# 1 1 1 1 0 1 1 1 1
# 0 1 1 0 1 0 0 1 1
# 0 0 0 1 1 0 0 1 0
# 0 0 0 1 1 1 1 0 1
# 0 0 0 0 1 1 0 1 0

# code for the matrixes
rook_ones_ind<-c()
queen_ones_ind<-c()
ind<-1
for(n in 1:3){
  for(m in 1:3){
    for(i in 1:3){
      for(j in 1:3){
        if(xor(abs(n-i)==1&&m==j,abs(m-j)==1&&n==i)){
          rook_ones_ind<-c(rook_ones_ind,ind)
        }
        if(abs(n-i)<=1&&abs(m-j)<=1&&(!(n==i&&m==j))){
          queen_ones_ind<-c(queen_ones_ind,ind)
        }
        ind<-ind+1
      }
    }
  }
}
zeros=rep(0,9*9)
rook_vec<-zeros
rook_vec[rook_ones_ind]<-1
queen_vec<-zeros
queen_vec[queen_ones_ind]<-1
rook_mat<-matrix(rook_vec,nrow=9,ncol=9)
queen_mat<-matrix(queen_vec,nrow=9,ncol=9)
rook_mat
queen_mat
