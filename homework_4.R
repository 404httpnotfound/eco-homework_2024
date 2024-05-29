##################
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about
#preprocessing machine learning data(filling in NaNs, rangeing,data partitioning)
#feature selection
#trainning random forest model and parameter tuning
#evaluating the performance of model
##################
#loading packages and data.
library(caret)
library(tidyverse)
library(ggplot2)
library(skimr)
mtdata<-mtcars
ncols<-ncol(mtdata)
nrows<-nrow(mtdata)
#marking which will be treated as dummy vars, and which one is the target
dummy_names<-c("vs","am")
target_name<-"mpg"
#preprocesing:removing NAs(if there is any, in this case there is none) by doing knn clustering imputation
isna<-is.na(mtdata)
if(0|sum(isna)){
  cols<-colnames(mtdata)
  mtdata_nona<-na.omit(mtdata)
  for(n in 1:ncols){
    if(0|sum(isna[,n])){
      col_this<-cols[n]
      set.seed(96)
      imputer<-train(mtdata_nona[,-n],mtdata_nona[,n],method="knn")
      for(k in 1:nrows){
        if(is.na(mtdata[k,n])){
          imputed=predict(imputer,mtdata[k,-n])
          if(0|sum(col_this==dummy_names)){
            mtdata[k,n]<-round(imputed,0)
          }
          else{
            mtdata[k,n]<-imputed
          }
        }
      }
    }
  }
}
#preprocesssing:putting data between 0 and 1
predictors<-select(mtdata,-as.name(target_name))
set.seed(96)
ranger<-preProcess(predictors,method="range")
predictors<-predict(ranger,newdata=predictors)
mtdata<-cbind(mtdata[target_name],predictors)
#preprocessing:creating dummy variables
mtdata[dummy_names]<-as.data.frame(apply(mtdata[dummy_names],2,as.character))
dummy_cols_formula<-as.formula(paste("~",paste(dummy_names,collapse = "+"),sep=""))
set.seed(96)
dummies<-dummyVars(dummy_cols_formula,data=mtdata)
dummy_cols<-predict(dummies,newdata=mtdata)
mtdata<-cbind(select(mtdata,-as.name(dummy_names)),dummy_cols)
mtdata
#preprocessing:data set partitioning
set.seed(96)
trainindex<-createDataPartition(mtdata[target_name][,1],p=0.8,list=FALSE,times=1)
traindata<-mtdata[trainindex,]
validationdata1<-mtdata[-trainindex,]
#first round of training and and feature selection
traincontrol<-trainControl(method="repeatedcv", number=10, repeats=10)
set.seed(96)
forest_raw1<-train(select(traindata,-as.name(target_name)),unlist(traindata[target_name]),method="rf",trainControl=traincontrol,metric="RSE")
forest_raw1
importance<-varImp(forest_raw1,scale=TRUE)
plot(importance)
#selecting the top 8 most important variables
cutoff<-8#feel free to tune this parameter, may well put the whole thing in a for loop
selected_features<-importance[1][1]%>%
  data.frame()%>%
  arrange(-Overall)%>%
  rownames()%>%
  .[1:cutoff]
selected_features
traindata_selected<-traindata[c(target_name,selected_features)]
validation_data2<-validationdata1[c(target_name,selected_features)]
#getting new model after feature selection
set.seed(96)
forest_raw2<-train(select(traindata_selected,-as.name(target_name)),unlist(traindata_selected[target_name]),method="rf",trainControl=traincontrol,metric="RSE")
forest_raw2
#training and parameter tuning for mtree and ntree
mtrees1<-2:(ncol(traindata)-1)
mtrees2<-2:(ncol(traindata_selected)-1)
ntree_multiplier<-c(3,5,7,10,14,20,30)#feel free to tune these numbers
ntrees1<-ncol(traindata)*c(3,5,7,10,14,20,30)
ntrees2<-ncol(traindata_selected)*c(3,5,7,10,14,20,30)
mtrygrid1<-expand.grid(mtry=mtrees1)
mtrygrid2<-expand.grid(mtry=mtrees2)
traincontrol_search<-trainControl(method="repeatedcv", number=10, repeats=10,search="grid")
rmses1<-c()
rmses2<-c()
trials<-length(ntree_multiplier)
for(n in 1:trials){
  forestname1<-paste("forest1_tune",as.character(n),sep="")
  forestname2<-paste("forest2_tune",as.character(n),sep="")
  set.seed(96)
  forest_n1<-train(select(traindata,-as.name(target_name)),unlist(traindata[target_name]),method="rf",tuneGrid =mtrygrid1,trainControl=traincontrol,ntree=ntrees1[n],metric="RSE")
  set.seed(96)
  forest_n2<-train(select(traindata_selected,-as.name(target_name)),unlist(traindata_selected[target_name]),method="rf",tuneGrid =mtrygrid2,trainControl=traincontrol,ntree=ntrees2[n],metric="RSE")
  assign(forestname1,forest_n1)
  assign(forestname2,forest_n2)
  prediction1<-predict(forest_n1,select(traindata,-as.name(target_name)),)
  prediction2<-predict(forest_n2,select(traindata,-as.name(target_name)),)
  rmse1<-RMSE(prediction1,unlist(traindata[target_name]))
  rmse2<-RMSE(prediction2,unlist(traindata_selected[target_name]))
  rmses1[n]<-rmse1
  rmses2[n]<-rmse2
}
rmses_df1<-data.frame(features=c("all_features"),ntree=ntrees1,rmse=rmses1)
rmses_df2<-data.frame(features=c(paste(as.character(cutoff),"features_selected",sep="_")),ntree=ntrees2,rmse=rmses2)
rmses_df<-rbind(rmses_df1,rmses_df2)
rmse_plot<-ggplot(data=rmses_df,aes(x=ntree,y=rmse,color=features))
rmse_plot%>%
  +geom_point(alpha=0.5)+theme_bw()+theme(panel.grid = element_line(),plot.background=element_blank())+labs(x="ntree",y="RMSE")+ylim(0.9,1.15)+facet_wrap(~features)+geom_line()
#selecting best forests from the parameter search
best_trial1<-which.min(rmses1)
best_trial2<-which.min(rmses2)
forest_raw3<-get(paste("forest1_tune",as.character(best_trial1),sep=""))
forest_raw4<-get(paste("forest2_tune",as.character(best_trial2),sep=""))
forest_raw3
forest_raw4
#selecting the best forest out of the all tried
predict1<-predict(forest_raw1,select(traindata,-as.name(target_name)))
predict2<-predict(forest_raw2,select(traindata_selected,-as.name(target_name)))
rmse1<-RMSE(predict1,unlist(traindata[target_name]))
rmse2<-RMSE(predict2,unlist(traindata_selected[target_name]))
rmse3<-rmses1[best_trial1]
rmse4<-rmses2[best_trial2]
best_forest_index<-which.min(c(rmse1,rmse2,rmse3,rmse4))
best_forest<-get(paste("forest_raw",as.character(best_forest_index),sep=""))
parity<-2-(best_forest_index%%2)
features_sorted=!(2-parity)
{
  if(best_forest_index<=2){
  ntree_final="AUTO"
  }
  else{
    ntrees_list<-get(paste("ntrees",as.character(parity),sep=""))
    best_trial_used<-get(paste("best_trial",as.character(parity),sep=""))
    ntree_final<-ntrees_list[best_trial_used]
  }
}
mtry_final<-best_forest["bestTune"][[1]][1,1]
#the forest used
best_forest
features_sorted
mtry_final
ntree_final
#validation of the used model
validation_data<-get(paste("validation_data",as.character(parity),sep=""))
validation_real<-unlist(validation_data[target_name])
validation_predict<-predict(best_forest,select(validation_data,-as.name(target_name)))
validation_predict_df<-data.frame(prediction=validation_predict)
validation_data%>%
  cbind(validation_predict_df)
rmse_validation=RMSE(validation_predict,validation_real)
validation_data
rmse_validation
