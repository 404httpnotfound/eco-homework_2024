##################
#Author: Xin Ming
#created on:2024.05.25
#xm123456@mail.ustc.edu.cn
#this is a homework for Data Driven Method of Ecological Research class 2024
#this homework is about：
#constructing ts timeseries
#extracting features (lag features, sliding window stats and fourier terms)
#machine learning for forecasting time series(same model iterative forecasting vs recursive training-prediction)
##################

##creating a ts object and visualizing it
#downloading and loading in data 
library(tidyverse)
library(tidymodels)
library(tseries)
library(zoo)
library(timetk)
library(recipes)
#updating function for updating sliding window features when forecasting
updater<-function(old_features,newval){
  new_features<-rev(old_features)%>%
    .[-(1:3)]%>%
    c(newval)%>%
    rev()
  values<-new_features
  max<-max(values)
  min<-min(values)
  new_features<-c(new_features,max)%>%
    c(min)
  return(new_features)
}
download.file(url="https://figshare.com/ndownloader/files/39686458",destfile="D:/eco-homework_2024/ts_data.txt")
ts_file<-read.csv("D:/eco-homework_2024/ts_data.txt",sep=" ")
head(ts_file)
#filtering out species VAI and station VERCah, and removing NAs
ts_df<-ts_file%>%
  filter(STATION=="VERCah"&SP=="VAI")%>%
  select(-c("X_L93","Y_L93","BIOMASS"))%>%
  drop_na()
#taking the mean of simultanous observations and removing duplicates
dates<-unique(ts_df$DATE)
for(date in dates){
  rows<-filter(ts_df,DATE==date)
  mean_dens<-mean(rows$DENSITY)
  ts_df$DENSITY[which(ts_df$DATE==date)]<-mean_dens
}
ts_df<-distinct(ts_df)
ts_df
#filling in missing data with lerp
startyear<-min(ts_df$YEAR)
endyear<-max(ts_df$YEAR)
years<-(startyear:endyear)
n_year_original<-nrow(ts_df)
density_lerped<-c()
for(n in (1:(n_year_original-1))){
  density_lerped<-c(density_lerped,ts_df$DENSITY[n])
  if((ts_df$YEAR[n+1]-ts_df$YEAR[n])>1){
    diff<-ts_df$YEAR[n+1]-ts_df$YEAR[n]
    density_prev<-ts_df$DENSITY[n]
    density_after<-ts_df$DENSITY[n+1]
    k<-(1:(diff-1))
    density_lerped_local<-((diff-k)*density_prev+k*density_after)/diff
    density_lerped<-c(density_lerped,density_lerped_local)
  }
}
density_lerped<-c(density_lerped,ts_df$DENSITY[n_year_original])
lerped_df<-data.frame(YEAR=years,DENSITY=density_lerped)
#converting data to a ts object and visualizing it
dens_ts<-ts(lerped_df,start=startyear,frequency=1)
dens_ts
class(dens_ts)
plot(dens_ts[,2])

##extracting features in preparation for machine learning
#converting the data into a tibble
dens_ts_tbl<-dens_ts%>%
  tk_tbl()%>%
  select(-index)
class(dens_ts_tbl)
dens_ts_tbl
#analysing lag autocorrelation
n_year<-nrow(dens_ts_tbl)
max_lag_allowed<-20
cors<-c()
for(n in 1:max_lag_allowed){
  cors<-c(cors,cor(dens_ts_tbl$DENSITY[-(1:n)],dens_ts_tbl$DENSITY[-(n_year-0:(n-1))]))
}
cors_df<-data.frame(lag=1:max_lag_allowed,autocorrelation=cors)
plot(cors_df)
#adding lag features
window_size<-5#feel free to change this number
dens_lags<-embed(dens_ts_tbl$DENSITY,window_size+1)
colnames(dens_lags)<-c("DENSITY",paste("lag",as.character(1:window_size),sep=""))
dens_ts_lag_tbl<-cbind(data.frame(YEAR=dens_ts_tbl$YEAR[-(1:window_size)]),dens_lags)
dens_ts_lag_tbl
#plotting time series and using linear fitting to fit for the data
lm_lag<-lm(as.formula(paste("DENSITY",paste(colnames(dens_ts_lag_tbl)[-2],collapse="+"),sep="~")),data=dens_ts_lag_tbl)
lm_lag
fit_lag<-predict(lm_lag,dens_ts_lag_tbl[-2])
fit_df<-data.frame(YEAR=dens_ts_lag_tbl$YEAR,DENSITY=dens_ts_lag_tbl$DENSITY,fit_lag<-fit_lag)
R2_lag<-round(cor(dens_ts_lag_tbl$DENSITY,fit_lag)^2,3)
lin_fit_plot<-ggplot()+geom_line(data=fit_df,aes(x=YEAR,y=DENSITY,color="DENSITY"))+geom_line(data=fit_df,aes(x=YEAR,y=fit_lag,color="lags"))+theme_minimal()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.title = element_text(size = 12, face = "bold"),legend.text = element_text(size = 10),legend.position = "bottom" )
lin_fit_plot+scale_color_manual(name="", values = c("DENSITY" = "black", "lags" = "cyan"),labels = c("DENSITY" = "DENSITY", "lags" = paste0("lag R^2=",as.character(R2_lag))))
#adding moving window statistics(average,max)
dens_lags<-dens_lags[,-1]
#mean<-apply(dens_lags,MARGIN = 1,FUN = mean)#mean removed due to error induced by co-linearity with the lags
max<-apply(dens_lags,MARGIN = 1,FUN = max)
min<-apply(dens_lags,MARGIN = 1,FUN = min)
dens_ts_lag_win_tbl<-dens_ts_lag_tbl%>%
#  cbind(mean)%>%
  cbind(max)%>%
  cbind(min)
#plotting time series and using linear fitting to fit for the data
lm_lag_win<-lm(as.formula(paste("DENSITY",paste(colnames(dens_ts_lag_win_tbl)[-2],collapse="+"),sep="~")),data=dens_ts_lag_win_tbl)
lm_lag_win
fit_lag_win<-predict(lm_lag_win,dens_ts_lag_win_tbl[-2])
fit_df<-cbind(fit_df,fit_lag_win)
R2_lag_win<-round(cor(dens_ts_lag_win_tbl$DENSITY,fit_lag_win)^2,3)
lin_fit_plot<-lin_fit_plot+geom_line(data=fit_df,aes(x=YEAR,y=fit_lag_win,color="lags+win"))
lin_fit_plot+scale_color_manual(name="", values = c("DENSITY" = "black", "lags" = "cyan","lags+win"="blue"),labels = c("DENSITY" = "DENSITY", "lags" = paste0("lag R^2=",as.character(R2_lag)),"lags+win"=paste0("lag+win R^2=",as.character(R2_lag_win))))
#adding fourier term features
max_freq<-5
period<-n_year-window_size
years_trimmed<-dens_ts_lag_win_tbl$YEAR
dens_ts_lag_win_tri_tbl<-dens_ts_lag_win_tbl
colnames<-colnames(dens_ts_lag_win_tri_tbl)
for(n in 1:max_freq){
  cosname<-paste0("cos",as.character(n))
  sinname<-paste0("sin",as.character(n))
  dens_ts_lag_win_tri_tbl<-dens_ts_lag_win_tri_tbl%>%
    cbind(fourier_vec(years_trimmed,period=period,K=n,type="cos"))%>%
    cbind(fourier_vec(years_trimmed,period=period,K=n,type="sin"))
  colnames<-colnames%>%
    c(cosname)%>%
    c(sinname)
}
colnames(dens_ts_lag_win_tri_tbl)<-colnames
#plotting time series and using linear fitting to fit for the data
lm_lag_win_tri<-lm(as.formula(paste("DENSITY",paste(colnames(dens_ts_lag_win_tri_tbl)[-2],collapse="+"),sep="~")),data=dens_ts_lag_win_tri_tbl)
lm_lag_win_tri
fit_lag_win_tri<-predict(lm_lag_win_tri,dens_ts_lag_win_tri_tbl[-2])
fit_df<-cbind(fit_df,fit_lag_win_tri)
R2_lag_win_tri<-round(cor(dens_ts_lag_win_tri_tbl$DENSITY,fit_lag_win_tri)^2,3)
lin_fit_plot<-lin_fit_plot+geom_line(data=fit_df,aes(x=YEAR,y=fit_lag_win_tri,color="lags+win+tri"))
lin_fit_plot+scale_color_manual(name="", values = c("DENSITY" = "black", "lags" = "cyan","lags+win"="blue","lags+win+tri"="purple"),labels = c("DENSITY" = "DENSITY", "lags" = paste0("lag R^2=",as.character(R2_lag)),"lags+win"=paste0("lag+win R^2=",as.character(R2_lag_win)),"lags+win+tri"=paste0("lag+win+tri R^2=",as.character(R2_lag_win_tri))))

##Machine learning(comparison between same model prediction and rewcursive training prediction, using random forest and artificial neural network model)
#splitting into training and testing dataset
training_fraction<-0.8
train_index=1:round(training_fraction*period)
train_data<-dens_ts_lag_win_tri_tbl[train_index,]
train_data_ann<-train_data
train_data_rf<-train_data
updated_cols=2+(1:(2+window_size))
testing_data<-dens_ts_lag_win_tri_tbl[-train_index,]
#creating recipe for feeding the data into ml model,with a normalization pre-processing applied on the features 
recipe_rf<-colnames%>%
  .[-2]%>%
  paste(collapse="+")%>%
  paste0("DENSITY~",.)%>%
  as.formula()%>%
  recipe(data=train_data_rf)%>%
  step_normalize(all_predictors())
recipe_ann<-colnames%>%
  .[-2]%>%
  paste(collapse="+")%>%
  paste0("DENSITY~",.)%>%
  as.formula()%>%
  recipe(data=train_data_ann)%>%
  step_normalize(all_predictors())
#creating models and workflow(ANN vs RF)
model_ann<-mlp(mode="regression",hidden_units=6,engine="nnet")#feel free to add and tune parameters(layer num and size, activation function type ect)
model_rf<-rand_forest(mode="regression",engine="ranger")#feel free to add and tune parameters(mtry and ntree ect)
workflow_ann<-workflow()%>%
 add_recipe(recipe_ann)%>%
  add_model(model_ann)
workflow_rf<-workflow()%>%
  add_recipe(recipe_rf)%>%
  add_model(model_rf)
#training and prediction:single model, iterative prediction
set.seed(96)
trained_rf<-fit(workflow_rf,data=train_data_rf)
set.seed(96)
trained_ann<-fit(workflow_ann,data=train_data_ann)
prediction_length<-nrow(testing_data)
predicted_rf<-c()
predicted_ann<-c()
updated_features_rf<-unlist(testing_data[updated_cols][1,])
names(updated_features_rf)<-NULL
updated_features_ann<-updated_features_rf
for(n in 1:prediction_length){
  test_features_rf<-testing_data[n,]%>%
    .[-2]
  test_features_ann<-testing_data[n,]%>%
    .[-2]
  test_features_rf[updated_cols-1]<-updated_features_rf
  test_features_ann[updated_cols-1]<-updated_features_ann
  set.seed(96)
  pred_rf<-unlist(predict(trained_rf,new_data=test_features_rf))
  set.seed(96)
  pred_ann<-unlist(predict(trained_ann,new_data=test_features_ann))
  updated_features_rf<-updater(updated_features_rf,pred_rf)
  updated_features_ann<-updater(updated_features_rf,pred_ann)
  predicted_rf[n]<-pred_rf
  predicted_ann[n]<-pred_ann
}
results<-testing_data[1:2]%>%
  cbind(rf1=predicted_rf)%>%
  cbind(ann1=predicted_ann)
#training and prediction:recursive training and prediction
dynamic_train_data_rf<-train_data
dynamic_train_data_ann<-train_data
predicted_rf<-c()
predicted_ann<-c()
updated_features_rf<-unlist(testing_data[updated_cols][1,])
names(updated_features_rf)<-NULL
updated_features_ann<-updated_features_rf
for(n in 1:prediction_length){
  set.seed(96)
  trained_rf<-fit(workflow_rf,data=dynamic_train_data_rf)
  set.seed(96)
  trained_ann<-fit(workflow_ann,data=dynamic_train_data_ann)
  test_rf<-testing_data[n,]
  test_ann<-testing_data[n,]
  test_rf[updated_cols]<-updated_features_rf
  test_ann[updated_cols]<-updated_features_ann
  test_features_rf<-test_rf[-2]
  test_features_ann<-test_rf[-2]
  set.seed(96)
  pred_rf<-unlist(predict(trained_rf,new_data=test_features_rf))
  set.seed(96)
  pred_ann<-unlist(predict(trained_ann,new_data=test_features_ann))
  updated_features_rf<-updater(updated_features_rf,pred_rf)
  updated_features_ann<-updater(updated_features_ann,pred_ann)
  predicted_rf[n]<-pred_rf
  predicted_ann[n]<-pred_ann
  test_rf[2]<-pred_rf
  test_ann[2]<-pred_ann
  dynamic_train_data_rf<-dynamic_train_data_rf[-1,]%>%
    rbind(test_rf)
  dynamic_train_data_ann<-dynamic_train_data_ann[-1,]%>%
    rbind(test_ann)
}
results<-cbind(results,rf2=predicted_rf)%>%
  cbind(ann2=predicted_ann)
#evaluationg model performance and visdualizing results
pred_names<-colnames(results)[-(1:2)]
rmses<-c()
for(n in 1:length(pred_names)){
  rmses[n]<-rmse_vec(results[,2],results[,2+n])
}
colors<-c("red","yellow","cyan","purple")
names(rmses)<-pred_names
rmses
rmses<-round(rmses,3)
ml_plot<-ggplot()%>%
  +geom_line(data=fit_df,aes(x=YEAR,y=DENSITY,color="Actual"))%>%
  +geom_line(data=results,aes(x=YEAR,y=rf1,color="rf1"))%>%
  +geom_line(data=results,aes(x=YEAR,y=ann1,color="ann1"))%>%
  +geom_line(data=results,aes(x=YEAR,y=rf2,color="rf2"))%>%
  +geom_line(data=results,aes(x=YEAR,y=ann2,color="ann2"))%>%
  +theme_minimal()%>%
  +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.title = element_text(size = 12, face = "bold"),legend.text = element_text(size = 10),legend.position = "bottom" )%>%
  +scale_color_manual(name="", values = c("Actual" = "black", "rf1" = colors[1],"ann1"=colors[2],"rf2"=colors[3],"ann2"=colors[4]),labels = c("Actual" = "Actual", "rf1" = paste0("rf1 rmse=",as.character(rmses[1])),"ann1"=paste0("ann1 rmse=",as.character(rmses[2])),"rf2"=paste0("rf2 rmse=",as.character(rmses[3])),"ann2"=paste0("ann2 rmse=",as.character(rmses[4]))))
ml_plot  
  

