########################################################################### 
##  Start date: 2021-03-11 ------------------------------------------------
##  Author: NIKITA PAVLOV, ELINE LI, LUC HAAFKES, ELSEMIEKE GIJSSEL--------
##  Subject: allocation optimisation, hyperparameter tuning and complexity script
###########################################################################

#saved github commands for the terminal
# git reset --hard (input_SHA key of previous version)
# library(usethis)
# use_github(protocol='https', auth_token = Sys.getenv("GITHUB_PAT"))
# git config --global user.email "nikita.pavlov.sva@hotmail.com"
# git clone "clonable URL of repo"

#install packages (if not installed yet) and import packages
# install.packages('scales')
# install.packages('dplyr')
# install.packages('randomForest')
# install.packages('rpart')
# install.packages('xgboost')
# install.packages('openxlsx')
# install.packages('Rcpp')
# install.packages('mlr')
# install.packages('caret')
# install.packages("parallelMap")
# install.packages("moments")
# install.packages("writexl")
# install.packages('reticulate')
library(scales)
library(dplyr)
library(randomForest)
library(rpart)
library(xgboost)
library(openxlsx)
library(Rcpp)
library(mlr)
library(caret)
library(parallelMap)
library(moments)
library(writexl)
library(doParallel)
library(parallel)
library(MASS)

#======== Section 1: load RData files for descriptives ===================================================

load("return_var_train_list_simple.RData")
load("return_var_test_list_simple.RData")
load("state_var_train_list_simple.RData")
load("state_var_test_list_simple.RData")


#======== Section 2: calculate descriptive statistics for cumulative returns of return and state variables

descrp_train_df <- data.frame(c(names(return_var_train_list),names(state_var_train_list)))
names(descrp_train_df) <- c("variable")
descrp_train_df['mean'] <- 0
descrp_train_df['stdev'] <- 0
descrp_train_df['SR'] <- 0
descrp_train_df['skew'] <- 0
descrp_train_df['kurtosis'] <- 0
descrp_train_df['min'] <- 0
descrp_train_df['max'] <- 0


descrp_test_df <- data.frame(c(names(return_var_train_list),names(state_var_train_list)))
names(descrp_test_df) <- c("variable")
descrp_test_df['mean'] <- 0
descrp_test_df['stdev'] <- 0
descrp_test_df['SR'] <- 0
descrp_test_df['skew'] <- 0 
descrp_test_df['kurtosis'] <- 0
descrp_test_df['min'] <- 0
descrp_test_df['max'] <- 0

for (var_name in c(names(return_var_train_list),names(state_var_train_list))){
  print(var_name)
  if (length(names(return_var_train_list)[names(return_var_train_list)==var_name])>0){
    cum_returns_train <- return_var_train_list[[var_name]]
    cum_returns_test <- return_var_test_list[[var_name]]
  } else {
    cum_returns_train <- state_var_train_list[[var_name]]
    cum_returns_test <- state_var_test_list[[var_name]]
  }
  #cum_returns_train <- apply(return_var_train_list[[var_name]],1,mean)
  #cum_returns_train <- cum_returns_train[,ncol(cum_returns_train)]
  descrp_train_df[descrp_train_df[1]==var_name,'mean'] <- mean(cum_returns_train)
  descrp_train_df[descrp_train_df[1]==var_name,'stdev'] <- sd(cum_returns_train)
  descrp_train_df[descrp_train_df[1]==var_name,'min'] <- min(cum_returns_train)
  descrp_train_df[descrp_train_df[1]==var_name,'max'] <- max(cum_returns_train)
  descrp_train_df[descrp_train_df[1]==var_name,'skew'] <- mean(skewness(cum_returns_train))
  descrp_train_df[descrp_train_df[1]==var_name,'kurtosis'] <- mean(kurtosis(cum_returns_train))
  
  #cum_returns_test <- apply(return_var_test_list[[var_name]],1,mean)
  #cum_returns_test <- cum_returns_test[,ncol(cum_returns_test)]
  descrp_test_df[descrp_test_df[1]==var_name,'mean'] <- round(mean(cum_returns_test),3)
  descrp_test_df[descrp_test_df[1]==var_name,'stdev'] <- round(sd(cum_returns_test),3)
  descrp_test_df[descrp_test_df[1]==var_name,'min'] <- round(min(cum_returns_test),3)
  descrp_test_df[descrp_test_df[1]==var_name,'max'] <- round(max(cum_returns_test),3)
  descrp_test_df[descrp_test_df[1]==var_name,'skew'] <- round(mean(skewness(cum_returns_test)),3)
  descrp_test_df[descrp_test_df[1]==var_name,'kurtosis'] <- round(mean(kurtosis(cum_returns_test)),3)
}

descrp_train_df[,'mean'] <- descrp_train_df[,'mean'] - 1
descrp_train_df[,'min'] <- descrp_train_df[,'min'] - 1
descrp_train_df[,'max'] <- descrp_train_df[,'max'] - 1
descrp_train_df[,'SR'] <- round(descrp_train_df[,'mean']/descrp_train_df[,'stdev'],3)
descrp_train_df[,2:ncol(descrp_train_df)] <- round(descrp_train_df[,2:ncol(descrp_train_df)],3)

descrp_test_df[,'mean'] <- descrp_test_df[,'mean'] - 1
descrp_test_df[,'min'] <- descrp_test_df[,'min'] - 1
descrp_test_df[,'max'] <- descrp_test_df[,'max'] - 1
descrp_test_df[,'SR'] <- round(descrp_test_df[,'mean']/descrp_test_df[,'stdev'],3)
descrp_test_df[,2:ncol(descrp_test_df)] <- round(descrp_test_df[,2:ncol(descrp_test_df)],3)