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