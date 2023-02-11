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
library(readxl)

#======== Section 1: Load required dataset here =================

file_str_esg_score <- "kmeans_final_esgScore_cluster"
simulation_file_name <- "simple_simulation10000_normalReturns_cluster_6"
output_file_name <- "cluster_6_input_normalReturns_simple.RData"

#======== Section 2: Specify the settings for the script =================

#For which investment strategy do you want to get output for
#Choice between 'Dynamic', 'Myopic' or 'BuyHold'
investment_strategy = 'Dynamic'

#The discount factor to be used
#Set to 0.97, to account for a 2 to 3% inflation for the future
discount_factor <- 1 

#Split between training and testing set
train_ratio <- 0.8

#The maximum horizon for the asset allocation, in years
max_horizon <- 15

#choose from following across path models: OLS (linear regression), RT (Regression tree), RF (Random forest),
#and XGB (xg Boosting)
model_type <- 'OLS'
#Choose whether to perform hyperparameter tuning or not
#Note that if running hyperparameter tuning, you cannot obtain optimal allocation from get_optimal_allocation
hyperParm_tuning <- F
#Choose whether to perform the complexity
complexity <- F
#Choose whether you want to perform part 1 of part 2 of the complexity
#Input either 1 or 2
which_complexity = 1
#Choose whether you want to ESG-restrict the optimal asset allocation
ESG_constraint <- F
#what is the ESG threshold you want to apply?
ESG_threshold <- 65
#what is the risk preference of the investor, defined by gamma?
gamma <- 5

#these are the weights vector among the 7 specifications of ESG preferences
env_weight_list <- c(0.33,0.7,0.1,0.2,1,0,0)
soc_weight_list <- c(0.33,0.2,0.8,0.1,0,1,0)
gov_weight_list <- c(0.33,0.1,0.1,0.7,0,0,1)

#set seed for random number generator
seed <- 10

#======== Section 3: load functions here =================

obtain_return_variables <- function(excel_file_name){
  #this function obtains the simulated return variables and combines them into a list
  
  file_location = paste("C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/",as.character(excel_file_name),".xlsx",sep='')
  
  sheetnames = c( "Tbill return",
                  "Tnote return",
                  "corBond return",
                  "cluster_return_1",
                  "cluster_return_2",
                  "cluster_return_3",
                  "cluster_return_4",
                  "cluster_return_5",
                  "cluster_return_6",
                  "cluster_return_7",
                  "cluster_return_8")
  
  objectnames = c( "Tbill_return",
                   "Tnote_return",
                   "corBond_return",
                   "cluster_return_1",
                   "cluster_return_2",
                   "cluster_return_3",
                   "cluster_return_4",
                   "cluster_return_5",
                   "cluster_return_6",
                   "cluster_return_7",
                   "cluster_return_8")
  
  #print("cluster_return_6")
  #assign("cluster_return_6", openxlsx::read.xlsx(file_location, sheet = "cluster_return_6", colNames = F))
  
  for (i in 1:length(sheetnames)){
    print(sheetnames[i])
    assign(objectnames[i], read_excel(file_location, sheet = sheetnames[i], col_names = F))
  }
  
  # Change variables to matrices instead of dataframes
  Nscenarios = nrow(Tbill_return)
  Tbill_return = matrix(unlist(Tbill_return),nrow=Nscenarios)
  Tnote_return = matrix(unlist(Tnote_return),nrow=Nscenarios)
  corBond_return = matrix(unlist(corBond_return),nrow=Nscenarios)
  cluster_return_1 = matrix(unlist(cluster_return_1),nrow=Nscenarios)
  cluster_return_2 = matrix(unlist(cluster_return_2),nrow=Nscenarios)
  cluster_return_3 = matrix(unlist(cluster_return_3),nrow=Nscenarios)
  cluster_return_4 = matrix(unlist(cluster_return_4),nrow=Nscenarios)
  cluster_return_5 = matrix(unlist(cluster_return_5),nrow=Nscenarios)
  cluster_return_6 = matrix(unlist(cluster_return_6),nrow=Nscenarios)
  cluster_return_7 = matrix(unlist(cluster_return_7),nrow=Nscenarios)
  cluster_return_8 = matrix(unlist(cluster_return_8),nrow=Nscenarios)
  
  return(list( return_variable_1 = Tbill_return,
               return_variable_2 = Tnote_return,
               return_variable_3 = corBond_return,
               return_variable_4 = cluster_return_1,
               return_variable_5 = cluster_return_2,
               return_variable_6 = cluster_return_3,
               return_variable_7 = cluster_return_4,
               return_variable_8 = cluster_return_5,
               return_variable_9 = cluster_return_6,
               return_variable_10 = cluster_return_7,
               return_variable_11 = cluster_return_8
  ))
}

obtain_state_variables <- function(excel_file_name){
  #this function obtains the simulated state variables and combines them into a list
  
  file_location = paste("C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/",as.character(excel_file_name),".xlsx",sep='')
  
  sheetnames = c( "real rate",
                  "yield spread",
                  "credit spread",
                  "PE ratio",
                  "temperature change",
                  "log inflation rate")
  
  objectnames = c("real_rate",
                  "yield_spread",
                  "credit_spread",
                  "PE_ratio",
                  "temperature_change",
                  "log_inflation_rate")
  
  for (i in 1:length(sheetnames)){
    print(sheetnames[i])
    assign(objectnames[i], read_excel(file_location, sheet = sheetnames[i], col_names = F))
  }
  
  # Change variables to matrices instead of dataframes
  Nscenarios = nrow(real_rate)
  real_rate = matrix(unlist(real_rate),nrow=Nscenarios)
  yield_spread = matrix(unlist(yield_spread),nrow=Nscenarios)
  credit_spread = matrix(unlist(credit_spread),nrow=Nscenarios)
  PE_ratio = matrix(unlist(PE_ratio),nrow=Nscenarios)
  temperature_change = matrix(unlist(temperature_change),nrow=Nscenarios)
  log_inflation_rate = matrix(unlist(log_inflation_rate),nrow=Nscenarios)
  
  return(list( state_variable_1 = real_rate,
               state_variable_2 = yield_spread,
               state_variable_3 = credit_spread,
               state_variable_4 = PE_ratio,
               state_variable_5 = temperature_change,
               state_variable_6 = log_inflation_rate
  ))
}

#======== Section 4: generate a list of final ESG score per ESG cluster =================

#Load the final esg scores, for every ESG cluster preference
final_esg_score_list <- list()
for (i in 0:6){
  complete_str <- paste(file_str_esg_score,as.character(i),".xlsx",sep="")
  final_esg_score <- read.xlsx(complete_str)
  final_esg_score <- final_esg_score[,2:ncol(final_esg_score)]
  final_esg_score_list[[as.character(i)]] <- final_esg_score
}

monthly_return_var_list <- obtain_return_variables(simulation_file_name)
monthly_state_var_list <- obtain_state_variables(simulation_file_name)
# save(monthly_return_var_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/monthly_return_var_list_simple.RData")
# save(monthly_state_var_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/monthly_state_var_list_simple.RData")

#code to run this is above
#BEFORE RUNNING SENSITIVITY ANALYSIS, CHANGE THIS FILE
#load("C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/monthly_return_var_list_simple.RData")
#load("C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/monthly_state_var_list_simple.RData")

#======== Section 5: create the yearly return and state variables, using either averages or cumulatives =================

return_var_list <- list()
state_var_list <- list()

#create the annual returns, by calculating the cumulative returns within a specific year
for (var_name in names(monthly_return_var_list)){
  monthly_return_var_list[[var_name]] <- monthly_return_var_list[[var_name]] + 1
  
  return_var_list[[var_name]] <- matrix(0,nrow(monthly_return_var_list[[var_name]]),max_horizon)
  for (year in (1:max_horizon)){
    first_col <- 1 + ((year-1)*12)
    last_col <- year*12
    cum_returns <- t(apply(monthly_return_var_list[[var_name]][,first_col:last_col],1,cumprod))
    cum_returns <- cum_returns[,ncol(cum_returns)]
    return_var_list[[var_name]][,year] <- cum_returns
  }
}

average_state_var <- c("state_variable_2","state_variable_3","state_variable_4")
cumulative_state_var <- c("state_variable_1","state_variable_5","state_variable_6")

#create the annualised state variables. Depending on the type of state variable,
#you either take the average or the cumulative
#see above which ones are averages and which ones are cumulative state variables when annualised
for (var_name in names(monthly_state_var_list)){
  if (length(cumulative_state_var[cumulative_state_var==var_name])>0){
    monthly_state_var_list[[var_name]] <- monthly_state_var_list[[var_name]] + 1
    
    state_var_list[[var_name]] <- matrix(0,nrow(monthly_state_var_list[[var_name]]),max_horizon)
    for (year in (1:max_horizon)){
      first_col <- 1 + ((year-1)*12)
      last_col <- year*12
      cum_returns <- t(apply(monthly_state_var_list[[var_name]][,first_col:last_col],1,cumprod))
      cum_returns <- cum_returns[,ncol(cum_returns)]
      state_var_list[[var_name]][,year] <- cum_returns
    }
  } else {
    state_var_list[[var_name]] <- matrix(0,nrow(monthly_state_var_list[[var_name]]),max_horizon)
    for (year in (1:max_horizon)){
      first_col <- 1 + ((year-1)*12)
      last_col <- year*12
      av_returns <- t(apply(monthly_state_var_list[[var_name]][,first_col:last_col],1,mean))
      state_var_list[[var_name]][,year] <- av_returns
    }
  }
}

#======== Section 6: Train test split =================

# generate a training and test split among the 10000 scenarios
# an 80/20% training/testing split is performed
training_subset <- sort(sample(nrow(return_var_list$return_variable_1), nrow(return_var_list$return_variable_1)*train_ratio))

return_var_train_list <- list()
return_var_test_list <- list()
state_var_train_list <- list()
state_var_test_list <- list()

for (var_name in names(return_var_list)){
  return_var_train_list[[var_name]] <- return_var_list[[var_name]][training_subset,]
  return_var_test_list[[var_name]] <- return_var_list[[var_name]][-training_subset,]
}

for (var_name in names(state_var_list)){
  state_var_train_list[[var_name]] <- state_var_list[[var_name]][training_subset,]
  state_var_test_list[[var_name]] <- state_var_list[[var_name]][-training_subset,]
}

#to keep the training and test set the same, we use stored RData files for the above
#save(return_var_train_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/return_var_train_list_simple.RData")
#save(return_var_test_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/return_var_test_list_simple.RData")
#save(state_var_train_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/state_var_train_list_simple.RData")
#save(state_var_test_list,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/state_var_test_list_simple.RData")

#load the training and testing sets
# load("return_var_train_list_kmeans.RData")
# load("return_var_test_list_kmeans.RData")
# load("state_var_train_list_kmeans.RData")
# load("state_var_test_list_kmeans.RData")

#======== Section 7: Create the RData file for the numerical model =================
rm(monthly_return_var_list)
rm(monthly_state_var_list)
rm(return_var_list)
rm(state_var_list)
rm(av_returns)
rm(training_subset)
rm(cum_returns)
save.image(file=output_file_name)
