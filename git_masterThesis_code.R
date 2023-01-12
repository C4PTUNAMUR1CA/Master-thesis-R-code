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
library(testPack2)

#======== Section 1: functions for hyperparameter tuning of ML models  ===============

RT_parameterTuning <- function(across_path_df){
  #create a train task
  trainTask <- makeRegrTask(data = across_path_df,target='y')
  
  #make tree learner
  makeatree <- makeLearner("regr.rpart", predict.type = "response")
  
  #set 5 fold cross validation
  set_cv <- makeResampleDesc("CV",iters = 5L)
  
  #Create a grid for the hyperparameters
  gs <- makeParamSet(
    makeIntegerParam("minsplit",lower = 10, upper = 50),
    makeIntegerParam("minbucket", lower = 5, upper = 50),
    makeNumericParam("cp", lower = 0.001, upper = 0.2))
  
  #set settings for a random search
  rancontrol <- makeTuneControlRandom(maxit = 60L)
  
  #hypertune the parameters
  stune <- tuneParams(learner = makeatree, resampling = set_cv, 
                      task = trainTask, par.set = gs, control = rancontrol, 
                      measures = mse,show.info = F)
  
  #return best parameters
  return(c(round(stune$x$cp),round(stune$x$minsplit),stune$x$minbucket))
}

RF_parameterTuning <- function(across_path_df){
  #create a train task
  trainTask <- makeRegrTask(data = across_path_df,target='y')
  
  #create a learner
  rf <- makeLearner("regr.randomForest", predict.type = "response")
  
  #set tunable parameters
  #grid search to find hyperparameters
  rf_param <- makeParamSet(
    makeIntegerParam("ntree",lower = 50, upper = 100),
    makeIntegerParam("mtry", lower = 1, upper = 7),
    makeIntegerParam("nodesize",lower = 1, upper = 15),
    makeIntegerParam("maxnodes",lower = 5, upper = 100))
  
  #set 5 fold cross validation
  set_cv <- makeResampleDesc("CV",iters = 5L)
  
  #do a random search for 60 iterations
  rancontrol <- makeTuneControlRandom(maxit = 60L)
  
  #hypertuning
  rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask,
                        par.set = rf_param, control = rancontrol,
                        measures = mse,show.info = F)
  
  #check best parameter
  return(c(rf_tune$x$ntree,rf_tune$x$mtry,rf_tune$x$nodesize,rf_tune$x$maxnodes))
}

XGB_parameterTuning <- function(across_path_df){
  #create a train task
  trainTask <- makeRegrTask(data = across_path_df,target='y')
  
  #create a learner and set objective and evaluation metric
  lrn_xgboost <- makeLearner('regr.xgboost',predict.type = 'response')
  lrn_xgboost$par.vals <- list(objective='reg:linear',eval_metric='error')
  
  #create grid for the hyperparameters to tune
  params <- makeParamSet(makeIntegerParam('nrounds',lower=500,upper=1000),
                         makeNumericParam("eta", lower=0.001, upper=0.3),
                         makeIntegerParam("max_depth",lower = 3,upper = 10),
                         makeIntegerParam("min_child_weight",lower = 1,upper = 8),
                         makeIntegerParam("early_stopping_rounds",lower = 5,upper = 10),
                         makeNumericParam("subsample",lower = 0.5,upper = 1),
                         makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                         makeNumericParam("gamma",lower = 0,upper = 2))
  
  #set up 5 K-fold validation
  resamp_xgboost <- makeResampleDesc("CV",iters=5L)
  #Set the settings for a random search
  param_search <- makeTuneControlRandom(maxit=60L)
  
  #hyperparameter tuning
  xg_tune <- tuneParams(learner = lrn_xgboost, task = trainTask,
                        resampling = resamp_xgboost, measures = mse,
                        par.set = params, control = param_search,
                        show.info = F)
  
  #return best parameters
  return(c(xg_tune$x$nrounds,xg_tune$x$eta,xg_tune$x$max_depth,
           xg_tune$x$gamma,xg_tune$x$colsample_bytree,xg_tune$x$min_child_weight,
           xg_tune$x$subsample,xg_tune$x$early_stopping_rounds))
}

Tuning_selector <- function(tuning_model,across_path_df){
  #obtains the optimal hyperparameters from the tuning, for the specified ML model
  if (tuning_model=='RT'){
    optimal_parameters <- RT_parameterTuning(across_path_df)
  } else if (tuning_model=='RF'){
    optimal_parameters <- RF_parameterTuning(across_path_df)
  } else if (tuning_model=='XGB') {
    optimal_parameters <- XGB_parameterTuning(across_path_df)
  }
  return(optimal_parameters)
}


#======== Section 2: functions for running the model and obtaining fitted values: OLS, RF, RT and XGB ===

OLS_fittedValue <- function(across_path_df){
  #fits an OLS regression on the dataframe and returns the fitted values
  
  across_path_reg <- lm(y~.,data=across_path_df)
  fitted_values_ols <- across_path_reg$fitted.values
  
  return(fitted_values_ols)
}

xgBoosting_fittedValue <- function(across_path_df,opt_hyperparm){
  #Trains a XG boosting model and returns the fitted values from it
  
  #Define the explanatory and dependent variables
  df_y <- across_path_df$y
  df_x <- across_path_df %>% select(-y)
  
  #Assign the optimal hyperparameters
  optimal_nrounds <- opt_hyperparm[1]
  optimal_eta <- opt_hyperparm[2]
  optimal_max_depth <- opt_hyperparm[3]
  optimal_gamma <- opt_hyperparm[4]
  optimal_colsample_bytree <- opt_hyperparm[5]
  optimal_min_child_weight <- opt_hyperparm[6]
  optimal_subsample <- opt_hyperparm[7]
  optimal_stopRound <- opt_hyperparm[8]
  
  #Run the training for the XG boosting model
  xgb.fit <- xgboost(
    data = as.matrix(df_x),
    label = as.matrix(df_y),
    eta = optimal_eta,
    nrounds= optimal_nrounds,
    max.depth = optimal_max_depth,
    gamma = optimal_gamma,
    min_child_weight = optimal_min_child_weight,
    colsample_bytree = optimal_colsample_bytree,
    subsample = optimal_subsample,
    early_stopping_rounds = optimal_stopRound,
    objective = "reg:linear",  # for regression type
    verbose = 0)
  
  #Get the fitted values from the training xgb model
  xgb_fittedValue <- predict(xgb.fit,as.matrix(df_x))
  return(xgb_fittedValue)
}

Regression_tree_fittedValue <- function(across_path_df,opt_hyperparm){
  #Trains a regression tree model and returns the fitted values from it
  
  #Assign the optimal hyperparameters
  optimal_cp <- opt_hyperparm[1]
  optimal_minsplit <- opt_hyperparm[2]
  optimal_minbucket <- opt_hyperparm[3]
  
  #train a regression tree model and specify its hyperparameters
  dec_tree <- rpart(y~., data = across_path_df,method='anova',control = rpart.control(cp=optimal_cp,minsplit=optimal_minsplit,minbucket=optimal_minbucket))
  #obtain its fitted values
  fitted_values_rt <- predict(dec_tree,across_path_df,type='vector')
  
  return(fitted_values_rt)
}

Rf_fittedValue <- function(across_path_df,opt_hyperparm){
  #Trains a Random forest model and returns the fitted values from it
  
  #Assign the optimal hyperparameters
  optimal_numbertrees <- opt_hyperparm[1]
  optimal_mtry <- opt_hyperparm[2]
  optimal_nodesize <- opt_hyperparm[3]
  optimal_maxnodes <- opt_hyperparm[4]
  
  #train the random forest model and set the hyperparameters
  rf <- randomForest(y~., data = across_path_df, ntree=optimal_numbertrees,mtry = optimal_mtry,nodesize = optimal_nodesize,max_nodes = optimal_maxnodes)
  
  #obtain the fitted values from it
  return(as.vector(rf$predicted))
}

model_fittedValues <- function(model_type,across_path_df,opt_hyperparm=NULL){
  #Returns the fitted values from the specified model, optionally including the model's hyperparameters
  #The model to be chosen from are 'OLS', 'RF', 'RT' or 'XGB'
  
  if (model_type=='OLS'){
    fitted_values <- OLS_fittedValue(across_path_df)
  } else if (model_type=='RF'){
    fitted_values <- Rf_fittedValue(across_path_df,opt_hyperparm)
  } else if (model_type=='RT'){
    fitted_values <- Regression_tree_fittedValue(across_path_df,opt_hyperparm)
  } else if (model_type=='XGB'){
    fitted_values <- xgBoosting_fittedValue(across_path_df,opt_hyperparm)
  }
  return(fitted_values)
}

get_optimal_hyperparameters <- function(model_type){
  #Returns the optimal hyperparameters for the specified model in vector form
  #Note that you can input your optimal hyperparameters in here
  
  if (model_type=='RT'){
    optimal_cp <- 0.001
    optimal_minsplit <- 31
    optimal_minbucket <- 25
    return(c(optimal_cp,optimal_minsplit,optimal_minbucket))
  } else if (model_type=='RF'){
    optimal_numbertrees <- 75
    optimal_mtry <- 4
    optimal_nodesize <- 9
    optimal_maxnodes <- 19
    return(c(optimal_numbertrees,optimal_mtry,optimal_nodesize,optimal_maxnodes))
  } else if (model_type=='XGB'){
    optimal_nrounds <- 764
    optimal_eta <- 0.2951486
    optimal_max_depth <- 7
    optimal_gamma <- 0.9576501
    optimal_colsample_bytree <- 0.7518910
    optimal_min_child_weight <- 4
    optimal_subsample <- 0.7407549
    optimal_stopRound <- 8
    return(c(optimal_nrounds,optimal_eta,optimal_max_depth,
             optimal_gamma,optimal_colsample_bytree,
             optimal_min_child_weight,optimal_subsample,optimal_stopRound))
  }
}

get_Model_parameters <- function(model_type){
  #Returns the model's hyperparameters (only for ML models) and its type
  #Required to know which hyperparameters can be rounded or not
  #Returns them in a list format, with two elements: 'params': containing the hyperparameter names and 'param_type': hyperparameter's type
  
  if (model_type=='RT'){
    return(list(params = c('cp','minsplit','minbucket'), 
                param_type = c('float','int','int')))
  } else if (model_type=='RF'){
    return(list(params = c('NumberOfTrees','mtry','nodesize','maxnodes'), 
                param_type = c('int','int','int','int')))
  } else if (model_type=='XGB'){
    return(list(params = c('nrounds','eta','max_depth','gamma','colsample_bytree','min_child_weight','subsample','stoppingRounds'), 
                param_type = c('int','float','int','float','float','int','float','int')))
  } 
}

#======== Section 3: Additional functions =============================

vector_utility_calculation <- function(allocations,return_list,gamma,
                                       period,retirement_age,period_list){
  #calculates the utility for all scenarios, for a power utility function.
  #INPUT:
  #allocations : a vector of allocations, with columns noted as Equities, short_bonds and long_bonds
  #Normalised_wealth : The simulated normalised wealth over all periods and scenarios
  #Income: the simulated income over all scenarios and periods
  #premium_fraction: The fraction of income that is invested
  #return_list is the list containing all the current-period returns of all variables, for all scenarios, in matrix column form
  #gamma is the relative risk aversion factor
  #period is the current period
  #retirement age is the last period
  #period_list is a list containing for every period, the optimal portfolio allocations and its corresponding utility over all scenarios
  
  rownames(allocations) <- NULL
  
  #if we are in the first period, then we dont require to collect utilities from other periods
  wealth <- matrix(0,nrow=nrow(return_list[[1]]),ncol=1)
  if (period==(retirement_age)){
    #calculate the power utility given the allocation, over all scenarios
    col_num <- 1
    for (var in names(return_list)){
      wealth <- wealth + allocations[1,col_num]*return_list[[var]][,period]
      col_num <- col_num + 1
    }
    gamma_adjusted_utility <- U(wealth,gamma)
  } else {
    col_num <- 1
    for (var in names(return_list)){
      wealth <- wealth + allocations[1,col_num]*return_list[[var]][,period]
      col_num <- col_num + 1
    }
    #Use power utility, except when gamma is 1, then refer to log utility
    if (gamma==1){
      wealth_gammaPower <- log(wealth)
    } else {
      wealth_gammaPower <- wealth^(1-gamma)
    }
    
    #Collects the accumulated wealth over all future periods, for each scenario
    accumulated_wealth <- wealth_gammaPower
    for (future_period in (period+1):(retirement_age)){
      future_period_chr <- paste('age_',as.character(future_period),sep='')
      accumulated_wealth <- accumulated_wealth * as.vector(period_list[[future_period]][,'utility'])
    }
    #Use power utility, except when gamma is 1, then refer to log utility
    if (gamma==1){
      gamma_adjusted_utility <- accumulated_wealth
    } else {
      gamma_adjusted_utility <- (1/(1-gamma))*accumulated_wealth
    }
  }
  #Returns a vector of utility, with size of the number of scenarios
  return(gamma_adjusted_utility)
}

vector_myopic_utility_calculation <- function(allocations,return_list,gamma){
  #calculates the utility for a whole vector for the myopic or Buy&Hold portfolio choice, for a power utility function.
  #INPUT:
  #allocations is a vector, with columns noted as Equities, short_bonds and long_bonds
  #equity_return, shortBond_return and longBond_return are the current-period returns, for all scenarios, in matrix column form
  #gamma is the relative risk aversion factor
  rownames(allocations) <- NULL
  
  matrix(0,nrow=nrow(return_list[[1]]),ncol=1)
  col_num <- 1
  for (var in names(return_list)){
    wealth <- wealth + allocations[1,col_num]*return_list[[var]][,period]
    col_num <- col_num + 1
  }
  power_utility <- U(wealth,gamma)
  
  return(power_utility)
}

cumulative_returns <- function(return_list,start_period,end_period){
  #Returns the cumulative returns between two periods, over all scenarios
  
  cumulative_returns_list <- list()
  
  for (var in names(return_list)){
    cum_return_current_var <- t(apply(return_list[[var]][,((start_period):(end_period))],1,cumprod))
    cumulative_returns_list[[var]] <- matrix(cum_return_current_var[,ncol(cum_return_current_var)],nrow=nrow(cum_return_current_var),ncol=1)
  }
  
  return(cumulative_returns_list)
}

limit_allocations <- function(all_allocations,current_allocation){
  #Returns the remaining possible allocations, by considering that stock allocation cannot change more than 8%
  
  for (i in 1:ncol(all_allocations)){
    #print(current_allocation[,1])
    all_allocations[,paste(assets[i],"_dif",sep='')] = round(all_allocations[,assets[i]] - current_allocation[,i],2)
    #return(all_allocations)
    all_allocations <- all_allocations[(all_allocations[,paste(assets[i],"_dif",sep='')] >= -0.1) & (all_allocations[,paste(assets[i],"_dif",sep='')] <= 0.1),]
    rownames(all_allocations) <- NULL
  }
  #all_allocations <- all_allocations[,1:11]
  
  return(all_allocations)
}

generate_next_allocation_grid_v2 <- function(current_allocation,increment_value,width_length){
  #This function generated a new allocation grid
  #Inputs:
  #current_allocation: is the current asset allocation around which you want to build the portfolio grid from
  #increment_value: by what increment the portfolio weights can increase or decrease
  #width_length: the maximum increase or decrease of a particular portfolio weights between two neighboring periods
  
  #ADD A TRYCATCH FUNCTION HERE
  
  #Generate here the possible portfolio weights per asset, given its current portfolio weight in current_allocation
  allocations_assets <- list()
  for (asset in 1:length(current_allocation)){
    if (round(current_allocation[,asset],2)<=width_length) {
      allocations_assets[[asset]] <- seq(0,(2*width_length),by=increment_value)
    } else if (round(current_allocation[,asset],2)<=(1-width_length)){
      allocations_assets[[asset]] <- seq(current_allocation[,asset]-width_length,current_allocation[,asset]+width_length,by=increment_value)
    } else {
      allocations_assets[[asset]] <- seq((1-(2*width_length)),1,by=increment_value)
    }
  }
  
  all_allocations <- matrix(NA,nrow=20000000,ncol=num_assets)
  next_row <- 1
  
  #Loop over all the possible portfolio weights
  #and only store the combined portfolio weights which add to 1, the portfolio restriction
  for (asset_1 in allocations_assets[[1]]){
    for (asset_2 in allocations_assets[[2]]){
      for (asset_3 in allocations_assets[[3]]){
        for (asset_4 in allocations_assets[[4]]){
          for (asset_5 in allocations_assets[[5]]){
            for (asset_6 in allocations_assets[[6]]){
              for (asset_7 in allocations_assets[[7]]){
                for (asset_8 in allocations_assets[[8]]){
                  for (asset_9 in allocations_assets[[9]]){
                    for (asset_10 in allocations_assets[[10]]){
                      for (asset_11 in allocations_assets[[11]]){
                        if (round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10+asset_11,2)==1){
                          allocation_vector <- c(asset_1,asset_2,asset_3,
                                                 asset_4,asset_5,asset_6,
                                                 asset_7,asset_8,asset_9,
                                                 asset_10,asset_11)
                          all_allocations[next_row,] <- allocation_vector
                          next_row <- next_row + 1
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #reset the names of the rows of the matrix, for convenience
  rownames(all_allocations) <- NULL
  #convert to dataframe, for calculation convenience later on
  all_allocations <- as.data.frame(all_allocations)
  
  #filter out the rows with NAs
  all_allocations <- all_allocations %>% filter(!is.na(all_allocations[,1]))
  
  names(all_allocations) <- assets
  
  return(all_allocations)
}

# round(sum(c(0.46,0.0,0.02,0.12,0.00,0.4,0.0,0.0,0.0,0.0,0.0)),2)
# generate_next_allocation_grid(c(0.46,0.0,0.06,0.16,0.00,0.4,0.0,0.0,0.0,0.0,0.0),0.04,0.04)

generate_next_allocation_grid <- function(current_allocation,increment_value,width_length){
  #This function generated a new allocation grid
  #Inputs:
  #current_allocation: is the current asset allocation around which you want to build the portfolio grid from
  #increment_value: by what increment the portfolio weights can increase or decrease
  #width_length: the maximum increase or decrease of a particular portfolio weights between two neighboring periods
  
  #Generate here the possible portfolio weights per asset, given its current portfolio weight in current_allocation
  allocations_assets <- list()
  for (asset in 1:length(current_allocation)){
    if (round(current_allocation[asset],2)<=width_length) {
      allocations_assets[[asset]] <- round(seq(current_allocation[asset],current_allocation[asset]+(2*width_length),by=increment_value),2)
      #allocations_assets[[asset]] <- round(seq(0,(2*width_length),by=increment_value),2)
    } else if (round(current_allocation[asset],2)<=(1-width_length)){
      allocations_assets[[asset]] <- round(seq(current_allocation[asset]-width_length,current_allocation[asset]+width_length,by=increment_value),2)
    } else {
      allocations_assets[[asset]] <- round(seq((1-(2*width_length)),1,by=increment_value),2)
    }
  }
  
  all_allocations <- matrix(NA,nrow=20000000,ncol=num_assets)
  next_row <- 1

  #Loop over all the possible portfolio weights
  #and only store the combined portfolio weights which add to 1, the portfolio restriction
  for (asset_1 in allocations_assets[[1]]){
    for (asset_2 in allocations_assets[[2]]){
      for (asset_3 in allocations_assets[[3]]){
        for (asset_4 in allocations_assets[[4]]){
          for (asset_5 in allocations_assets[[5]]){
            for (asset_6 in allocations_assets[[6]]){
              for (asset_7 in allocations_assets[[7]]){
                for (asset_8 in allocations_assets[[8]]){
                  for (asset_9 in allocations_assets[[9]]){
                    for (asset_10 in allocations_assets[[10]]){
                      for (asset_11 in allocations_assets[[11]]){
                        #print(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10+asset_11,2))
                        if (round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10+asset_11,2)==1.00){
                          allocation_vector <- c(asset_1,asset_2,asset_3,
                                                 asset_4,asset_5,asset_6,
                                                 asset_7,asset_8,asset_9,
                                                 asset_10,asset_11)
                          all_allocations[next_row,] <- allocation_vector
                          next_row <- next_row + 1
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  #reset the names of the rows of the matrix, for convenience
  rownames(all_allocations) <- NULL
  #convert to dataframe, for calculation convenience later on
  all_allocations <- as.data.frame(all_allocations)
  
  #filter out the rows with NAs
  all_allocations <- all_allocations %>% filter(!is.na(all_allocations[,1]))
  
  names(all_allocations) <- assets
  
  return(all_allocations)
}
#allocation_test <- generate_next_allocation_grid(matrix(c(0.1,0.2,0.3,0,0,0,0,0.1,0,0.1,0.2),nrow=1,ncol=11),0.04,0.04)
ESG_restrict_allocations <- function(all_allocations,ESG_scores,ESG_threshold,
                                     env_weight,soc_weight,gov_weight){
  #Returns the remaining possible allocations, by considering that the equity portfolio should adhere to the ESG threshold score
  
  ESG_score_current <- ESG_scores
  #calculates the total ESG score, given the env, soc and gov weights and the final ESG-component scores for each equity ESG portfolio
  ESG_score_current[,'ESG total'] <- env_weight*ESG_score_current[,'env_2020'] + soc_weight*ESG_score_current[,'soc_2020'] + gov_weight*ESG_score_current[,'gov_2020']
  
  #Obtain the total summed weight of all the equity ESG portfolios
  all_allocations[,'allocation equity'] <- apply(all_allocations[,4:ncol(all_allocations)],1,sum)
  #Obtain the ESG score for each asset allocation.
  #this is calculated by average-weighted weight per portfolio, multiplied by the total ESG score per equity ESG portfolio
  all_allocations[,'ESG score'] <- apply(all_allocations[,4:(ncol(all_allocations)-1)],1,function(x) sum((x/sum(x))*ESG_score_current[,'ESG total']))
  
  # Keep only the asset allocations which either have an ESG score that exceeds the ESG threshold or an asset allocation
  #which does not have anything invested into any of the equity ESG portfolios
  all_allocations <- all_allocations[((all_allocations[,'ESG score'] >= ESG_threshold)|(all_allocations[,'allocation equity']==0)),]
  all_allocations <- all_allocations[,1:(ncol(all_allocations)-2)]
  rownames(all_allocations) <- NULL
  
  return(all_allocations)
}

find_max <- function(num_col,df) {
  #This function extracts the maximum 
  return(max(df[,num_col]))
}

find_max_index <- function(num_col,df) {
  return(which(df[,num_col]==max(df[,num_col]))[1])
}


#======== Section 4: Function to obtain optimal allocations, depending on model type and return scenarios =================

get_optimal_allocation <- function(return_var_list,state_var_list,all_allocations,ESG_constraint,ESG_scores,
                                   ESG_threshold,env_weight,soc_weight,gov_weight,subset_size){
  #Returns the optimal allocation for a dynamic, myopic and Buy&Hold portfolio
  #But if HyperParm_tuning is True, then it returns the optimal hyperparameters for the ML model, per period
  
  #Define lists to store the optimal allocations, for every investment strategy, for all gammas
  allocations_dynamic_horizons <- list()
  allocations_myopic_horizons <- list()
  allocations_BuyHold_horizons <- list()
  
  #loop over all horizons, to find the optimal asset allocation per forecasting horizon
  for (horizon in 2:max_horizon){
    print(paste('horizon_',horizon,sep=''))
    
    #in case the ESG constraint is active, get rid of the asset allocations from the grid which do not exceed the ESG threshold score
    if (ESG_constraint){
      all_allocations <- ESG_restrict_allocations(all_allocations,ESG_scores,ESG_threshold,
                                                  env_weight,soc_weight,gov_weight)
    }
    
    #Reset the possible allocation for both dynamic and myopic strategy
    all_allocations_dynamic <- all_allocations
    
    #list that will store for every period, the optimal portfolio allocations and its corresponding utility, over all scenarios
    #Only relevant for the Dynamic asset allocation
    period_list <- list()
    
    #loop over all the periods backwards, since it is a backward recursion algorithm
    if (horizon==1){
      current_max_horizon <- 2
    }else{
      current_max_horizon <- horizon
    }
    
    #these matrices will store the optimal asset allocations per period, per investment strategy
    allocations_dynamic <- matrix(0,nrow=current_max_horizon,ncol=length(assets))
    colnames(allocations_dynamic) <- assets
    allocations_buyHold <- matrix(0,nrow=current_max_horizon,ncol=length(assets))
    colnames(allocations_buyHold) <- assets
    
    for (period in (current_max_horizon):1){
      
      iteration <- 1
      print(paste('Period:',as.character(period),sep=' '))
      
      #These matrices collect the expected utilities in the final periods, to calculate the across-path mean
      final_expected_utility_dynamic <- matrix(0,nrow=nrow(all_allocations_dynamic),ncol=1)
      if (period>1){
        
        # Given that the number of allocations are enormous, we have to work with subsets of the full allocation grid
        # This finds how many subsets with (subset_size) allocations are to be found in the grid, while also finding the number
        # of allocations in the last subset
        print(paste('Grid size is ',nrow(all_allocations_dynamic),sep=''))
        full_subsets <- floor(nrow(all_allocations_dynamic)/subset_size)
        last_subset_size <- nrow(all_allocations_dynamic)%%subset_size
        
        for (subset in 1:(full_subsets+1)){
          print(paste('Entered subset number ',as.character(subset),sep=''))
          print(Sys.time())
          if (subset<(full_subsets+1)){
            current_allocation_subset <- all_allocations_dynamic[(1+subset_size*(subset-1)):(subset_size*(subset)),]
          } else {
            current_allocation_subset <- all_allocations_dynamic[(1+subset_size*(subset-1)):(nrow(all_allocations_dynamic)),]
          }
          print(paste('current_allocation_subset is ',nrow(current_allocation_subset),sep=''))
          rownames(current_allocation_subset) <- NULL
          #return(current_allocation_subset)
          
          cl <- parallel::makeCluster(detectCores())
          doParallel::registerDoParallel(cl)
          utility_over_allocations_dynamic_list <- foreach(iteration=1:nrow(current_allocation_subset),.packages='testPack2') %dopar% {
            
            # if((horizon==3)&(period==2)){
            # 
            # 
            #   #if we are in the first period, then we dont require to collect utilities from other periods
            #   wealth <- matrix(0,nrow=nrow(return_var_list[[1]]),ncol=1)
            #   if (period==(current_max_horizon)){
            #     #calculate the power utility given the allocation, over all scenarios
            #     col_num <- 1
            #     for (var in names(return_var_list)){
            #       wealth <- wealth + current_allocation_subset[iteration,col_num]*return_var_list[[var]][,period]
            #       col_num <- col_num + 1
            #     }
            #     gamma_adjusted_utility <- apply(wealth,1,U)
            #   } else {
            #     col_num <- 1
            #     for (var in names(return_var_list)){
            #       wealth <- wealth + current_allocation_subset[iteration,col_num]*return_var_list[[var]][,period]
            #       col_num <- col_num + 1
            #     }
            #     #Use power utility, except when gamma is 1, then refer to log utility
            #     wealth_gammaPower <- apply(wealth,1,function(x) x^(1-5))
            #     #wealth_gammaPower <- wealth^(1-gamma)
            # 
            #     #Collects the accumulated wealth over all future periods, for each scenario
            #     accumulated_wealth <- wealth_gammaPower
            #     for (future_period in (period+1):(current_max_horizon)){
            #       future_period_chr <- paste('age_',as.character(future_period),sep='')
            #       accumulated_wealth <- accumulated_wealth * as.vector(period_list[[as.character(future_period)]][,'utility'])
            #     }
            #     #accumulated_wealth
            #     #Use power utility, except when gamma is 1, then refer to log utility
            #     gamma_adjusted_utility <- apply(matrix(accumulated_wealth,nrow=1,ncol=nrow(return_var_list[[1]])),2,function(x) (x/(1-5)))
            #     #gamma_adjusted_utility <- (1/(1-gamma))*accumulated_wealth
            #   }
            #   
            #   # utility_all_scenarios_dynamic <- vector_utility_calculation(current_allocation_subset[iteration,],
            #   #                                                             return_var_list,
            #   #                                                             gamma,
            #   #                                                             period,(current_max_horizon),
            #   #                                                             period_list)
            # } else {
            #   utility_all_scenarios_dynamic <- vector_utility_calculation(current_allocation_subset[iteration,],
            #                                                               return_var_list,
            #                                                               gamma,
            #                                                               period,(current_max_horizon),
            #                                                               period_list)
            #   utility_all_scenarios_dynamic
            #   across_path_df <- list()
            #   across_path_df[['y']] <- utility_all_scenarios_dynamic
            # 
            #   for (state_var in names(state_var_list)){
            #     #Create state variables
            #     current_state_var <- state_var_list[[state_var]][,(period-1)]
            #     across_path_df[[state_var]] <- current_state_var
            #   }
            #   across_path_df <- as.data.frame(do.call(cbind,across_path_df))
            #   colnames(across_path_df)[1] <- 'y'
            #   fitted_utility <- OLS_fittedValue(across_path_df)
            # 
            #   append(fitted_utility,mean(across_path_df$y))
            # }
            utility_all_scenarios_dynamic <- vector_utility_calculation(current_allocation_subset[iteration,],
                                                                        return_var_list,
                                                                        gamma,
                                                                        period,(current_max_horizon),
                                                                        period_list)
            across_path_df <- list()
            across_path_df[['y']] <- utility_all_scenarios_dynamic

            for (state_var in names(state_var_list)){
              #Create state variables
              current_state_var <- state_var_list[[state_var]][,(period-1)]
              across_path_df[[state_var]] <- current_state_var
            }
            across_path_df <- as.data.frame(do.call(cbind,across_path_df))
            colnames(across_path_df)[1] <- 'y'
            fitted_utility <- OLS_fittedValue(across_path_df)

            append(fitted_utility,mean(across_path_df$y))
          }
          parallel::stopCluster(cl)
          
          utility_over_allocations_dynamic_list <- do.call(rbind,utility_over_allocations_dynamic_list)
          
          if (subset<(full_subsets+1)){
            final_expected_utility_dynamic[((1+subset_size*(subset-1)):(subset_size*(subset))),1] <- utility_over_allocations_dynamic_list[,ncol(utility_over_allocations_dynamic_list)]
          } else {
            final_expected_utility_dynamic[((1+subset_size*(subset-1)):(nrow(all_allocations_dynamic))),1] <- utility_over_allocations_dynamic_list[,ncol(utility_over_allocations_dynamic_list)]
          }
          utility_over_allocations_dynamic <- utility_over_allocations_dynamic_list[,(1:(ncol(utility_over_allocations_dynamic_list)-1))]
          
          #Obtain the allocation which has the highest utility, for each scenario
          max_utility_rowIndex_perScenario_dynamic <- unlist(mclapply(1:ncol(utility_over_allocations_dynamic),find_max_index,utility_over_allocations_dynamic))
          #Extract these allocations from the matrix with all possible allocations for every scenario
          max_utility_perScenario_dynamic <- unlist(mclapply(1:ncol(utility_over_allocations_dynamic),find_max,utility_over_allocations_dynamic))
          #NEED TO COME UP WITH A COMPARISON TOOL BETWEEN SUBSETS AND THE MAXIMUM UTILITY
          optimal_allocations_perScenario_dynamic <- current_allocation_subset[max_utility_rowIndex_perScenario_dynamic,]
          #Reset the index of the matrix
          row.names(optimal_allocations_perScenario_dynamic) <- NULL
          
          if (subset==1){
            #Within the first subset, just store the maximum utility and the corresponding optimal asset allocation
            current_total_max_utility_perScenario_dynamic <- max_utility_perScenario_dynamic
            current_total_opt_allocation_perScenario_dynamic <- optimal_allocations_perScenario_dynamic
          } else {
            #from the second subset onwards, we need to find the max utility and thus the max optimal allocation across the subsets
            
            #combine the maximum utility per scenario of the current subset with the best utility per scenario across the previous subsets
            #this is a 2x#scenarios matrix
            current_total_max_utility_perScenario_dynamic <- rbind(current_total_max_utility_perScenario_dynamic,max_utility_perScenario_dynamic)
            rownames(current_total_max_utility_perScenario_dynamic) <- NULL
            
            #find the index (so either row index 1 or 2) per scenario which has the highest utility
            total_max_utility_rowIndex_perScenario_dynamic <- apply(current_total_max_utility_perScenario_dynamic,2,function(x) which(x==max(x))[1])
            
            #combine the optimal allocation per scenario of the current subset with the optimal allocation per scenarios across the previous subsets
            #With this combined, this becomes a #scenariosx(2*#assets) matrix
            current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,optimal_allocations_perScenario_dynamic)
            rownames(current_total_opt_allocation_perScenario_dynamic) <- NULL
            #create a new column in this matrix which shows which index gives the maximum utility per scenario, from current_total_max_utility_perScenario_dynamic
            current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,total_max_utility_rowIndex_perScenario_dynamic)
            
            #the idea is that if for a particular scenario the utility is maximal for the first row index, then choose the first (#assets) columns as the optimal asset allocation
            #if the utility is maximal at the second row index, then choose the second (#assets) columns as the optimal asset allocation
            #Given my non-expertise of R, I do this using an ifelse statement and creating additional (#assets) columns at the end of the matrix
            current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,matrix(0,nrow=scenarios_train,ncol=length(assets)))
            
            #loop over all the assets and implement the idea of above, using an if else statement, dependent on the max rowIndex column in the matrix
            for (asset in 1:length(assets)){
              current_total_opt_allocation_perScenario_dynamic[,(length(assets)*2)+1+asset] <- ifelse(current_total_opt_allocation_perScenario_dynamic[,(length(assets)*2)+1]==1,
                                                                                                  current_total_opt_allocation_perScenario_dynamic[,asset],
                                                                                                  current_total_opt_allocation_perScenario_dynamic[,length(assets)+asset])
            }
            
            col_number <- ncol(current_total_opt_allocation_perScenario_dynamic)
            #Extract the last (#assets) columns from current_total_opt_allocation_perScenario_dynamic, because that would be the current optimal asset allocations per scenario
            current_total_opt_allocation_perScenario_dynamic <- current_total_opt_allocation_perScenario_dynamic[,(col_number-length(assets)+1):col_number]
            #rename the columns again to the original asset names
            colnames(current_total_opt_allocation_perScenario_dynamic) <- assets
          }
        }
        
        #Obtains the row index (thus the optimal portfolio allocation) in each scenario, where the maximum fitted utility is found, for each scenario
        max_finalUtility_rowIndex_dynamic <- which(final_expected_utility_dynamic==max(final_expected_utility_dynamic))
        #using the above row indexes, collect the optimal portfolio allocations for each scenario
        #essentially a dataframe/matrix (# of scenarios)x(# of assets) matrix
        final_optimal_allocation_dynamic <- all_allocations_dynamic[max_finalUtility_rowIndex_dynamic,]
        #Reset the index of the matrix
        row.names(final_optimal_allocation_dynamic) <- NULL
        
        
        
        # for (subset in 1:(full_subsets+1)){
        #   # #create a loop over all subsets of the allocation grid and store them separately, as seen below
        #   # if ((1+25000*(subset-1))%%25000==0){
        #   #   print(paste('subset is at iteration ',as.character((1+25000*(subset-1))),', from the ',as.character(nrow(all_allocations_dynamic)),sep=''))
        #   # }
        #   
        #   if (subset<(full_subsets)){
        #     current_allocation_subset <- all_allocations_dynamic[(1+25000*(subset-1)):(25000*(subset)),]
        #   } else {
        #     current_allocation_subset <- all_allocations_dynamic[(1+25000*(subset-1)):(nrow(all_allocations_dynamic)),]
        #   }
        #   rownames(current_allocation_subset) <- NULL
        #   
        #   #this matrix will store all the utilities, per scenario per asset allocation
        #   utility_over_allocations_dynamic <- matrix(0,nrow=nrow(current_allocation_subset),ncol=scenarios_train)
        #   
        #   for (row in 1:nrow(current_allocation_subset)){
        #     #calculate the utility for every scenario, given a specific allocation
        #     
        #     if (iteration%%2500==0){
        #       print(Sys.time())
        #       print(paste('iteration is at ',as.character(iteration),sep=''))
        #     }
        #     iteration <- iteration + 1
        #     utility_all_scenarios_dynamic <- vector_utility_calculation(current_allocation_subset[row,],
        #                                                                 return_var_list,
        #                                                                 gamma,
        #                                                                 period,(current_max_horizon),
        #                                                                 period_list)
        #     
        #     across_path_df <- data.frame(y= utility_all_scenarios_dynamic)
        #     
        #     for (state_var in names(state_var_list)){
        #       #Create state variables
        #       current_state_var <- state_var_list[[state_var]][,(period-1)]
        #       across_path_df <- cbind(across_path_df,new_col = current_state_var)
        #       names(across_path_df)[names(across_path_df)=='new_col'] <- state_var
        #     }
        #     
        #     if (model_type=='OLS'){
        #       fitted_utility <- model_fittedValues(model_type,across_path_df)
        #     } else {
        #       #If using ML models, also specify the optimal hyperparameters to be used for the optimisation
        #       fitted_utility <- model_fittedValues(model_type,across_path_df,optimal_hypPar)
        #     }
        #     
        #     #collect the fitted values of the utility
        #     utility_over_allocations_dynamic[row,] <- fitted_utility
        #     
        #     #get across-path mean
        #     across_path_mean <- mean(across_path_df$y)
        #     final_expected_utility_dynamic[(25000*(subset-1))+row,1] <- across_path_mean
        #   }
        #   
        #   #store the current highest portfolio allocation per scenario, within the subset
        #   #then check with the next subset whether the highest utility changes and thus new optimal allocation is to be found
        #   
        #   #Obtain the allocation which has the highest utility, for each scenario
        #   max_utility_rowIndex_perScenario_dynamic <- apply(utility_over_allocations_dynamic,2,function(x) which(x==max(x))[1])
        #   #Extract these allocations from the matrix with all possible allocations for every scenario
        #   max_utility_perScenario_dynamic <- matrix(apply(utility_over_allocations_dynamic,2,max),nrow=1,ncol=scenarios_train)
        #   #NEED TO COME UP WITH A COMPARISON TOOL BETWEEN SUBSETS AND THE MAXIMUM UTILITY
        #   optimal_allocations_perScenario_dynamic <- current_allocation_subset[max_utility_rowIndex_perScenario_dynamic,]
        #   #Reset the index of the matrix
        #   row.names(optimal_allocations_perScenario_dynamic) <- NULL
        #   if (subset==1){
        #     #Within the first subset, just store the maximum utility and the corresponding optimal asset allocation
        #     current_total_max_utility_perScenario_dynamic <- max_utility_perScenario_dynamic
        #     current_total_opt_allocation_perScenario_dynamic <- optimal_allocations_perScenario_dynamic
        #   } else {
        #     #from the second subset onwards, we need to find the max utility and thus the max optimal allocation across the subsets
        #     
        #     #combine the maximum utility per scenario of the current subset with the best utility per scenario across the previous subsets
        #     #this is a 2x#scenarios matrix
        #     current_total_max_utility_perScenario_dynamic <- rbind(current_total_max_utility_perScenario_dynamic,max_utility_perScenario_dynamic)
        #     rownames(current_total_max_utility_perScenario_dynamic) <- NULL
        #     #find the index (so either row index 1 or 2) per scenario which has the highest utility
        #     total_max_utility_rowIndex_perScenario_dynamic <- apply(current_total_max_utility_perScenario_dynamic,2,function(x) which(x==max(x))[1])
        #     
        #     #combine the optimal allocation per scenario of the current subset with the optimal allocation per scenarios across the previous subsets
        #     #With this combined, this becomes a #scenariosx(2*#assets) matrix
        #     current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,optimal_allocations_perScenario_dynamic)
        #     rownames(current_total_opt_allocation_perScenario_dynamic) <- NULL
        #     #create a new column in this matrix which shows which index gives the maximum utility per scenario, from current_total_max_utility_perScenario_dynamic
        #     current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,total_max_utility_rowIndex_perScenario_dynamic)
        #     
        #     #the idea is that if for a particular scenario the utility is maximal for the first row index, then choose the first (#assets) columns as the optimal asset allocation
        #     #if the utility is maximal at the second row index, then choose the second (#assets) columns as the optimal asset allocation
        #     #Given my non-expertise of R, I do this using an ifelse statement and creating additional (#assets) columns at the end of the matrix
        #     current_total_opt_allocation_perScenario_dynamic <- cbind(current_total_opt_allocation_perScenario_dynamic,matrix(0,nrow=scenarios_train,ncol=length(assets)))
        #     
        #     #loop over all the assets and implement the idea of above, using an if else statement, dependent on the max rowIndex column in the matrix
        #     for (asset in 1:length(assets)){
        #       current_total_opt_allocation_perScenario_dynamic[,length(assets)+1+asset] <- ifelse(current_total_opt_allocation_perScenario_dynamic[,length(assets)+1]==1,
        #                                                                                           current_total_opt_allocation_perScenario_dynamic[length(assets)+1+asset],
        #                                                                                           current_total_opt_allocation_perScenario_dynamic[length(assets)+1+asset+length(asset)])
        #     }
        #     
        #     
        #     col_number <- ncol(current_total_opt_allocation_perScenario_dynamic)
        #     #Extract the last (#assets) columns from current_total_opt_allocation_perScenario_dynamic, because that would be the current optimal asset allocations per scenario
        #     current_total_opt_allocation_perScenario_dynamic <- current_total_opt_allocation_perScenario_dynamic[,(col_number-length(assets)+1):col_number]
        #     #rename the columns again to the original asset names
        #     colnames(current_total_opt_allocation_perScenario_dynamic) <- assets
        #   }
        # }
        # 
        # #Obtains the row index (thus the optimal portfolio allocation) in each scenario, where the maximum fitted utility is found, for each scenario
        # max_finalUtility_rowIndex_dynamic <- apply(final_expected_utility_dynamic,2,function(x) which(x==max(x))[1])
        # #using the above row indexes, collect the optimal portfolio allocations for each scenario
        # #essentially a dataframe/matrix (# of scenarios)x(# of assets) matrix
        # final_optimal_allocation_dynamic <- all_allocations_dynamic[max_finalUtility_rowIndex_dynamic,]
        # #Reset the index of the matrix
        # row.names(final_optimal_allocation_dynamic) <- NULL
        
      }
      #if you are optimizing in the period for which you want to find the optimal allocation, enter this if statement
      #if it is the final period of the for loop, use the asset allocation of the period before, because
      #state variables do not exist in period t-1
      for (asset in assets){
        if (period==1){
          allocations_dynamic[1,asset] <- allocations_dynamic[2,asset]
        } else {
          #Store the optimal allocations for that period
          allocations_dynamic[period,asset] <- final_optimal_allocation_dynamic[1,asset]
        }
      }
      if (period==1){
        #need to create here for Buy&Hold investor
        
        #by first obtaining cumulative returns
        list_cumulativeReturns <- cumulative_returns(return_var_list,period,(current_max_horizon))
        
        #As the Buy&Hold strategy is only determined at starting_age
        cl <- parallel::makeCluster(detectCores())
        doParallel::registerDoParallel(cl)
        final_expected_utility_buyHold_list <- foreach(iteration=1:nrow(all_allocations),.packages='testPack2') %dopar% {
          
          utility_all_scenarios_buyHold <- vector_myopic_utility_calculation(all_allocations[iteration,],
                                                                             list_cumulativeReturns)
          
          mean(utility_all_scenarios_buyHold)
        }
        parallel::stopCluster(cl)
        final_expected_utility_buyHold <- unlist(final_expected_utility_buyHold_list)
        
        #repeat above steps, but now for the Buy&Hold portfolio allocation optimization
        max_finalUtility_rowIndex_buyHold <- which(final_expected_utility_buyHold==max(final_expected_utility_buyHold))
        #REPEAT HERE TO FIND OPTIMAL ALLOCATION AROUND THE OTHER OPTIMAL ALLOCATION
        all_allocations_buyHold <- generate_next_allocation_grid_v2(all_allocations[max_finalUtility_rowIndex_buyHold,],0.04,0.04)
        #in case the ESG constraint is active, get rid of the asset allocations from the grid which do not exceed the ESG threshold score
        if (ESG_constraint){
          all_allocations_buyHold <- ESG_restrict_allocations(all_allocations_buyHold,ESG_scores,ESG_threshold,
                                                              env_weight,soc_weight,gov_weight)
        }
        
        cl <- parallel::makeCluster(detectCores())
        doParallel::registerDoParallel(cl)
        final_expected_utility_buyHold_list <- foreach(iteration=1:nrow(all_allocations_buyHold),.packages='testPack2') %dopar% {
          
          utility_all_scenarios_buyHold <- vector_myopic_utility_calculation(all_allocations_buyHold[iteration,],
                                                                             list_cumulativeReturns)
          #Obtain the across-path mean of the Buy&Hold utilities
          mean(utility_all_scenarios_buyHold)
        }
        parallel::stopCluster(cl)
        final_expected_utility_buyHold <- unlist(final_expected_utility_buyHold_list)
        
        #repeat above steps, but now for the Buy&Hold portfolio allocation optimization
        max_finalUtility_rowIndex_buyHold <- which(final_expected_utility_buyHold==max(final_expected_utility_buyHold))
        final_optimal_allocation_buyHold <- all_allocations_buyHold[max_finalUtility_rowIndex_buyHold,]
        row.names(final_optimal_allocation_buyHold) <- NULL
        
        for (asset in assets){
          allocations_buyHold[,asset] <- final_optimal_allocation_buyHold[1,asset]
        }
        #Break because we do not need the calculations below for the last iteration anyways
        break
      }
      #for the optimal portfolio allocations for each scenario, calculate its corresponding utility
      #should be a (#scenarios)x1 matrix
      vector_wealth <- matrix(0,nrow=nrow(return_var_list[[1]]),ncol=1)
      num_col <- 1
      for (var in names(return_var_list)){
        vector_wealth <- vector_wealth + current_total_opt_allocation_perScenario_dynamic[,num_col]*return_var_list[[var]][,period]
        num_col <- num_col + 1
      }
      
      #Use power utility, except when gamma is 1, then use log utility function
      if (gamma==1){
        optimal_allocations_perScenario_dynamic$utility <- log(vector_wealth)
      } else {
        optimal_allocations_perScenario_dynamic$utility <- (vector_wealth)^(1-gamma)
      }
      
      #store in period list
      period_list[['newPeriod']] <- optimal_allocations_perScenario_dynamic
      names(period_list)[names(period_list)=='newPeriod'] <- period
      
      #update the possible portfolio allocation grid, by adhering to
      #to the fact that stock allocation can decrease by at most 8% over time
      if ((horizon==11)&(period==8)){
        print(allocations_dynamic[period,])
      }
      if ((horizon==11)&(period==9)){
        print(allocations_dynamic[period,])
      }
      all_allocations_dynamic <- generate_next_allocation_grid(allocations_dynamic[period,],0.04,0.04)
      # if ((horizon==3)&(period==3)){
      #   return(all_allocations_dynamic)
      # }
      #in case the ESG constraint is active, get rid of the asset allocations from the grid which do not exceed the ESG threshold score
      if (ESG_constraint){
        all_allocations_dynamic <- ESG_restrict_allocations(all_allocations_dynamic,ESG_scores,ESG_threshold,
                                                            env_weight,soc_weight,gov_weight)
      }
    }
    
    #Store the optimal allocations over all periods for the specific gamma in the list
    if (horizon==2){
      allocations_dynamic_horizons[[1]] <- as.data.frame(allocations_dynamic)
      allocations_BuyHold_horizons[[1]] <- as.data.frame(allocations_buyHold)
      allocations_dynamic_horizons[[horizon]] <- as.data.frame(allocations_dynamic)
      allocations_BuyHold_horizons[[horizon]] <- as.data.frame(allocations_buyHold)
    } else {
      allocations_dynamic_horizons[[horizon]] <- as.data.frame(allocations_dynamic)
      allocations_BuyHold_horizons[[horizon]] <- as.data.frame(allocations_buyHold)
    }
  }
  if (hyperParm_tuning){
    return(optimal_hyperparameters)
  } else {
    if (investment_strategy=='Dynamic'){
      allocations_dynamic_buyhold <- list()
      allocations_dynamic_buyhold[['Dynamic']] <- allocations_dynamic_horizons
      allocations_dynamic_buyhold[['BuyHold']] <- allocations_BuyHold_horizons
      return(allocations_dynamic_buyhold)
    } else if (investment_strategy=='Myopic'){
      return(allocations_myopic_horizons)
    } else {
      return(allocations_BuyHold_horizons)
    }
  }
}

#======== Section 5:  functions for obtaining the certainty equivalent and the accummulated wealth per scenario ===============================================

get_wealth_perScenario <- function(opt_allocation,return_list){
  
  Nscenarios <- nrow(return_list[[1]])
  
  wealth <- matrix(0, Nscenarios, max_horizon+1)
  wealth[,1] <- 1
  for (t in 1:(max_horizon)){
    num_col <- 1
    return <- matrix(0,nrow=Nscenarios,ncol=1)
    for(var in names(return_list)){
      return <- return + return_list[[var]][,t]*opt_allocation[t,num_col]
      num_col <- num_col + 1
    }
    wealth[,t+1] <- wealth[,t]*return
  }
  return(wealth[,max_horizon+1])
}

get_CE <- function(opt_allocation,return_list){
  wealth_build_up <- get_wealth_perScenario(opt_allocation,return_list)
  
  CE_rational <- CE(wealth_build_up,gamma = 5)
  
  return(CE_rational)
}

#======== Section 6: Load RData file for specific cluster =================

source('Utility Functions.R')


#======== Section 7: Data cleaning and preparation =================

#create a matrix where all the portfolio choice allocations will be stored over the years

assets <- c( "Tbill_return","Tnote_return","corBond_return",
             "cluster_return_1","cluster_return_2","cluster_return_3",
             "cluster_return_4","cluster_return_5","cluster_return_6",
             "cluster_return_7","cluster_return_8")
scenarios_train <- nrow(return_var_train_list[[1]])
scenarios_test <- nrow(return_var_test_list[[1]])
num_assets <- length(assets)
allocations_dynamic_list <- list()
allocations_myopic_list <- list()
allocations_buyHold_list <- list()
for (horizon in c(1:max_horizon)){
  allocations_dynamic <- matrix(0,nrow=horizon, ncol=num_assets)
  colnames(allocations_dynamic) <- assets
  rownames(allocations_dynamic) <- (1:horizon)
  allocations_dynamic_list[[horizon]] <- allocations_dynamic
  allocations_myopic <- matrix(0,nrow=horizon, ncol=num_assets)
  colnames(allocations_myopic) <- assets
  rownames(allocations_myopic) <- (1:horizon)
  allocations_myopic_list[[horizon]] <- allocations_myopic
  allocations_buyHold <- matrix(0,nrow=horizon, ncol=num_assets)
  colnames(allocations_buyHold) <- assets
  rownames(allocations_buyHold) <- (1:horizon)
  allocations_buyHold_list[[horizon]] <- allocations_buyHold
}

#======== Section 9: Generate all possible allocations =================

all_allocations <- matrix(NA,nrow=200000,ncol=num_assets)
firstRow_exception <- T
next_row <- 1

#generate a grid of weights for the allocation to 5 year bonds
increment_value <- 0.1
allocations_assets <- seq(0,1,by=increment_value)

for(asset_1 in allocations_assets){
  print(paste("the first asset is at ",as.character(asset_1),sep=''))
  asset_2 <- allocations_assets[1]
  while(round(asset_1+asset_2,2)<=1.00){
    print(paste("the second asset is at ",as.character(asset_2),sep=''))
    asset_3 <- allocations_assets[1]
    while(round(asset_1+asset_2+asset_3,2)<=1.00){
      #print(paste("the third asset is at ",as.character(asset_3),sep=''))
      asset_4 <- allocations_assets[1]
      while(round(asset_1+asset_2+asset_3+asset_4,2)<=1.00){
        #print(paste("the fourth asset is at ",as.character(asset_4),sep=''))
        asset_5 <- allocations_assets[1]
        while(round(asset_1+asset_2+asset_3+asset_4+asset_5,2)<=1.00){
          asset_6 <- allocations_assets[1]
          while(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6,2)<=1.00){
            asset_7 <- allocations_assets[1]
            while(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7,2)<=1.00){
              asset_8 <- allocations_assets[1]
              while(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8,2)<=1.00){
                asset_9 <- allocations_assets[1]
                while(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9,2)<=1.00){
                  asset_10 <- allocations_assets[1]
                  while(round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10,2)<=1.00){
                    asset_11 <- allocations_assets[1]
                    while (round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10+asset_11,2)<=1.00){
                      if (round(asset_1+asset_2+asset_3+asset_4+asset_5+asset_6+asset_7+asset_8+asset_9+asset_10+asset_11,2)==1.00){
                        allocation_vector <- c(asset_1,asset_2,asset_3,
                                               asset_4,asset_5,asset_6,
                                               asset_7,asset_8,asset_9,
                                               asset_10,asset_11)
                        all_allocations[next_row,] <- allocation_vector
                        next_row <- next_row + 1
                      }
                      asset_11 <- asset_11 + increment_value
                    }
                    asset_10 <- asset_10 + increment_value
                  }
                  asset_9 <- asset_9 + increment_value
                }
                asset_8 <- asset_8 + increment_value
              }
              asset_7 <- asset_7 + increment_value
            }
            asset_6 <- asset_6 + increment_value
          }
          asset_5 <- asset_5 + increment_value
        }
        asset_4 <- asset_4 + increment_value
      }
      asset_3 <- asset_3 + increment_value
    }
    asset_2 <- asset_2 + increment_value
  }
}

#Need to satisfy the following constraints for allocation:
#All allocations have to add up to 1
#No short-selling or leveraging, so all allocations should be between or equal to 0 and 1
#Hedge allocation cannot be more than twice that of the bond allocation, in order hold enough collateral for the swaps within the hedge portfolio


#reset the names of the rows of the matrix, for convenience
rownames(all_allocations) <- NULL
#convert to dataframe, for calculation convenience later on
all_allocations <- as.data.frame(all_allocations)

all_allocations <- all_allocations %>% filter(!is.na(all_allocations[,1]))

names(all_allocations) <- assets
total_Allocation_count <- nrow(all_allocations)

#save(all_allocations,file="C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/all_allocations_five.RData")

#load("C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/R code/all_allocations.RData")

#==== Section 10: simulate the optimal allocation, through maximising expected utility for dynamic, myopic and Buy&Hold portfolios

#all_allocations <- all_allocations[121736:(121736+1000),]
#rownames(all_allocations) <- NULL
#Run the optimisation over the base case dataset

create_output <- function(type_strat){
  if (type_strat==1){
    
  } else if (type_strat==2){
    
  } else if (type_strat==3){
    
  } else {
    
  }
}

for (i in 1:4){
  if (i==1){
    load('cluster_0_input.RData')
    ESG_constraint <- F
    output_file_name <- "simple_returnOnly_optimal_allocations_v2.RData"
  } else if (i==2){
    load('cluster_0_input.RData')
    ESG_constraint <- T
    output_file_name <- "simple_ESGRestricted_optimal_allocations_v2.RData"
  } else if (i==3){
    load('cluster_0_input_kmeans.RData')
    ESG_constraint <- F
    output_file_name <- "kmeans_returnOnly_optimal_allocations_v2.RData"
  } else {
    load('cluster_0_input_kmeans.RData')
    ESG_constraint <- T
    output_file_name <- "kmeans_ESGRestricted_optimal_allocations_v2.RData"
  }
  
  if (hyperParm_tuning){
    optimal_hyperparameters <- get_optimal_allocation(return_var_train_list,state_var_train_list,all_allocations,ESG_constraint,final_esg_score_list[[as.character(0)]],
                                                      ESG_threshold,env_weight_list[1],soc_weight_list[1],gov_weight_list[1],30000)
  } else {
    optimal_allocations <- get_optimal_allocation(return_var_train_list,state_var_train_list,all_allocations,ESG_constraint,final_esg_score_list[[as.character(0)]],
                                                  ESG_threshold,env_weight_list[1],soc_weight_list[1],gov_weight_list[1],30000)
  }
  
  save(optimal_allocations,file=output_file_name)
}


if (hyperParm_tuning){
  optimal_hyperparameters <- get_optimal_allocation(return_var_train_list,state_var_train_list,all_allocations,ESG_constraint,final_esg_score_list[[as.character(0)]],
                                                    ESG_threshold,env_weight_list[1],soc_weight_list[1],gov_weight_list[1],30000)
} else {
  optimal_allocations <- get_optimal_allocation(return_var_train_list,state_var_train_list,all_allocations,ESG_constraint,final_esg_score_list[[as.character(0)]],
                                                ESG_threshold,env_weight_list[1],soc_weight_list[1],gov_weight_list[1],30000)
}


#Export in a RData file type to Documents
optimal_allocations_kmeans_returnOnly <- optimal_allocations
save(optimal_allocations_kmeans_returnOnly,file='kmeans_returnOnly_optimal_allocations.RData')
#save(optimal_allocations_simple_returnOnly,file='simple_returnOnly_optimal_allocations_final.RData')