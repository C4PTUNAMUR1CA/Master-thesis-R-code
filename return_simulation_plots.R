#load packages
#install.packages("fanplot")
#install.packages("ggfan")
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggfan)

#=============== Section 1: load all asset allocations ========

assets <- c( "Tbill_return","Tnote_return","corBond_return",
             "cluster_return_1","cluster_return_2","cluster_return_3",
             "cluster_return_4","cluster_return_5","cluster_return_6",
             "cluster_return_7","cluster_return_8")

load("return_var_train_list_kmeans.RData")
return_train_set_kmeans <- return_var_train_list
load("return_var_test_list_kmeans.RData")
return_test_set_kmeans <- return_var_test_list
load("return_var_train_list_simple.RData")
return_train_set_simple <- return_var_train_list
load("return_var_test_list_simple.RData")
return_test_set_simple <- return_var_test_list

source("Utility Functions.R")

#=============== Section 2: load functions ========

create_list_with_combined_equity <- function(simulation_list){
  
  num_scenario <- nrow(simulation_list[[1]])
  equity_return <- matrix(0,nrow=num_scenario,ncol=ncol(simulation_list[[1]]))
  for (scenario in 1:num_scenario){
    for(var in 4:length(simulation_list)){
      equity_return[scenario,] <- equity_return[scenario,] + ((1/8)*simulation_list[[var]][scenario,])
    }
  }
  
  new_list <- list()
  new_list[[names(simulation_list)[1]]] <- simulation_list[[1]]
  new_list[[names(simulation_list)[2]]] <- simulation_list[[2]]
  new_list[[names(simulation_list)[3]]] <- simulation_list[[3]]
  new_list[['equity']] <- equity_return
  
  return(new_list)
}

wealth_uncertainty_plot <- function(wealthPerScenario){
  
  wealth_graph <- data.frame(Horizon=1:ncol(wealthPerScenario),t(wealthPerScenario)) %>% gather(key = Sim,value=y,-Horizon)
  
  ggplot(wealth_graph, aes(x=Horizon,y=y)) + 
    geom_fan() + 
    labs(y= "Cumulative Return", x = "Horizon") +
    theme_bw() + 
    scale_fill_distiller(palette="Spectral") +
    scale_x_continuous(breaks = c(3,6,9,12,15))
}

cumulative_returns <- function(return_list){
  #Returns the cumulative returns between two periods, over all scenarios
  
  cumulative_returns_list <- list()
  
  for (var in names(return_list)){
    cumulative_returns_list[[var]] <- t(apply(return_list[[var]],1,cumprod))
  }
  
  return(cumulative_returns_list)
}

#=============== Section 3: Create simulation plots for kmeans train-set ========

kmeans_train_assetClasses_list <- create_list_with_combined_equity(return_train_set_kmeans)
cum_returns_kmeans_train_list <- cumulative_returns(kmeans_train_assetClasses_list)
wealth_uncertainty_plot(cum_returns_kmeans_train_list[[1]])
wealth_uncertainty_plot(cum_returns_kmeans_train_list[[2]])
wealth_uncertainty_plot(cum_returns_kmeans_train_list[[3]])
wealth_uncertainty_plot(cum_returns_kmeans_train_list[[4]])

#=============== Section 4: Create simulation plots for kmeans test-set ========

kmeans_test_assetClasses_list <- create_list_with_combined_equity(return_test_set_kmeans)
cum_returns_kmeans_test_list <- cumulative_returns(kmeans_test_assetClasses_list)
wealth_uncertainty_plot(cum_returns_kmeans_test_list[[1]])
wealth_uncertainty_plot(cum_returns_kmeans_test_list[[2]])
wealth_uncertainty_plot(cum_returns_kmeans_test_list[[3]])
wealth_uncertainty_plot(cum_returns_kmeans_test_list[[4]])

#=============== Section 5: Create simulation plots for simple train-set ========

simple_train_assetClasses_list <- create_list_with_combined_equity(return_train_set_simple)
cum_returns_simple_train_list <- cumulative_returns(simple_train_assetClasses_list)
wealth_uncertainty_plot(cum_returns_simple_train_list[[1]])
wealth_uncertainty_plot(cum_returns_simple_train_list[[2]])
wealth_uncertainty_plot(cum_returns_simple_train_list[[3]])
wealth_uncertainty_plot(cum_returns_simple_train_list[[4]])

#=============== Section 6: Create simulation plots for simple test-set ========

simple_test_assetClasses_list <- create_list_with_combined_equity(return_test_set_simple)
cum_returns_simple_test_list <- cumulative_returns(simple_test_assetClasses_list)
wealth_uncertainty_plot(cum_returns_simple_test_list[[1]])
wealth_uncertainty_plot(cum_returns_simple_test_list[[2]])
wealth_uncertainty_plot(cum_returns_simple_test_list[[3]])
wealth_uncertainty_plot(cum_returns_simple_test_list[[4]])
