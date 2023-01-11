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

load("return_var_test_list_simple.RData")
return_test_set <- return_var_test_list
load("return_var_test_list_kmeans.RData")
return_test_set_kmeans <- return_var_test_list

source("Utility Functions.R")

#=============== Section 2: load functions ========

get_terminal_wealth_perScenario <- function(opt_allocation,return_list,max_horizon){
  
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

get_turnover <- function(opt_allocation,return_list,max_horizon){
  
  Nscenarios <- nrow(return_list[[1]])
  
  total_turnover <- matrix(0,Nscenarios, 1)
  for (t in 1:(max_horizon)){
    num_col <- 1
    portfolio_return <- matrix(0,nrow=Nscenarios,ncol=1)
    for(var in names(return_list)){
      portfolio_return[,1] <- portfolio_return[,1] + return_list[[var]][,t]*opt_allocation[t,num_col]
      num_col <- num_col + 1
    }
    
    num_col <- 1
    weighted_return <- matrix(0,nrow=Nscenarios,ncol=length(names(return_list)))
    rebalanced_weight <- matrix(0,nrow=Nscenarios,ncol=length(names(return_list)))
    turnover_perAsset <- matrix(0,nrow=Nscenarios,ncol=length(names(return_list)))
    turnover_forPeriod <- matrix(0,nrow=Nscenarios,ncol=1)
    for(var in names(return_list)){
      weighted_return[,num_col] <- return_list[[var]][,t]/portfolio_return[,1]
      rebalanced_weight[,num_col] <- opt_allocation[t,num_col]*weighted_return[,num_col]
      turnover_perAsset[,num_col] <- abs(rebalanced_weight[,num_col] - opt_allocation[t,num_col])
      num_col <- num_col + 1
    }
    turnover_forPeriod[,1] <- apply(turnover_perAsset,1,sum)
    total_turnover[,1] <- total_turnover[,1] + turnover_forPeriod[,1]
  }
  total_turnover[,1] <- total_turnover[,1]/max_horizon
  mean_turnover_across_scenarios <- mean(total_turnover)
  return(mean_turnover_across_scenarios)
}

get_wealth_perScenario <- function(opt_allocation,return_list,max_horizon){
  
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
  return(wealth)
}

get_CE <- function(opt_allocation,return_list,max_horizon){
  wealth_build_up <- get_terminal_wealth_perScenario(opt_allocation,return_list,max_horizon)
  
  CE_rational <- CE(wealth_build_up,gamma = 5)
  
  return(CE_rational)
}

CE_plot <- function(CE_over_horizons){
  
  CE_graph <- cbind(1:15,stack(CE_over_horizons[1:ncol(CE_over_horizons)]))
  colnames(CE_graph) = cbind('Horizon', 'CE', 'Strategy')

  # stacked area chart
  ggplot(CE_graph, aes(x=Horizon, y=CE,fill=Strategy,color=Strategy)) + 
    theme_classic() +
    labs(y= "Certainty Equivalent Rate", x = "Horizon") +
    geom_line() +
    scale_x_continuous(breaks = c(3,6,9,12,15))+
    scale_color_manual(values=c("red", "blue", "green","black","purple","cyan","pink","orange"))
}

wealth_uncertainty_plot <- function(wealthPerScenario){
  
  wealth_graph <- data.frame(Horizon=1:ncol(wealthPerScenario),t(wealthPerScenario)) %>% gather(key = Sim,value=y,-Horizon)
  
  ggplot(wealth_graph, aes(x=Horizon,y=y)) + 
    geom_fan() + 
    labs(y= "Accumulated Wealth", x = "Horizon") +
    ylim(0,7) +
    theme_bw() + 
    scale_fill_distiller(palette="Spectral") +
    scale_x_continuous(breaks = c(3,6,9,12,15))
}

#=============== Section 3: 1 over N allocation, with equal weight across asset classes ========

bond_allocations <- as.data.frame(matrix(round(1/4,2),nrow=15,ncol=3))
equity_allocations <- as.data.frame(matrix(round(0.25/8,2),nrow=15,ncol=8))

oneOverN_allocation_equityFair <- cbind(bond_allocations,equity_allocations)
colnames(oneOverN_allocation_equityFair) <- assets

CE_oneOverNFair_horizons <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_oneOverNFair_horizons) <- "oneOverNFair"
for (horizon in 1:15){
  CE_oneOverNFair_horizons[horizon,1] <- get_CE(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon)
  if (horizon==15){
    wealthPerScenario_oneOverNFair <- get_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon)
    mean_terminal_wealth_oneOverNFair <- mean(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon))
    stdev_terminal_wealth_oneOverNFair <- sd(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon))
    SR_oneOverNFair <- mean_terminal_wealth_oneOverNFair/stdev_terminal_wealth_oneOverNFair
    hist(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon))
    turnover_oneOverNFair <- get_turnover(oneOverN_allocation_equityFair[1:horizon,],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_oneOverNFair)

#=============== Section 4: 1 over N allocation, with equal weight across all assets ========

oneOverN_allocation <- as.data.frame(matrix(round(1/length(assets),2),nrow=15,ncol=11))
colnames(oneOverN_allocation) <- assets

CE_oneOverN_horizons <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_oneOverN_horizons) <- "oneOverN"
for (horizon in 1:15){
  CE_oneOverN_horizons[horizon,1] <- get_CE(oneOverN_allocation[1:horizon,],return_test_set,horizon)
  if (horizon==15){
    wealthPerScenario_oneOverN <- get_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set,horizon)
    mean_terminal_wealth_oneOverN <- mean(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set,horizon))
    stdev_terminal_wealth_oneOverN <- sd(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set,horizon))
    hist(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set,horizon))
    turnover_oneOverN <- get_turnover(oneOverN_allocation[1:horizon,],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_oneOverN)

#=============== Section 5: Optimal asset allocations for return-only and simple sorting  ========================

load('simple_returnOnly_optimal_allocations_final.RData')

#perform for the Dynamic Allocation here

CE_simple_returnOnly_horizons_Dynamic <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_simple_returnOnly_horizons_Dynamic) <- "simple, return-only, Dynamic"
for (horizon in 1:15){
  if (horizon==1){
    CE_simple_returnOnly_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon+1)
  } else {
    CE_simple_returnOnly_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon)
  }
  if (horizon==15){
    wealthPerScenario_simple_returnOnly_dynamic <- get_wealth_perScenario(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon)
    mean_terminal_wealth_simple_returnOnly <- mean(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon))
    stdev_terminal_wealth_simple_returnOnly <- sd(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon))
    turnover_simple_returnOnly <- get_turnover(optimal_allocations_simple_returnOnly[['Dynamic']][[horizon]],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_simple_returnOnly_dynamic)

#perform for the Buy&Hold Allocation here

CE_simple_returnOnly_horizons_buyHold <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_simple_returnOnly_horizons_buyHold) <- "simple, return-only, Buy&Hold"
for (horizon in 1:15){
  if (horizon==1){
    CE_simple_returnOnly_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon+1)
  } else {
    CE_simple_returnOnly_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon)
  }
  if (horizon==15){
    wealthPerScenario_simple_returnOnly_buyHold <- get_wealth_perScenario(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon)
    mean_terminal_wealth_simple_returnOnly_buyHold <- mean(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon))
    stdev_terminal_wealth_simple_returnOnly_buyHold <- sd(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon))
    turnover_simple_returnOnly_buyHold <- get_turnover(optimal_allocations_simple_returnOnly[['BuyHold']][[horizon]],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_simple_returnOnly_buyHold)

#=============== Section 5: Optimal asset allocations for ESG restricted and simple sorting  ========================

load('simple_ESGRestricted_optimal_allocations.RData')

#perform for the Dynamic Allocation here

CE_simple_ESG_horizons_Dynamic <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_simple_ESG_horizons_Dynamic) <- "simple, ESG-restricted, Dynamic"
for (horizon in 1:15){
  if (horizon==1){
    CE_simple_ESG_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon+1)
  } else {
    CE_simple_ESG_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon)
  }
  if (horizon==15){
    wealthPerScenario_simple_ESG_dynamic <- get_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon)
    mean_terminal_wealth_simple_ESG <- mean(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon))
    stdev_terminal_wealth_simple_ESG <- sd(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon))
    turnover_simple_ESG <- get_turnover(optimal_allocations_simple_ESGRestricted[['Dynamic']][[horizon]],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_simple_ESG_dynamic)

#perform for the Buy&Hold Allocation here

CE_simple_ESG_horizons_buyHold <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_simple_ESG_horizons_buyHold) <- "simple, ESG-restricted, Buy&Hold"
for (horizon in 1:15){
  if (horizon==1){
    CE_simple_ESG_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon+1)
  } else {
    CE_simple_ESG_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon)
  }
  if (horizon==15){
    wealthPerScenario_simple_ESG_buyHold <- get_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon)
    mean_terminal_wealth_simple_ESG_buyHold <- mean(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon))
    stdev_terminal_wealth_simple_ESG_buyHold <- sd(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon))
    turnover_simple_ESG_buyHold <- get_turnover(optimal_allocations_simple_ESGRestricted[['BuyHold']][[horizon]],return_test_set,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_simple_ESG_buyHold)

#=============== Section 6: Optimal asset allocations for ESG restricted and kmeans sorting  ========================

load('kmeans_ESGRestricted_optimal_allocations.RData')

#perform for the Dynamic Allocation here

CE_kmeans_ESG_horizons_Dynamic <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_kmeans_ESG_horizons_Dynamic) <- "kmeans, ESG-restricted, Dynamic"
for (horizon in 1:15){
  if (horizon==1){
    CE_kmeans_ESG_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon+1)
  } else {
    CE_kmeans_ESG_horizons_Dynamic[horizon,1] <- get_CE(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon)
  }
  if (horizon==15){
    wealthPerScenario_kmeans_ESG_dynamic <- get_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon)
    mean_terminal_wealth_kmeans_ESG <- mean(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon))
    stdev_terminal_wealth_kmeans_ESG <- sd(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon))
    turnover_kmeans_ESG <- get_turnover(optimal_allocations_kmeans_ESGRestricted_final[['Dynamic']][[horizon]],return_test_set_kmeans,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_kmeans_ESG_dynamic)

#perform for the Buy&Hold Allocation here

CE_kmeans_ESG_horizons_buyHold <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_kmeans_ESG_horizons_buyHold) <- "kmeans, ESG-restricted, Buy&Hold"
for (horizon in 1:15){
  if (horizon==1){
    CE_kmeans_ESG_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon+1)
  } else {
    CE_kmeans_ESG_horizons_buyHold[horizon,1] <- get_CE(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon)
  }
  if (horizon==15){
    wealthPerScenario_kmeans_ESG_buyHold <- get_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon)
    mean_terminal_wealth_kmeans_ESG_buyHold <- mean(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon))
    stdev_terminal_wealth_kmeans_ESG_buyHold <- sd(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon))
    hist(get_terminal_wealth_perScenario(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon))
    turnover_kmeans_ESG_buyHold <- get_turnover(optimal_allocations_kmeans_ESGRestricted_final[['BuyHold']][[horizon]],return_test_set_kmeans,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_kmeans_ESG_buyHold)

#=============== Section End: Create CE plot with all asset allocations ========

#add other asset allocations here
CE_all <- cbind(CE_oneOverNFair_horizons,CE_oneOverN_horizons,CE_simple_returnOnly_horizons_Dynamic,
                CE_simple_returnOnly_horizons_buyHold,CE_simple_ESG_horizons_Dynamic,
                CE_simple_ESG_horizons_buyHold,CE_kmeans_ESG_horizons_Dynamic,CE_kmeans_ESG_horizons_buyHold)

#show the CE plot
CE_plot(CE_all)
