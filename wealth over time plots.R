#load packages

library(ggplot2)
library(dplyr)

#=============== Section 1: load all asset allocations ========

assets <- c( "Tbill_return","Tnote_return","corBond_return",
             "cluster_return_1","cluster_return_2","cluster_return_3",
             "cluster_return_4","cluster_return_5","cluster_return_6",
             "cluster_return_7","cluster_return_8")

load("return_var_test_list_kmeans.RData")
return_test_set <- return_var_test_list

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
    scale_color_manual(values=c("red", "blue", "green"))
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
  }
}

#=============== Section 4: 1 over N allocation, with equal weight across all assets ========

oneOverN_allocation <- as.data.frame(matrix(round(1/length(assets),2),nrow=15,ncol=11))
colnames(oneOverN_allocation) <- assets

CE_oneOverN_horizons <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_oneOverN_horizons) <- "oneOverN"
for (horizon in 1:15){
  CE_oneOverN_horizons[horizon,1] <- get_CE(oneOverN_allocation[1:horizon,],return_test_set,horizon)
  if (horizon==15){
    wealthPerScenario_oneOverN <- get_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set,horizon)
  }
}

#=============== Section 5: Create CE plot with all asset allocations ========

CE_all <- cbind(CE_oneOverNFair_horizons,CE_oneOverN_horizons)

CE_plot(CE_all)
