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
    scale_color_manual(values=c("red", "blue", "green","black","purple",
                                     "cyan","pink","orange","yellow","darkgreen",
                                     "grey","brown","beige","darkgoldenrod3"))
    #scale_color_manual(values=c("red", "blue", "green","black","purple","cyan","pink","orange","yellow","darkgreen"))
}

wealth_uncertainty_plot <- function(wealthPerScenario){
  
  wealth_graph <- data.frame(Horizon=1:ncol(wealthPerScenario),t(wealthPerScenario)) %>% gather(key = Sim,value=y,-Horizon)
  
  plot <- ggplot(wealth_graph, aes(x=Horizon,y=y)) + 
    geom_fan() + 
    labs(y= "Accumulated Wealth", x = "Horizon") +
    ylim(0,7) +
    theme_bw() + 
    scale_fill_distiller(palette="Spectral") +
    scale_x_continuous(breaks = c(3,6,9,12,15))
  print(plot)
}

Generate_all_plots <- function(optimal_allocation,return_set,colname){
  
  CE_df_dyn <- as.data.frame(matrix(0,nrow=15,ncol=1))
  colnames(CE_df_dyn) <- paste(colname,"Dynamic",sep=' ')
  for (horizon in 1:15){
    if (horizon==1){
      CE_df_dyn[horizon,1] <- get_CE(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon+1)
    } else {
      CE_df_dyn[horizon,1] <- get_CE(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon)
    }
    if (horizon==15){
      wealthPerScenario_dyn <- get_wealth_perScenario(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon)
      mean_terminal_wealth_dyn <- mean(get_terminal_wealth_perScenario(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon))
      stdev_terminal_wealth_dyn <- sd(get_terminal_wealth_perScenario(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon))
      hist(get_terminal_wealth_perScenario(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon))
      turnover_dyn <- get_turnover(optimal_allocation[['Dynamic']][[horizon]],return_set,horizon)
    }
  }
  
  wealth_uncertainty_plot(wealthPerScenario_dyn)
  
  CE_df_BH <- as.data.frame(matrix(0,nrow=15,ncol=1))
  colnames(CE_df_BH) <- paste(colname,"Buy&Hold",sep=' ')
  for (horizon in 1:15){
    if (horizon==1){
      CE_df_BH[horizon,1] <- get_CE(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon+1)
    } else {
      CE_df_BH[horizon,1] <- get_CE(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon)
    }
    if (horizon==15){
      wealthPerScenario_BH <- get_wealth_perScenario(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon)
      mean_terminal_wealth_BH <- mean(get_terminal_wealth_perScenario(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon))
      stdev_terminal_wealth_BH <- sd(get_terminal_wealth_perScenario(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon))
      hist(get_terminal_wealth_perScenario(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon))
      turnover_BH <- get_turnover(optimal_allocation[['BuyHold']][[horizon]],return_set,horizon)
    }
  }
  
  wealth_uncertainty_plot(wealthPerScenario_BH)
  return(c(CE_df_dyn,mean_terminal_wealth_dyn,stdev_terminal_wealth_dyn,turnover_dyn,
           CE_df_BH,mean_terminal_wealth_BH,stdev_terminal_wealth_BH,turnover_BH))
}

#=============== Section 3: 1 over N allocation, with equal weight across asset classes ========

bond_allocations <- as.data.frame(matrix(round(1/4,2),nrow=15,ncol=3))
equity_allocations <- as.data.frame(matrix(round(0.25/8,2),nrow=15,ncol=8))

oneOverN_allocation_equityFair <- cbind(bond_allocations,equity_allocations)
colnames(oneOverN_allocation_equityFair) <- assets

CE_oneOverNFair_horizons <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_oneOverNFair_horizons) <- "oneOverNFair"
for (horizon in 1:15){
  CE_oneOverNFair_horizons[horizon,1] <- get_CE(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon)
  if (horizon==15){
    wealthPerScenario_oneOverNFair <- get_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon)
    mean_terminal_wealth_oneOverNFair <- mean(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon))
    stdev_terminal_wealth_oneOverNFair <- sd(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon))
    SR_oneOverNFair <- mean_terminal_wealth_oneOverNFair/stdev_terminal_wealth_oneOverNFair
    hist(get_terminal_wealth_perScenario(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon))
    turnover_oneOverNFair <- get_turnover(oneOverN_allocation_equityFair[1:horizon,],return_test_set_kmeans,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_oneOverNFair)

output_table <- as.data.frame(matrix(0,nrow=8,ncol=4))
colnames(output_table) <- c("Strategy","Average Terminal Wealth","Stdev Terminal Wealth","Turnover")
output_table[1,] <- c("1 over N fair",mean_terminal_wealth_oneOverNFair,
                      stdev_terminal_wealth_oneOverNFair,turnover_oneOverNFair)

#=============== Section 4: 1 over N allocation, with equal weight across all assets ========

oneOverN_allocation <- as.data.frame(matrix(round(1/length(assets),2),nrow=15,ncol=11))
colnames(oneOverN_allocation) <- assets

CE_oneOverN_horizons <- as.data.frame(matrix(0,nrow=15,ncol=1))
colnames(CE_oneOverN_horizons) <- "oneOverN"
for (horizon in 1:15){
  CE_oneOverN_horizons[horizon,1] <- get_CE(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon)
  if (horizon==15){
    wealthPerScenario_oneOverN <- get_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon)
    mean_terminal_wealth_oneOverN <- mean(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon))
    stdev_terminal_wealth_oneOverN <- sd(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon))
    hist(get_terminal_wealth_perScenario(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon))
    turnover_oneOverN <- get_turnover(oneOverN_allocation[1:horizon,],return_test_set_kmeans,horizon)
  }
}

wealth_uncertainty_plot(wealthPerScenario_oneOverN)

output_table[2,] <- c("1 over N",mean_terminal_wealth_oneOverN,
                      stdev_terminal_wealth_oneOverN,turnover_oneOverN)

#=============== Section 5: Optimal asset allocations for return-only and simple sorting  ========================

load('simple_returnOnly_optimal_allocations_vfinal_55.RData')
optimal_allocations_simple_returnOnly <- optimal_allocations

output_vector <- Generate_all_plots(optimal_allocations_simple_returnOnly,return_test_set_kmeans,"simple, return-only,")

CE_simple_returnOnly_horizons_Dynamic <- output_vector[[1]]
mean_terminal_wealth_simple_returnOnly <- output_vector[[2]]
stdev_terminal_wealth_simple_returnOnly <- output_vector[[3]]
turnover_simple_returnOnly <- output_vector[[4]]
CE_simple_returnOnly_horizons_buyHold <- output_vector[[5]]
mean_terminal_wealth_simple_returnOnly_buyHold <- output_vector[[6]]
stdev_terminal_wealth_simple_returnOnly_buyHold <- output_vector[[7]]
turnover_simple_returnOnly_buyHold <- output_vector[[8]]
#perform for the Dynamic Allocation here

output_table[3,] <- c("Simple, return-only, Dynamic",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[4,] <- c("Simple, return-only, Buy&Hold",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 5: Optimal asset allocations for ESG restricted and simple sorting  ========================

load('simple_ESGRestricted_optimal_allocations_vfinal_55.RData')
optimal_allocations_simple_ESGRestricted <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_simple_ESGRestricted,return_test_set_kmeans,"simple, ESG-restricted,")

CE_simple_ESG_horizons_Dynamic_55 <- output_vector[[1]]
mean_terminal_wealth_simple_ESG <- output_vector[[2]]
stdev_terminal_wealth_simple_ESG <- output_vector[[3]]
turnover_simple_ESG <- output_vector[[4]]
CE_simple_ESG_horizons_buyHold_55 <- output_vector[[5]]
mean_terminal_wealth_simple_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_simple_ESG_buyHold <- output_vector[[7]]
turnover_simple_ESG_buyHold <- output_vector[[8]]

output_table[5,] <- c("Simple, ESG restricted, Dynamic, 55",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[6,] <- c("Simple, ESG restricted, Buy&Hold, 55",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 6: Optimal asset allocations for return-only and kmeans sorting  ========================

load('kmeans_returnOnly_optimal_allocations_vfinal_55.RData')
optimal_allocations_kmeans_returnOnly <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_kmeans_returnOnly,return_test_set_kmeans,"K-means, return-only,")

CE_kmeans_returnOnly_horizons_Dynamic <- output_vector[[1]]
mean_terminal_wealth_kmeans_returnOnly <- output_vector[[2]]
stdev_terminal_wealth_kmeans_returnOnly <- output_vector[[3]]
turnover_kmeans_returnOnly <- output_vector[[4]]
CE_kmeans_returnOnly_horizons_buyHold <- output_vector[[5]]
mean_terminal_wealth_kmeans_returnOnly_buyHold <- output_vector[[6]]
stdev_terminal_wealth_kmeans_returnOnly_buyHold <- output_vector[[7]]
turnover_kmeans_returnOnly_buyHold <- output_vector[[8]]

output_table[7,] <- c("K-means, return-only, Dynamic",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[8,] <- c("K-means, return-only, Buy&Hold",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 7: Optimal asset allocations for ESG restricted and kmeans sorting  ========================

load('kmeans_ESGRestricted_optimal_allocations_vfinal_55.RData')
optimal_allocations_kmeans_ESGRestricted_final <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_kmeans_ESGRestricted_final,return_test_set_kmeans,"K-means, ESG-restricted,")

CE_kmeans_ESG_horizons_Dynamic_55 <- output_vector[[1]]
mean_terminal_wealth_kmeans_ESG <- output_vector[[2]]
stdev_terminal_wealth_kmeans_ESG <- output_vector[[3]]
turnover_kmeans_ESG <- output_vector[[4]]
CE_kmeans_ESG_horizons_buyHold_55 <- output_vector[[5]]
mean_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[7]]
turnover_kmeans_ESG_buyHold <- output_vector[[8]]

output_table[9,] <- c("K-means, ESG restricted, Dynamic, 55",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[10,] <- c("K-means, ESG restricted, Buy&Hold, 55",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 8: Optimal asset allocations for ESG restricted and kmeans sorting, V6  ========================

load('kmeans_ESGRestricted_optimal_allocations_vfinal_65.RData')
optimal_allocations_kmeans_ESGRestricted_final <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_kmeans_ESGRestricted_final,return_test_set_kmeans,"K-means, ESG-restricted,")

CE_kmeans_ESG_horizons_Dynamic_65 <- output_vector[[1]]
mean_terminal_wealth_kmeans_ESG <- output_vector[[2]]
stdev_terminal_wealth_kmeans_ESG <- output_vector[[3]]
turnover_kmeans_ESG <- output_vector[[4]]
CE_kmeans_ESG_horizons_buyHold_65 <- output_vector[[5]]
mean_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[7]]
turnover_kmeans_ESG_buyHold <- output_vector[[8]]

output_table[11,] <- c("K-means, ESG restricted, Dynamic, 65",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[12,] <- c("K-means, ESG restricted, Buy&Hold, 65",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 8: Optimal asset allocations for ESG restricted and kmeans sorting, V7  ========================

load('kmeans_ESGRestricted_optimal_allocations_vfinal_75.RData')
optimal_allocations_kmeans_ESGRestricted_final <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_kmeans_ESGRestricted_final,return_test_set_kmeans,"K-means, ESG-restricted,")

CE_kmeans_ESG_horizons_Dynamic_75 <- output_vector[[1]]
mean_terminal_wealth_kmeans_ESG <- output_vector[[2]]
stdev_terminal_wealth_kmeans_ESG <- output_vector[[3]]
turnover_kmeans_ESG <- output_vector[[4]]
CE_kmeans_ESG_horizons_buyHold_75 <- output_vector[[5]]
mean_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_kmeans_ESG_buyHold <- output_vector[[7]]
turnover_kmeans_ESG_buyHold <- output_vector[[8]]

output_table[13,] <- c("K-means, ESG restricted, Dynamic, 75",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[14,] <- c("K-means, ESG restricted, Buy&Hold, 75",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 8: Optimal asset allocations for ESG restricted and kmeans sorting, V7  ========================

load('simple_ESGRestricted_optimal_allocations_vfinal_65.RData')
optimal_allocations_simple_ESGRestricted <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_simple_ESGRestricted,return_test_set_kmeans,"simple, ESG-restricted,")

CE_simple_ESG_horizons_Dynamic_65 <- output_vector[[1]]
mean_terminal_wealth_simple_ESG <- output_vector[[2]]
stdev_terminal_wealth_simple_ESG <- output_vector[[3]]
turnover_simple_ESG <- output_vector[[4]]
CE_simple_ESG_horizons_buyHold_65 <- output_vector[[5]]
mean_terminal_wealth_simple_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_simple_ESG_buyHold <- output_vector[[7]]
turnover_simple_ESG_buyHold <- output_vector[[8]]

output_table[15,] <- c("Simple, ESG restricted, Dynamic, 65",output_vector[[2]],
                      output_vector[[3]],output_vector[[4]])
output_table[16,] <- c("Simple, ESG restricted, Buy&Hold, 65",output_vector[[6]],
                      output_vector[[7]],output_vector[[8]])

#=============== Section 8: Optimal asset allocations for ESG restricted and kmeans sorting, V7  ========================

load('simple_ESGRestricted_optimal_allocations_vfinal_75.RData')
optimal_allocations_simple_ESGRestricted <- optimal_allocations

#perform for the Dynamic Allocation here
output_vector <- Generate_all_plots(optimal_allocations_simple_ESGRestricted,return_test_set_kmeans,"simple, ESG-restricted,")

CE_simple_ESG_horizons_Dynamic_75 <- output_vector[[1]]
mean_terminal_wealth_simple_ESG <- output_vector[[2]]
stdev_terminal_wealth_simple_ESG <- output_vector[[3]]
turnover_simple_ESG <- output_vector[[4]]
CE_simple_ESG_horizons_buyHold_75 <- output_vector[[5]]
mean_terminal_wealth_simple_ESG_buyHold <- output_vector[[6]]
stdev_terminal_wealth_simple_ESG_buyHold <- output_vector[[7]]
turnover_simple_ESG_buyHold <- output_vector[[8]]

output_table[17,] <- c("Simple, ESG restricted, Dynamic, 75",output_vector[[2]],
                       output_vector[[3]],output_vector[[4]])
output_table[18,] <- c("Simple, ESG restricted, Buy&Hold, 75",output_vector[[6]],
                       output_vector[[7]],output_vector[[8]])

#=============== Section End: Create CE plot with all asset allocations ========

output_table[,2] <- as.numeric(output_table[,2])
output_table[,3] <- as.numeric(output_table[,3])
output_table[,4] <- as.numeric(output_table[,4])
output_table[,2:4] <- round(output_table[,2:4],3)
#add other asset allocations here

# CE_all <- cbind(CE_oneOverNFair_horizons,CE_oneOverN_horizons,
#                 CE_simple_returnOnly_horizons_Dynamic,CE_simple_returnOnly_horizons_buyHold,
#                 CE_simple_ESG_horizons_Dynamic,CE_simple_ESG_horizons_buyHold,
#                 CE_kmeans_returnOnly_horizons_Dynamic,CE_kmeans_returnOnly_horizons_buyHold,
#                 CE_kmeans_ESG_horizons_Dynamic,CE_kmeans_ESG_horizons_buyHold)

CE_all <- cbind(CE_oneOverNFair_horizons,CE_oneOverN_horizons,
                CE_simple_returnOnly_horizons_Dynamic,CE_simple_returnOnly_horizons_buyHold,
                CE_kmeans_returnOnly_horizons_Dynamic,CE_kmeans_returnOnly_horizons_buyHold,
                CE_kmeans_ESG_horizons_Dynamic_65,CE_kmeans_ESG_horizons_buyHold_65,
                CE_simple_ESG_horizons_Dynamic_65,CE_simple_ESG_horizons_buyHold_65)

CE_all <- as.data.frame(CE_all)

colnames(CE_all) <- output_table[c(1,2,3,4,7,8,11,12,15,16),1]

#show the CE plot
CE_plot(CE_all)
