library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(writexl)
library(ggridges)
library(hrbrthemes)

#============== Section 1: load necessary functions ===============================

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

#============== Section 2: load data ===============================

assets <- c( "Tbill_return","Tnote_return","corBond_return",
             "cluster_return_1","cluster_return_2","cluster_return_3",
             "cluster_return_4","cluster_return_5","cluster_return_6",
             "cluster_return_7","cluster_return_8")

load('kmeans_ESGRestricted_optimal_allocations_vfinal_65.RData')
optimal_allocations_kmeans_averageInvestor <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor1_v3.RData')
optimal_allocations_kmeans_investor1 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor2_v3.RData')
optimal_allocations_kmeans_investor2 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor3_v3.RData')
optimal_allocations_kmeans_investor3 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor4_v3.RData')
optimal_allocations_kmeans_investor4 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor5_v3.RData')
optimal_allocations_kmeans_investor5 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_investor6_v3.RData')
optimal_allocations_kmeans_investor6 <- optimal_allocations[['Dynamic']]

load("return_var_test_list_kmeans.RData")
return_test_set_kmeans_averageInvestor <- return_var_test_list

load("return_var_test_list_kmeans_cluster1.RData")
return_test_set_kmeans_investor1 <- return_var_test_list

load("return_var_test_list_kmeans_cluster2.RData")
return_test_set_kmeans_investor2 <- return_var_test_list

load("return_var_test_list_kmeans_cluster3.RData")
return_test_set_kmeans_investor3 <- return_var_test_list

load("return_var_test_list_kmeans_cluster4.RData")
return_test_set_kmeans_investor4 <- return_var_test_list

load("return_var_test_list_kmeans_cluster5.RData")
return_test_set_kmeans_investor5 <- return_var_test_list

load("return_var_test_list_kmeans_cluster6.RData")
return_test_set_kmeans_investor6 <- return_var_test_list

source("Utility Functions.R")

#=============== section 3: obtain terminal wealth for horizon 15 ====================

terminal_wealth_averageInvestor <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_averageInvestor[[15]],return_test_set_kmeans_averageInvestor,15)
terminal_wealth_investor1 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor1[[15]],return_test_set_kmeans_investor1,15)
terminal_wealth_investor2 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor2[[15]],return_test_set_kmeans_investor2,15)
terminal_wealth_investor3 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor3[[15]],return_test_set_kmeans_investor3,15)
terminal_wealth_investor4 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor4[[15]],return_test_set_kmeans_investor4,15)
terminal_wealth_investor5 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor5[[15]],return_test_set_kmeans_investor5,15)
terminal_wealth_investor6 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_investor6[[15]],return_test_set_kmeans_investor6,15)

df_averageInvestor <- as.data.frame(terminal_wealth_averageInvestor)
df_averageInvestor[,2] <- 'Average investor'
df_investor1 <- as.data.frame(terminal_wealth_investor1)
df_investor1[,2] <- 'Investor 1'
df_investor2 <- as.data.frame(terminal_wealth_investor2)
df_investor2[,2] <- 'Investor 2'
df_investor3 <- as.data.frame(terminal_wealth_investor3)
df_investor3[,2] <- 'Investor 3'
df_investor4 <- as.data.frame(terminal_wealth_investor4)
df_investor4[,2] <- 'Investor 4'
df_investor5 <- as.data.frame(terminal_wealth_investor5)
df_investor5[,2] <- 'Investor 5'
df_investor6 <- as.data.frame(terminal_wealth_investor6)
df_investor6[,2] <- 'Investor 6'
colnames(df_averageInvestor) <- c(1,2)
colnames(df_investor1) <- c(1,2)
colnames(df_investor2) <- c(1,2)
colnames(df_investor3) <- c(1,2)
colnames(df_investor4) <- c(1,2)
colnames(df_investor5) <- c(1,2)
colnames(df_investor6) <- c(1,2)

df_total <- rbind(df_averageInvestor,df_investor1,df_investor2,
                  df_investor3,df_investor4,df_investor5,df_investor6)
colnames(df_total) <- c('terminal_wealth','ESG_investor')
df_total[,2] <- as.character(df_total[,2])

#plot the histograms
ggplot(df_total, aes(x = terminal_wealth, y = ESG_investor)) + 
  geom_density_ridges() +
  labs(y= "ESG investor", x = "Terminal wealth's density")
