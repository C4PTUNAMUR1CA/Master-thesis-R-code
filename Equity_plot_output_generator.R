#================= Section 0: Import packages ====================

library(openxlsx)
library(writexl)
library(dplyr)

#================= section 1: Change parameters ==================

num_strategies <- 10
equity_allocation_df <- matrix(0,nrow=15,ncol=num_strategies)
equity_allocation_df <- as.data.frame(equity_allocation_df)

cluster <- 0

env_weight_list <- c(0.33,0.7,0.1,0.2,1,0,0)
soc_weight_list <- c(0.33,0.2,0.8,0.1,0,1,0)
gov_weight_list <- c(0.33,0.1,0.1,0.7,0,0,1)

#================= section 2: Import optimal asset allocations ==================

load('simple_returnOnly_optimal_allocations_vfinal_65.RData')
simple_returnOnly_allocations <- optimal_allocations
load('simple_ESGRestricted_optimal_allocations_vfinal_65.RData')
simple_ESGRestricted_allocations <- optimal_allocations
load('kmeans_returnOnly_optimal_allocations_vfinal_65.RData')
kmeans_returnOnly_allocations <- optimal_allocations
load('kmeans_ESGRestricted_optimal_allocations_vfinal_65.RData')
kmeans_ESGRestricted_allocations <- optimal_allocations

simple_file_str_esg_score <- "simple_final_esgScore_cluster"
kmeans_file_str_esg_score <- "kmeans_final_esgScore_cluster"

#================= section 3: function to generate the ESG score of the ESG portfolios ==========

Generate_ESG_score_per_portfolio <- function(file_name,cluster,
                                             env_weight,soc_weight,gov_weight){
  complete_str <- paste(file_name,as.character(cluster),".xlsx",sep="")
  final_esg_score <- read.xlsx(complete_str)
  final_esg_score <- final_esg_score[,2:ncol(final_esg_score)]
  
  final_esg_score[,'ESG total'] <- env_weight*final_esg_score[,'env_2020'] + soc_weight*final_esg_score[,'soc_2020'] + gov_weight*final_esg_score[,'gov_2020']
  
  final_esg_score <- final_esg_score[,'ESG total']
  
  return(as.data.frame(final_esg_score))
}

Obtain_ESG_score_per_horizon <- function(optimal_allocations,ESG_scores){
  
  initial_allocation_df <- as.data.frame(matrix(0,nrow=15,ncol=1))
  for (horizon in 1:15){
    sum_equity_allocation <- sum(optimal_allocations[[horizon]][1,4:ncol(optimal_allocations[[horizon]])])
    weighted_equity_allocation <- optimal_allocations[[horizon]][1,4:ncol(optimal_allocations[[horizon]])]/sum_equity_allocation
    
    initial_allocation_df[horizon,1] <- round(sum(weighted_equity_allocation*ESG_scores[,1]),2)
    
    #initial_allocation_df[horizon,1] <- apply(optimal_allocations[[horizon]][1,4:(ncol(optimal_allocations[[horizon]]))],1,function(x) sum((x/sum(x))*ESG_scores[,1]))
  }
  
  return(initial_allocation_df)
}

Obtain_equity_allocation_per_horizon <- function(optimal_allocations){
  
  initial_allocation_df <- as.data.frame(matrix(0,nrow=15,ncol=1))
  for (horizon in 1:15){
    initial_allocation_df[horizon,1] <- round(sum(optimal_allocations[[horizon]][1,4:ncol(optimal_allocations[[horizon]])]),2)
  }
  
  return(initial_allocation_df)
}

#================= section 4: generate the ESG score of the ESG portfolios ==========

simple_ESG_scores <- Generate_ESG_score_per_portfolio(simple_file_str_esg_score,cluster,
                                 env_weight_list[(cluster+1)],soc_weight_list[(cluster+1)],gov_weight_list[(cluster+1)])
kmeans_ESG_scores <- Generate_ESG_score_per_portfolio(kmeans_file_str_esg_score,cluster,
                                                      env_weight_list[(cluster+1)],soc_weight_list[(cluster+1)],gov_weight_list[(cluster+1)])

#================= section 5: Create ESG score dataframe with all strategies per column ==========

ESG_score_df <- data.frame(matrix(0,nrow=15,ncol=1))

ESG_score_df[,1] <- Obtain_ESG_score_per_horizon(simple_returnOnly_allocations[['Dynamic']],simple_ESG_scores)
colnames(ESG_score_df)[1] <- "Dynamic, simple sorting, return-only"
ESG_score_df[,2] <- Obtain_ESG_score_per_horizon(simple_returnOnly_allocations[['BuyHold']],simple_ESG_scores)
colnames(ESG_score_df)[2] <- "Buy&Hold, simple sorting, return-only"
ESG_score_df[,3] <- Obtain_ESG_score_per_horizon(simple_ESGRestricted_allocations[['Dynamic']],simple_ESG_scores)
colnames(ESG_score_df)[3] <- "Dynamic, simple sorting, ESG restricted"
ESG_score_df[,4] <- Obtain_ESG_score_per_horizon(simple_ESGRestricted_allocations[['BuyHold']],simple_ESG_scores)
colnames(ESG_score_df)[4] <- "Buy&Hold, simple sorting, ESG restricted"

ESG_score_df[,5] <- Obtain_ESG_score_per_horizon(kmeans_returnOnly_allocations[['Dynamic']],kmeans_ESG_scores)
colnames(ESG_score_df)[5] <- "Dynamic, K-means sorting, return-only"
ESG_score_df[,6] <- Obtain_ESG_score_per_horizon(kmeans_returnOnly_allocations[['BuyHold']],kmeans_ESG_scores)
colnames(ESG_score_df)[6] <- "Buy&Hold, K-means sorting, return-only"
ESG_score_df[,7] <- Obtain_ESG_score_per_horizon(kmeans_ESGRestricted_allocations[['Dynamic']],kmeans_ESG_scores)
colnames(ESG_score_df)[7] <- "Dynamic, K-means sorting, ESG restricted"
ESG_score_df[,8] <- Obtain_ESG_score_per_horizon(kmeans_ESGRestricted_allocations[['BuyHold']],kmeans_ESG_scores)
colnames(ESG_score_df)[8] <- "Buy&Hold, K-means sorting, ESG restricted"

write_xlsx(ESG_score_df,"C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/optimal ESG score df.xlsx")

#================= section 6: Create equity allocation dataframe with all strategies per column ==========

equity_allocation_df <- data.frame(matrix(0,nrow=15,ncol=1))

num_cols <- ncol(kmeans_returnOnly_allocations[['Dynamic']][[1]])


equity_allocation_df[,1] <- Obtain_equity_allocation_per_horizon(simple_returnOnly_allocations[['Dynamic']])
colnames(equity_allocation_df)[1] <- "Dynamic, simple sorting, return-only"
equity_allocation_df[,2] <- Obtain_equity_allocation_per_horizon(simple_returnOnly_allocations[['BuyHold']])
colnames(equity_allocation_df)[2] <- "Buy&Hold, simple sorting, return-only"
equity_allocation_df[,3] <- Obtain_equity_allocation_per_horizon(simple_ESGRestricted_allocations[['Dynamic']])
colnames(equity_allocation_df)[3] <- "Dynamic, simple sorting, ESG restricted"
equity_allocation_df[,4] <- Obtain_equity_allocation_per_horizon(simple_ESGRestricted_allocations[['BuyHold']])
colnames(equity_allocation_df)[4] <- "Buy&Hold, simple sorting, ESG restricted"

equity_allocation_df[,5] <- Obtain_equity_allocation_per_horizon(kmeans_returnOnly_allocations[['Dynamic']])
colnames(equity_allocation_df)[5] <- "Dynamic, K-means sorting, return-only"
equity_allocation_df[,6] <- Obtain_equity_allocation_per_horizon(kmeans_returnOnly_allocations[['Dynamic']])
colnames(equity_allocation_df)[6] <- "Buy&Hold, K-means sorting, return-only"
equity_allocation_df[,7] <- Obtain_equity_allocation_per_horizon(kmeans_ESGRestricted_allocations[['Dynamic']])
colnames(equity_allocation_df)[7] <- "Dynamic, K-means sorting, ESG restricted"
equity_allocation_df[,8] <- Obtain_equity_allocation_per_horizon(kmeans_ESGRestricted_allocations[['Dynamic']])
colnames(equity_allocation_df)[8] <- "Buy&Hold, K-means sorting, ESG restricted"

write_xlsx(equity_allocation_df,"C:/Users/nikit/OneDrive/Documents/EUR/Master QF/Master Thesis/new stuff/equity allocation df.xlsx")
