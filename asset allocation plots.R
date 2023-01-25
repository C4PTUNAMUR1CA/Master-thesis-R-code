#load packages

library(ggplot2)
library(dplyr)

#=============== Section 1: load all asset allocations ========

#=============== Section 2: load graph functions ========

allocations_graphs <- function(allocations,horizons){
  
  colnames(allocations) <- c("T-bill", "T-note", "cor-Bond",
                             "cluster 1","cluster 2","cluster 3",
                             "cluster 4","cluster 5","cluster 6",
                             "cluster 7","cluster 8")
  
  allocations_graph <- cbind(horizons,stack(allocations[1:ncol(allocations)]))
  colnames(allocations_graph) = cbind('Horizon', 'Allocations', 'Asset')
  allocations_graph$Asset <- factor(allocations_graph$Asset , levels=c("T-bill", "T-note", "cor-Bond",
                                                                       "cluster 1","cluster 2","cluster 3",
                                                                       "cluster 4","cluster 5","cluster 6",
                                                                       "cluster 7","cluster 8"))
  
  # stacked area chart
  ggplot(allocations_graph, aes(x=Horizon, y=Allocations, fill=Asset)) + 
    geom_area(alpha=0.5 , size=.5)+
    theme_classic()+
    scale_x_continuous(breaks = c(3,6,9,12,15))+
    #scale_fill_brewer(palette="RdGy")
    scale_fill_manual(values=c("#922B21", "#633974", "#1A5276",
                                "#117864", "#9A7D0A", "#873600",
                                "#979A9A", "#515A5A", "#1C2833",
                                "#999999", "#196F3D"))
}

allocations_graphs_equitySummed <- function(allocations,horizons){
  
  colnames(allocations) <- c("T-bill","T-note", "cor-Bond",
                             "cluster 1","cluster 2","cluster 3",
                             "cluster 4","cluster 5","cluster 6",
                             "cluster 7","cluster 8")
  
  allocations[,'Equity'] <- apply(allocations[,c("cluster 1","cluster 2","cluster 3",
                                                 "cluster 4","cluster 5","cluster 6",
                                                 "cluster 7","cluster 8")],1,sum)
  allocations <- allocations[,c("T-bill", "T-note","cor-Bond","Equity")]
  allocations_graph <- cbind(horizons,stack(allocations[1:ncol(allocations)]))
  colnames(allocations_graph) = cbind('Horizon', 'Allocations', 'Asset')
  allocations_graph$Asset <- factor(allocations_graph$Asset , levels=c("T-bill", "T-note", "cor-Bond",
                                                                       "Equity"))
  
  # stacked area chart
  ggplot(allocations_graph, aes(x=Horizon, y=Allocations, fill=Asset)) + 
    geom_area(alpha=0.5 , size=.5)+
    theme_classic()+
    scale_x_continuous(breaks = c(3,6,9,12,15))+
    #scale_fill_brewer(palette="RdGy")
    scale_fill_manual(values=c("#922B21", "#633974", "#1A5276",
                               "#117864"))
}

assets <- c( "Tbill_return","Tnote_return","corBond_return",
             "cluster_return_1","cluster_return_2","cluster_return_3",
             "cluster_return_4","cluster_return_5","cluster_return_6",
             "cluster_return_7","cluster_return_8")

obtain_final_period_allocation_per_horizon <- function(allocations_list,inv_strategy){
  
  num_assets <- ncol(allocations_list[[inv_strategy]][[1]])
  
  final_allocation_df <- as.data.frame(matrix(0,nrow=15,ncol=num_assets))
  
  allocations_strat <- allocations_list[[inv_strategy]]
  for (horizon in 1:15){
    num_rows <- nrow(allocations_strat[[horizon]])
    final_allocation_df[horizon,] <- allocations_strat[[horizon]][1,]
  }
  
  return(final_allocation_df)
}

#=============== Section 3: 1 over N allocation, with equal weight across asset classes ========

bond_allocations <- as.data.frame(matrix(round(1/4,2),nrow=15,ncol=3))
equity_allocations <- as.data.frame(matrix(round(0.25/8,2),nrow=15,ncol=8))

oneOverN_allocation_equityFair <- cbind(bond_allocations,equity_allocations)
colnames(oneOverN_allocation_equityFair) <- assets

horizons=1:15
allocations_graphs(oneOverN_allocation_equityFair,horizons)

horizons=1:15
allocations_graphs_equitySummed(oneOverN_allocation_equityFair,horizons)

#=============== Section 4: 1 over N allocation, with equal weight across all assets ========

oneOverN_allocation <- as.data.frame(matrix(round(1/length(assets),2),nrow=15,ncol=11))
colnames(oneOverN_allocation) <- assets

horizons=1:15
allocations_graphs(oneOverN_allocation,horizons)

horizons=1:15
allocations_graphs_equitySummed(oneOverN_allocation,horizons)

#=============== Section 4: Optimal asset allocations for return-only and simple sorting ==========================================

load('simple_returnOnly_optimal_allocations_v2.RData')
optimal_allocations_simple_returnOnly <- optimal_allocations
optimal_allocations_simple_returnOnly_dynamic <- obtain_final_period_allocation_per_horizon(optimal_allocations_simple_returnOnly,'Dynamic')
optimal_allocations_simple_returnOnly_buyHold <- obtain_final_period_allocation_per_horizon(optimal_allocations_simple_returnOnly,'BuyHold')

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_returnOnly_dynamic,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_returnOnly_dynamic,horizons)

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_returnOnly_buyHold,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_returnOnly_buyHold,horizons)

#=============== Section 5: Optimal asset allocations with ESG restriction and simple sorting ==========================================

load('simple_ESGRestricted_optimal_allocations_v2.RData')
optimal_allocations_simple_ESGRestricted <- optimal_allocations
optimal_allocations_simple_ESGRestricted_dynamic <- obtain_final_period_allocation_per_horizon(optimal_allocations_simple_ESGRestricted,'Dynamic')
optimal_allocations_simple_ESGRestricted_buyHold <- obtain_final_period_allocation_per_horizon(optimal_allocations_simple_ESGRestricted,'BuyHold')

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_ESGRestricted_dynamic,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_ESGRestricted_dynamic,horizons)

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_ESGRestricted_buyHold,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_ESGRestricted_buyHold,horizons)

#=============== Section 6: Optimal asset allocations with ESG restriction and kmeans sorting ==========================================

load('kmeans_ESGRestricted_optimal_allocations_v2.RData')
optimal_allocations_kmeans_ESGRestricted <- optimal_allocations
optimal_allocations_kmeans_ESGRestricted_dynamic <- obtain_final_period_allocation_per_horizon(optimal_allocations_kmeans_ESGRestricted,'Dynamic')
optimal_allocations_kmeans_ESGRestricted_buyHold <- obtain_final_period_allocation_per_horizon(optimal_allocations_kmeans_ESGRestricted,'BuyHold')

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_kmeans_ESGRestricted_dynamic,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_kmeans_ESGRestricted_dynamic,horizons)

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_kmeans_ESGRestricted_buyHold,horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_kmeans_ESGRestricted_buyHold,horizons)

#=============== Section 6: Optimal asset allocations with return only and kmeans sorting ==========================================

