#load packages

library(ggplot2)
library(dplyr)

#=============== Section 1: load all asset allocations ========

#=============== Section 2: load graph functions ========

allocations_graphs <- function(allocations,horizons){
  
  colnames(allocations) <- c("cor-Bond", "T-note", "T-bill",
                             "cluster 1","cluster 2","cluster 3",
                             "cluster 4","cluster 5","cluster 6",
                             "cluster 7","cluster 8")
  
  allocations_graph <- cbind(horizons,stack(allocations[1:ncol(allocations)]))
  colnames(allocations_graph) = cbind('Horizon', 'Allocations', 'Asset')
  allocations_graph$Asset <- factor(allocations_graph$Asset , levels=c("cor-Bond", "T-note", "T-bill",
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
  
  colnames(allocations) <- c("cor-Bond", "T-note", "T-bill",
                             "cluster 1","cluster 2","cluster 3",
                             "cluster 4","cluster 5","cluster 6",
                             "cluster 7","cluster 8")
  
  allocations[,'Equity'] <- apply(allocations[,c("cluster 1","cluster 2","cluster 3",
                                                 "cluster 4","cluster 5","cluster 6",
                                                 "cluster 7","cluster 8")],1,sum)
  allocations <- allocations[,c("cor-Bond", "T-note", "T-bill","Equity")]
  allocations_graph <- cbind(horizons,stack(allocations[1:ncol(allocations)]))
  colnames(allocations_graph) = cbind('Horizon', 'Allocations', 'Asset')
  allocations_graph$Asset <- factor(allocations_graph$Asset , levels=c("cor-Bond", "T-note", "T-bill",
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

load('simple_returnOnly_optimal_allocations_final.RData')

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_returnOnly[['Dynamic']][[15]],horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_returnOnly[['Dynamic']][[15]],horizons)

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_returnOnly[['BuyHold']][[15]],horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_returnOnly[['BuyHold']][[15]],horizons)

#=============== Section 5: Optimal asset allocations with ESG restriction and simple sorting ==========================================

load('simple_ESGRestricted_optimal_allocations.RData')

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_ESGRestricted[['Dynamic']][[15]],horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_ESGRestricted[['Dynamic']][[15]],horizons)

#plots for the Dynamic asset allocation
horizons=1:15
allocations_graphs(optimal_allocations_simple_ESGRestricted[['BuyHold']][[15]],horizons)

#plots for the Dynamic asset allocation with equity summed
horizons=1:15
allocations_graphs_equitySummed(optimal_allocations_simple_ESGRestricted[['BuyHold']][[15]],horizons)
