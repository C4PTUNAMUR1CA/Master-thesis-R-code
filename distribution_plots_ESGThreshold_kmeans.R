#load packages
#install.packages("fanplot")
#install.packages("ggfan")
install.packages("ggridges")
install.packages("hrbrthemes")
library(ggplot2)
library(magrittr)
library(tidyr)
library(dplyr)
library(writexl)
library(ggridges)
library(hrbrthemes)

#============== Section 1: load necessary functions ===============================

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()
df <- iris


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

load('kmeans_ESGRestricted_optimal_allocations_vfinal_35.RData')
optimal_allocations_kmeans_35 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_45.RData')
optimal_allocations_kmeans_45 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_55.RData')
optimal_allocations_kmeans_55 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_65.RData')
optimal_allocations_kmeans_65 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_75.RData')
optimal_allocations_kmeans_75 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_85.RData')
optimal_allocations_kmeans_85 <- optimal_allocations[['Dynamic']]

load('kmeans_ESGRestricted_optimal_allocations_vfinal_95.RData')
optimal_allocations_kmeans_95 <- optimal_allocations[['Dynamic']]

load("return_var_test_list_kmeans.RData")
return_test_set_kmeans <- return_var_test_list

source("Utility Functions.R")

#=============== section 3: obtain terminal wealth for horizon 15 ====================

terminal_wealth_35 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_35[[15]],return_test_set_kmeans,15)
terminal_wealth_45 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_45[[15]],return_test_set_kmeans,15)
terminal_wealth_55 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_55[[15]],return_test_set_kmeans,15)
terminal_wealth_65 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_65[[15]],return_test_set_kmeans,15)
terminal_wealth_75 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_75[[15]],return_test_set_kmeans,15)
terminal_wealth_85 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_85[[15]],return_test_set_kmeans,15)
terminal_wealth_95 <- get_terminal_wealth_perScenario(optimal_allocations_kmeans_95[[15]],return_test_set_kmeans,15)

df_35 <- as.data.frame(terminal_wealth_35)
df_35[,2] <- 35
df_45 <- as.data.frame(terminal_wealth_45)
df_45[,2] <- 45
df_55 <- as.data.frame(terminal_wealth_55)
df_55[,2] <- 55
df_65 <- as.data.frame(terminal_wealth_65)
df_65[,2] <- 65
df_75 <- as.data.frame(terminal_wealth_75)
df_75[,2] <- 75
df_85 <- as.data.frame(terminal_wealth_85)
df_85[,2] <- 85
df_95 <- as.data.frame(terminal_wealth_95)
df_95[,2] <- 95
colnames(df_35) <- c(1,2)
colnames(df_45) <- c(1,2)
colnames(df_55) <- c(1,2)
colnames(df_65) <- c(1,2)
colnames(df_75) <- c(1,2)
colnames(df_85) <- c(1,2)
colnames(df_95) <- c(1,2)

df_total <- rbind(df_35,df_45,df_55,df_65,df_75,df_85,df_95)
colnames(df_total) <- c('terminal_wealth','ESG_threshold_score')
df_total[,2] <- as.character(df_total[,2])

#plot the histograms
ggplot(df_total, aes(x = terminal_wealth, y = ESG_threshold_score)) + 
  geom_density_ridges() +
  labs(y= "ESG threshold score", x = "Terminal wealth's density")


