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
  power_utility <- apply(wealth,1,U)
    U(wealth,gamma)
  
  return(power_utility)
}

U = function(x, gamma) {
  r = 1 - gamma
  if (r == 0) {
    U = log(x)
  } else {
    U = (x^r)/r
  }
}