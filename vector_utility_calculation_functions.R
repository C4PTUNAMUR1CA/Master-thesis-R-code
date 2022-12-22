vector_utility_calculation <- function(allocations,return_list,gamma,
                                       period,retirement_age,period_list){
  #calculates the utility for all scenarios, for a power utility function.
  #INPUT:
  #allocations : a vector of allocations, with columns noted as Equities, short_bonds and long_bonds
  #Normalised_wealth : The simulated normalised wealth over all periods and scenarios
  #Income: the simulated income over all scenarios and periods
  #premium_fraction: The fraction of income that is invested
  #return_list is the list containing all the current-period returns of all variables, for all scenarios, in matrix column form
  #gamma is the relative risk aversion factor
  #period is the current period
  #retirement age is the last period
  #period_list is a list containing for every period, the optimal portfolio allocations and its corresponding utility over all scenarios
  
  rownames(allocations) <- NULL
  
  #if we are in the first period, then we dont require to collect utilities from other periods
  wealth <- matrix(0,nrow=nrow(return_list[[1]]),ncol=1)
  if (period==(retirement_age)){
    #calculate the power utility given the allocation, over all scenarios
    col_num <- 1
    for (var in names(return_list)){
      wealth <- wealth + allocations[1,col_num]*return_list[[var]][,period]
      col_num <- col_num + 1
    }
    gamma_adjusted_utility <- U(wealth,gamma)
  } else {
    col_num <- 1
    for (var in names(return_list)){
      wealth <- wealth + allocations[1,col_num]*return_list[[var]][,period]
      col_num <- col_num + 1
    }
    #Use power utility, except when gamma is 1, then refer to log utility
    if (gamma==1){
      wealth_gammaPower <- log(wealth)
    } else {
      wealth_gammaPower <- wealth^(1-gamma)
    }
    
    #Collects the accumulated wealth over all future periods, for each scenario
    accumulated_wealth <- wealth_gammaPower
    for (future_period in (period+1):(retirement_age)){
      future_period_chr <- paste('age_',as.character(future_period),sep='')
      accumulated_wealth <- accumulated_wealth * as.vector(period_list[[future_period]][,'utility'])
    }
    #Use power utility, except when gamma is 1, then refer to log utility
    if (gamma==1){
      gamma_adjusted_utility <- accumulated_wealth
    } else {
      gamma_adjusted_utility <- (1/(1-gamma))*accumulated_wealth
    }
  }
  #Returns a vector of utility, with size of the number of scenarios
  return(gamma_adjusted_utility)
}