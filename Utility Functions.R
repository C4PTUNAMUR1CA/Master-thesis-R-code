########################################################################### 
##  Start date: 2022-12-19 ------------------------------------------------
##  Author: Nikita Pavlov ---------------------------------------
##  Subject: Utility Functions --------------------------------------------
###########################################################################

# Functions ---------------------------------------------------------------

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function(header = FALSE, dec = ",") {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = header, dec = dec)
  close(f)
  return(df)
}

U = function(x, gamma) {
  r = 1 - gamma
  if (r == 0) {
    U = log(x)
  } else {
    U = (x^r)/r
  }
}

Uinv = function(x, gamma) {
  r = 1 - gamma
  if (r == 0) {
    Uinv = exp(x)
  } else {
    Uinv = (r*x)^(1/r)
  }
  
}

EU = function(x, gamma) {
  EU <- mean(U(x, gamma))
}

CE = function(x, gamma) {
  CE <-  Uinv(EU(x, gamma), gamma)
}

U_lambda = function(x, lambda){
  U = x
  U[x < 0] = lambda * x[x < 0]
  return(U)
}

Uinv_lambda = function(U, lambda){
  x = U
  x[U < 0] = U[U < 0] / lambda
  return(x)
}

CE_lambda = function(x, lambda) {
  Uinv_lambda(mean(U_lambda(x, lambda)), lambda)
}


CalculatesVasteDaling = function(Outcomematrix, daling){
  return(t( t(Outcomematrix) * ((1 + daling) ^ (duration - 1:NT)) ))
}

w_p1 = function(p, gamma_p = 1){
  # Probability weighting from T&K 1992
  return( p^gamma_p / (p^gamma_p + (1-p)^gamma_p)^(1/gamma_p)   )
}


w_p = function(p, gamma_p = 1){
  # Function according to Jorgo/Marike's specification
  return( p^gamma_p  )
}

RDU = function(x, gamma, gamma_p = 1){
  # Calculate the rank-dependent utility of a prospect x
  if (gamma_p == 1){
    return(mean(U(x, gamma)))
  } else {
    p = seq(0, 1, length.out = length(x) + 1)
    pi = diff(w_p(p, gamma_p))
    return(sum( U(sort(x, decreasing = T), gamma) * pi))
  }
}

CE_RDU = function(x, gamma, gamma_p = 1) {
  CE_RDU <-  Uinv(RDU(x, gamma, gamma_p), gamma)
}

GetsProbabilityWeights = function(outcomes, gamma_p = 1){
  p = seq(0, 1, length.out = length(outcomes) + 1)
  pi = diff(w_p(p, gamma_p))
  df = data.frame(outcomes, pi)
  return(pi[order(-outcomes)])
}

Calculates_Equivalent_Return = function(CE, vast_hoogte, Discount.factor, accuracy = 1000){
  # Initial estimate
  Rational_Return = CE / vast_hoogte - 1
  # Check what the CE would be using this estimate
  Rational_CE_check = sum( (1 + Rational_Return)^(1:NT) *  Discount.factor / sum(Discount.factor)) * vast_hoogte
  # Adjust the estimate slightly, up until the point where the rational 
  initial_sign = sign(CE - Rational_CE_check)
  while  (initial_sign == sign(CE - Rational_CE_check)){
    Rational_Return = Rational_Return * (1 - 1 / accuracy)
    Rational_CE_check = sum( (1 + Rational_Return)^(1:NT) *  Discount.factor / sum(Discount.factor)) * vast_hoogte
  }
  return(Rational_Return)
  
}
