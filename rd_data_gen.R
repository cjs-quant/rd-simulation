
rd_data_gen = function(discontinuity, order, variance, n) {
  # DESCRIPTION:
  # takes size of discontinuity, polynomial order, variance, and 
  # number of draws to generate data along the running variable (RV). 
  # I assume homoskedasticity in the data, and the RV ranges from -1 to 1
  
  # ARGUMENTS:
  # discontinuity: size of discontinuty for data generation, a real number between -1 and 1
  # order: order of polynomial to fit over RV, an integer between 1 and 4
  # variance: variance of data over RV, a real number between 0 and 2
  # n: number of random uniform draws over RV
  
  # bounds for RV
  rv_max = 1
  rv_min = -1
  
  # random uniform x-draws along RV
  x = runif(n, rv_min, rv_max)
  x = as.data.frame(x)
  
  # "true" data values for every x-draw
  if (order == 1) {y = x}
  if (order == 2) {y = x + x^2}
  if (order == 3) {y = x + x^2 + x^3}
  if (order == 4) {y = x + x^2 + x^3 + x^4}
  
  # fix colname for y-draws
  colnames(y) = "y"
  
  # applies noise
  noise = rnorm(n, 0, variance)
  y = y + noise
  
  # x-draws in col 1, y-draws in col 2
  draws = cbind(x, y)
  
  # applies discontinuity
  draws$y[draws$x >= 0] = draws$y[draws$x >= 0] + discontinuity
  
  return(draws)
  
}

