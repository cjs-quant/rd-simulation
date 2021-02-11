
rd_clean = function(draws, results, order, bw_l, bw_r) {
  # DESCRIPTION:
  # takes RD data and estimation results from rd_regress
  # and creates a data.frame for plotting results in ggplot2
  # and reporting in table form
  
  # ARGUMENTS:
  # draws: simulated data
  # results: list containing estimation results and underlying data
  # order: order of RD regression
  # bw_l: left-bandwidth
  # bw_r: right-bandwidth
  
  # plot RD estimate
  draws = draws[which(draws$x > -1*bw_l & draws$x < bw_r),]
  
  # compute predicted y-values
  y_hat = results[["data"]] %*% results[["estimate"]][,2]
  
  # append predicted y-values to draws
  temp = cbind(draws, y_hat)
  temp$y_hat_above = NA
  temp$y_hat_below = NA
  temp$y_hat_above[which(temp$x >= 0)] = temp$y_hat[which(temp$x >= 0)]
  temp$y_hat_below[which(temp$x < 0)] = temp$y_hat[which(temp$x < 0)]
  
  return(temp)
  
}

