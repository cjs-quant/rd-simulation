
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
  temp = draws[which(draws$x > -1*bw_l & draws$x < bw_r),]
  
  # build RD data
  for (i in 1:order) {
    temp[paste(colnames(temp[1]),toString(i),sep ="_")] = temp[1]^(i)
  }
  
  # compute predicted y-values
  temp$y_hat_below = data.matrix(cbind(1, temp[,3:ncol(temp)])) %*% results[["estimate"]][,2]
  temp$y_hat_above = data.matrix(cbind(1, temp[,3:(ncol(temp)-1)])) %*% results[["estimate"]][,3]
  temp$y_hat_below[temp$x > 0] = NA
  temp$y_hat_above[temp$x < 0] = NA
  
  return(temp)
  
}

