
rd_regress = function(data, order, bw_l, bw_r, kernel, discontinuity) {
  # DESCRIPTION:
  # takes RD data and computes an RD regression with
  # provided order and bandwidths. returns betas, variances,
  # t-statistics, p-values, and the data used in the RD
  
  # ARGUMENTS:
  # data: the input data in the RD - RV in col 1, observations in col2
  # order: order of RD regression
  # bw_l: left-bandwidth
  # bw_r: right-bandwidth
  # kernel: triangular or uniform computation of kernel weights
  # discontinuity: selected discontinuity
  
  # coerce to numeric
  order = as.numeric(order)
  
  # new changes
  data = data[which(data$x > -1*bw_l & data$x < bw_r),]
  
  # run RD regression
  rd = rdrobust(data$y, data$x, p=as.numeric(order), q=as.numeric(order)+1, h=c(bw_l, bw_r), kernel=kernel)
  
  # collect estimates
  estimate = cbind(discontinuity, rd$beta_p_l, rd$beta_p_r)
  colnames(estimate) = c("Discontinuity", "Beta_l", "Beta_r")
  
  # recover bias, variance, and MSE
  bias = rd$bias[2] - rd$bias[1]
  variance = rd$V_cl_l[1,1] + rd$V_cl_r[1,1]
  MSE_l = bw_l^(2*(order+1))*bias^2 + (1/(rd$N[1]*bw_l))*variance
  MSE_r = bw_r^(2*(order+1))*bias^2 + (1/(rd$N[2]*bw_r))*variance
  
  # create out_table
  out_table = cbind(discontinuity, rd$coef[1], rd$pv[1], rd$tau_cl[1], bias, variance, MSE_l, MSE_r)
  colnames(out_table) = c("True Discontinuity", "Estimated Discontinuity", 
                          "P-Value", "Control Mean", "Bias", "Variance",
                          "Left MSE", "Right MSE")
  
  # store coefficients for left regression equation
  coeffs_l = list()
  coeffs_l[1] = paste(round(rd$beta_p_l[1], 2))
  for (i in 1:order) {
    if (i == 1) {
      coeffs_l[i+1] = paste(ifelse(round(rd$beta_p_l[i+1], 2) >= 0, "+", ""), round(rd$beta_p_l[i+1], 2), " \\times X")
    }
    if (i > 1) {
      coeffs_l[i+1] = paste(ifelse(round(rd$beta_p_l[i+1], 2) >= 0, "+", ""), round(rd$beta_p_l[i+1], 2), " \\times X^", i)
    }
  }
  
  # store coefficients for right regression equation
  coeffs_r = list()
  coeffs_r[1] = paste(round(rd$beta_p_r[1], 2))
  for (i in 1:order) {
    if (i == 1) {
      coeffs_r[i+1] = paste(ifelse(round(rd$beta_p_r[i+1], 2) >= 0, "+", ""), round(rd$beta_p_r[i+1], 2), " \\times X")
    }
    if (i > 1) {
      coeffs_r[i+1] = paste(ifelse(round(rd$beta_p_r[i+1], 2) >= 0, "+", ""), round(rd$beta_p_r[i+1], 2), " \\times X^", i)
    }
  }
  
  # format regression equations
  equation_l = paste0(coeffs_l) 
  equation_r = paste0(coeffs_r)
  
  results = list("estimate" = estimate, "data" = data$x, "equation_r" = equation_r, "equation_l" = equation_l, "out_table" = out_table)
  
  return(results)
  
}

