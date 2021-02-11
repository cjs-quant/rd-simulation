
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
  
  # keep observations if within bandwidths
  data <- data[which(data$x > -1*bw_l & data$x < bw_r),]
  
  # kernel weights if triangular
  if (kernel == "triangular") {
    w = 0
    w[data$x < 0 & data$x > -1*bw_l] = 1 - abs(data$x[data$x < 0 & data$x > -1*bw_l]/bw_l)
    w[data$x >= 0 & data$x < bw_r] = 1 - abs(data$x[data$x >= 0 & data$x < bw_r]/bw_r)
    w = diag(w)
  }
  
  # kernel weights if uniform
  if (kernel == "uniform") {
    w = diag(1, nrow(data), nrow(data))
  }
  
  # dummy for over-cutoff (> 0)
  data$over_cutoff = ifelse(data$x > 0, 1, 0)
  
  # reates variables to be used in RD regression and cbinds together
  for (i in 1:order) {
    data[paste(colnames(data[1]),toString(i),sep ="_")] = data[1]^(i)
    data[paste(colnames(data[1]),"over_cutoff",toString(i),sep ="_")] = data[1]^(i) * data$over_cutoff
  }
  
  # define matrices for OLS
  y = data$y
  data$y = NULL
  data$x = NULL
  x = data
  x$constant = 1
  x = data.matrix(x)
  
  # run OLS
  beta = solve(t(x) %*% w %*% x) %*% t(x) %*% w %*% y
  
  # compute variance of each estimator
  r = y - x %*% beta
  sigma2_hat = (t(r) %*% r) / (nrow(x) - ncol(x))
  
  # compute standard errors
  vcov_beta_hat = c(sigma2_hat) * solve(t(x) %*% x)
  se = sqrt(diag(vcov_beta_hat))
  
  # compute t-statistic
  t = beta/se
  
  # compute p-values
  p = 2*pt(abs(t), nrow(x) - 1, lower=FALSE)
  p = p[1,1]
  
  # collect estimates
  estimate = cbind(discontinuity, beta)
  colnames(estimate) = c("Discontinuity", "Beta")
  
  # create output table
  bias = (discontinuity - beta[1])^2
  variance = sigma2_hat[1,1]
  imse = bias + variance
  out_table = cbind(discontinuity, beta[1], p, beta[nrow(beta)], bias, variance, imse)
  colnames(out_table) = c("$$\\text{True } \\beta$$", "$$\\text{Estimated } \\hat{\\beta}$$", 
                          "$$\\text{P-Value}$$", "$$\\text{Control Mean}$$", 
                          "$$Bias(\\hat{\\beta})^2$$", "$$Variance(\\hat{\\beta})$$", 
                          "$$\\text{IMSE}$$")
  
  # store coefficients for regression equation
  coeffs = list()
  coeffs[1] = paste(round(beta[nrow(beta)], 2))
  coeffs[2] = paste(ifelse(round(beta[1], 2) >= 0, "+", ""), round(beta[1], 2), " \\times \\mathbb{D}_{X > 0}")
  for (i in 1:order) {
    if (i == 1) {
      coeffs[2*i+1] = paste(ifelse(round(beta[2*i+1], 2) >= 0, "+", ""), round(beta[2*i+1], 2), " \\times X")
      coeffs[2*i+2] = paste(ifelse(round(beta[2*i+2], 2) >= 0, "+", ""), round(beta[2*i+2], 2), " \\times X \\times \\mathbb{D}_{X > 0}")
    }
    if (i > 1) {
      coeffs[2*i+1] = paste(ifelse(round(beta[2*i+1], 2) >= 0, "+", ""), round(beta[2*i+1], 2), " \\times X^", i)
      coeffs[2*i+2] = paste(ifelse(round(beta[2*i+2], 2) >= 0, "+", ""), round(beta[2*i+2], 2), " \\times X^", i, " \\times \\mathbb{D}_{X > 0}")
    }
  }
  
  # format regression equation
  equation = paste0(coeffs)
  
  # report estimates with underlying data
  results = list("estimate" = estimate, "data" = x, "equation" = equation, "out_table" = out_table)
  
  return(results)
  
}

