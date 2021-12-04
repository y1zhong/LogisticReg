#'lrm.rsqr
#'
#'get Cox & Snell and Nagelkerke R-square
#'
#' 
#'@param lrm_res a list generated from lrm()
#'
#'@return list, containing two R-square values
#'
#'@examples
#'set.seed(1)
#'y = sample(c(0, 1), 10, replace = TRUE)
#'X = round(matrix(rnorm(30), 10, 3), 3)
#'res = lrm(y, X)
#'lrm.rsqr(res)
#'
#'@export
#'
lrm.rsqr = function(lrm_res) {
  # fit intercept only logistic regression model
  y = lrm_res$dat$y
  lrm_intercept = lrm(y, rep(1, length(y)), intercept = F)
  
  # get the log likelihood for two models
  l0 = lrm_intercept$loglik
  l1 = lrm_res$loglik
  
  # Cox and Snell R-square
  R2_cox = 1 - (exp(l0 - l1)) ^ (2 / length(y))
  
  # adjust to get Nagelkerke's R-square
  R2_max = 1 - exp(l0) ^ (2/length(y))
  max_adj_R2 = R2_cox/R2_max
  
  return(list(Cox_Snell_rsqr = R2_cox,
              Nagelkerke_rsqr = max_adj_R2))
}

