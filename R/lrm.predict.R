#'lrm_predict
#'
#'perform predictions from new data
#'
#' 
#'@param lrm_res a list generated from lrm()
#'@param Xnew design matrix with new data
#'
#'@return dataframe, contains probability and predicted value of outcomes
#'
#'@examples
#'set.seed(1)
#' y = sample(c(0, 1), 10, replace = TRUE)
#' X = round(matrix(rnorm(30), 10, 3), 3)
#' res = lrm(y, X)
#'lrm.predict(Xnew=matrix(rnorm(15), 5, 3), lrm_res=res)
#'
#'@export
#'
lrm.predict = function(Xnew, lrm_res){
  # process new data
  if (lrm_res$intercept) Xnew = cbind(inter = 1, Xnew)
  if (ncol(Xnew) != ncol(lrm_res$dat$X)) stop ("predictors dimension not match")
  
  #predict probability by estimated coefficients
  eta = Xnew %*% lrm_res$coef
  prob = exp(eta) / (1 + exp(eta))
  pred = ifelse(prob > 0.5, 1, 0)
  
  #save both probability and predicted outcomes
  res = data.frame(prob = prob,
                    pred = pred)
  return(res)
}
