#'lrm
#'
#'Logistic regression on binary outcome
#'
#' 
#'@param y vector with two different values
#'@param X design matrix, with each row of samples and each column of covariates
#'@param intercept logical value, default is TRUE,
#' to include intercept in regression model
#'@param tol numeric value, default is 0.00001,
#'tolerance in iteratively reweighted least squares(IWLS) algorithm, 
#'@param iter_max integer, maximum iteration in IWLS, default is 50
#'
#'@return list, containing the data and regression results
#'
#'@examples
#'set.seed(1)
#'y = sample(c(0, 1), 10, replace = TRUE)
#'X = round(matrix(rnorm(30), 10, 3), 3)
#'res = lrm(y, X)
#'
#'@export
#'
lrm = function(y, X, 
               intercept = T,
               tol = 1e-8, iter_max = 25){
  ### check input data
  # check if y is two-level outcome
  if (length(unique(y)) != 2) stop("y variable does not have 2 levels")
  
  # convert y to numeric 0/1 matrix with one column
  if (is.factor(y)) y = as.numeric(y)
  y = y - min(y)
  y = as.matrix(y)
  
  # prepare X ready in regression model
  if (intercept) X = cbind(inter = 1, X)
  X = as.matrix(X)
  X = apply(X, 2, as.numeric)
  
  
  #check if y and X have same number of samples
  if(length(y) != nrow(X)) stop("sample number is y and X not match")
  
  #check if X is rank deficient
  tryCatch(solve(crossprod(X)),
           error = function(ex) stop("rank deficiency in X"))
  
  ### model fitting
  # get number of samples and predictors in design matrix
  n = nrow(X)
  p = ncol(X)
  
  # save data and settings in the return list
  res = list(dat = list(y = y,
                        X = X
                        ),
             intercept = intercept)
  
  # iteratively reweighted least squares algorithm
  beta = rep(0, p)
  iter = 0
  epsilon = 99
  while (epsilon > tol & iter <= iter_max){
    eta = X %*% beta
    mu = exp(eta) / (1 + exp(eta))
    nu = mu * (1 - mu)
    V = diag(x=as.vector(nu))
    Z = eta + solve(V) %*% (y - mu)
    XV = crossprod(X, V)
    beta_new = solve(XV %*% X) %*% XV %*% Z
    epsilon = sqrt(crossprod((beta_new - beta), (beta_new - beta)))
    beta = beta_new
    iter = iter + 1
  }
  res$coef = beta
  
  # save probability and fitted y with estimated coefficients
  eta = X %*% beta
  demoni = 1 + exp(eta)
  res$prob = exp(eta) / demoni
  res$fitted = ifelse(res$prob  > 0.5, 1, 0)
  
  ### check model fitting
  # save log likelihood
  res$loglik = sum(y * eta  - log(demoni))
  res$deviance = -2 * res$loglik
  res$aic = 2 * p + res$deviance

  return(res)
}