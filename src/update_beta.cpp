#include "RcppArmadillo.h"
using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]
//' update_beta
//' 
//' update beta with Iteratively Reweighted Least Squares (IRLS)
//' inside function, do not need to export
//' @param y outcome matrix
//' @param X design matrix
//' @param n number of sample
//' @param p number of predictors
//' @param tol integer for tolerance value
//' @param iter_max integer to maximum iteration number 
//' @return beta a vector of estimated coefficients
//' @export
// [[Rcpp::export]]
arma::vec update_beta(arma::mat y, arma::mat X, 
                      int n, int p, 
                      double tol, int iter_max){
  //initialization
  vec eta(n);
  vec mu(n);
  vec nu(n);
  vec Z(n);
  vec beta(p);
  vec beta_new(p);
      
  mat V(n, n);
  mat XV;
  
  double epsilon = 99;
  int iter = 0;
  
  // IRLS algorithm starts
  while( epsilon > tol & iter <= iter_max ) {
      eta = X * beta;
      mu = exp(eta) / (1 + exp(eta));
      nu = mu % (1 - mu);
      V = diagmat(nu); 
      Z = eta + inv(V) * (y - mu);
      XV = X.t() * V;
      beta_new = inv(XV * X) * XV * Z;
      epsilon = sqrt((beta_new-beta).t() * (beta_new-beta)).eval()(0,0);
      beta = beta_new;
      iter++;
  }
  
  return beta;
}

