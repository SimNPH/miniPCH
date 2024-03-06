#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cdfFunCpp_multistate (const NumericVector& Tint, const arma::cube& Q, const arma::vec& pi, const arma::vec& abs, const NumericVector& v){
  const int n = v.size();
  const int m = Tint.size();
  NumericVector result(n);
  arma::mat P;
  int i, j;

  for(i=0; i < n; i++){
    P = arma::eye(Q.n_rows, Q.n_cols);
    for(j=1; j < m; j++){
      P = P * arma::expmat(std::max(std::min(Tint[j], v[i]) - Tint[j-1], 0.) * Q.slice(j-1));
    }
    P = P * arma::expmat(std::max(v[i] - Tint[m-1], 0.) * Q.slice(m-1));
    result[i] = (pi.t() * P * abs).eval()(0,0);
  }

  return result;
}

// [[Rcpp::export]]
NumericVector survFunCpp_multistate(const NumericVector& Tint, const arma::cube& Q, const arma::vec& pi, const arma::vec& abs, const NumericVector& v){
  const int n = v.size();
  const int m = Tint.size();
  NumericVector result(n);
  arma::mat P;
  int i, j;

  for(i=0; i < n; i++){
    P = arma::eye(Q.n_rows, Q.n_cols);
    for(j=1; j < m; j++){
      P = P * arma::expmat(std::max(std::min(Tint[j], v[i]) - Tint[j-1], 0.) * Q.slice(j-1));
    }
    P = P * arma::expmat(std::max(v[i] - Tint[m-1], 0.) * Q.slice(m-1));
    result[i] = 1. - (pi.t() * P * abs).eval()(0,0);
  }

  return result;
}

// [[Rcpp::export]]
NumericVector hazFunCpp_multistate(const NumericVector& Tint, const arma::cube& Q, const arma::vec& pi, const arma::vec& abs, const NumericVector& v){
  const int n = v.size();
  const int m = Tint.size();
  NumericVector result(n);
  arma::mat P;
  int i, j;
  double pdf, surv;

  for(i=0; i < n; i++){
    P = arma::eye(Q.n_rows, Q.n_cols);
    for(j=1; j < m; j++){
      if(v[i] < Tint[j]){
        break;
      }
      P = P * arma::expmat(std::max(std::min(Tint[j], v[i]) - Tint[j-1], 0.) * Q.slice(j-1));
    }
    P = P * arma::expmat((v[i] - Tint[j-1]) * Q.slice(j-1));
    surv = 1. - (pi.t() * P * abs).eval()(0,0);
    pdf = (pi.t() * P * Q.slice(j-1) * abs).eval()(0,0);
    result[i] = pdf/surv;
  }

  return result;
}

// [[Rcpp::export]]
NumericVector cumhazFunCpp_multistate(const NumericVector& Tint, const arma::cube& Q, const arma::vec& pi, const arma::vec& abs, const NumericVector& v){
  const int n = v.size();
  const int m = Tint.size();
  NumericVector result(n);
  arma::mat P;
  int i, j;

  for(i=0; i < n; i++){
    P = arma::eye(Q.n_rows, Q.n_cols);
    for(j=1; j < m; j++){
      P = P * arma::expmat(std::max(std::min(Tint[j], v[i]) - Tint[j-1], 0.) * Q.slice(j-1));
    }
    P = P * arma::expmat(std::max(v[i] - Tint[m-1], 0.) * Q.slice(m-1));
    result[i] = -log(1. - (pi.t() * P * abs).eval()(0,0));
  }

  return result;
}

// [[Rcpp::export]]
NumericVector pdfFunCpp_multistate(const NumericVector& Tint, const arma::cube& Q, const arma::vec& pi, const arma::vec& abs, const NumericVector& v){
  const int n = v.size();
  const int m = Tint.size();
  NumericVector result(n);
  arma::mat P;
  int i, j;

  for(i=0; i < n; i++){
    P = arma::eye(Q.n_rows, Q.n_cols);
    for(j=1; j < m; j++){
      if(v[i] < Tint[j]){
        break;
      }
      P = P * arma::expmat(std::max(std::min(Tint[j], v[i]) - Tint[j-1], 0.) * Q.slice(j-1));
    }
    P = P * arma::expmat((v[i] - Tint[j-1]) * Q.slice(j-1));
    result[i] = (pi.t() * P * Q.slice(j-1) * abs).eval()(0,0);
  }

  return result;
}
