// sorting & ordering all the rows of a large table can be very slow in R

#include <Rcpp.h>
using namespace Rcpp;

//' Sort a 2d array.
//'
//' @param x A two dimensional numeric array.
//' @param byrow Set to true sort rows, false to sort columns.
//' @export
// [[Rcpp::export]]
NumericMatrix sort2d(NumericMatrix x, bool byrow=true) {
// sort a matrix by row
// this is MUCH faster than t(apply(x, 1, sort)) :-)
// make a copy: don't change the R variable itself by working on x
  NumericMatrix result(clone(x));
// data is ordered column-major! to sort rows take the transpose
  if (byrow) result = transpose(result);
  int i;
//  for (i=0; i<result.ncol() ; i++) quicksort( result.begin() + i*result.nrow(), result.nrow());
  for (i=0; i<result.ncol() ; i++) std::sort(result.begin()+i*result.nrow(),
                                             result.begin()+(i+1)*result.nrow());
  if (byrow) result = transpose(result);
  return result;
}



//' Compute the rank histogram for an EPS
//'
//' @param obs A vector of observations.
//' @param fc A two dimensional array of EPS data with members in columns.
//' @export
// [[Rcpp::export]]
NumericVector rankHistogram(NumericVector obs, NumericMatrix fc) {
// fc is a matrix with each column containing forecast of another ensemble member
// return value is a vector (ncol+1) giving the position of every obs
  int i, j, k, nmbr=fc.ncol(), nobs=obs.size();
  NumericVector result(nmbr+1, 0.0); // initial value must be explicitly a real
//  TODO : check if (fc.nrow() != nobs)
  for (i=0 ; i<nobs ; i++) {
    k=0;
    for (j=0 ; j < nmbr ; j++) k += (obs[i] > fc(i,j));
    result[k]++;
  }
  return result;
}

//' Compute the forecast probabilities for given thresholds
//'
//' @param fc A two dimensional array of EPS data with members in columns.
//' @param byrow The thresholds to compute probabilities for
//' @export
// [[Rcpp::export]]
NumericMatrix fcprob(NumericMatrix fc, NumericVector thresholds) {
  int i, j, k, nmbr=fc.ncol(), npoints=fc.nrow(), nthresh=thresholds.size();
  double mbrcount, mbrmean, dd;
  NumericMatrix result(npoints, nthresh + 3);

  for (i=0 ; i < npoints ; i++ ){
    for (j=0 ; j < nmbr ; j++){
      if (!NumericMatrix::is_na(fc(i,j))){
        for (k=0 ; k < nthresh ; k++) result(i,k) += (fc(i,j) > thresholds[k]);
        /* member count & mean : */
        result(i, nthresh) += 1 ;
        result(i, nthresh+1) += fc(i,j) ;
      }
    }
  }

  /* variance : */
  for (i=0 ; i < npoints ; i++){
    mbrcount = result(i, nthresh) ;
    /* probability = number_over_threshold / mbrcount */
    for (k=0 ; k < nthresh ; k++) result(i,k) /= mbrcount ;
    mbrmean = (result(i,nthresh+1) /= mbrcount);
    for (j=0 ; j < nmbr; j++){
      if (!NumericMatrix::is_na(fc(i,j))) {
        dd = fc(i,j) - mbrmean;
        result(i, nthresh+2) += dd*dd;
      }
    }
    result(i, nthresh+2) /= (mbrcount-1);
  }
  return result;
}



