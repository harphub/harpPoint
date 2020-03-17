// ROC curve

#include <Rcpp.h>
using namespace Rcpp;

//' Compute ROC and area under ROC
//'
//' @param obs A vector observations (value 0,1)
//' @param pred A vector of probabilities [0,1].
//' @param thresholds A vector of threshold probabilities
//' @value a list with area and vectors thresholds, H, F
// [[Rcpp::export]]
List roc(NumericVector obs, NumericVector pred,
    NumericVector thresholds ) {

    int ncases = obs.length();
    int i, th, nevents;
    double a, b, s;
    int nthresh = thresholds.length();
    NumericVector H(nthresh), F(nthresh), area(1);

    if (pred.length() != ncases) {
      return NA_INTEGER;
    }
    // Make sure the thresholds are sorted ?
    // Then you must clone the vector: don't modify the original!
    // thresholds.sort(thresholds.begin(), thresholds.end()) ;
//    if (thresholds[0] >= 0) {
      // add "-.01" to the threshold, because in this routine we use pred > threshold
      // so theshold "0" won't actually trigger all predictions to be "true"
      // conversely, with ">=" we would need a "1.01" threshold to avoid triggering all pred's
      // THIS IS BETTER DONE IN AN R WRAPPER
//    }

    // calculate number of "events" (obs==1)
    nevents = 0;
    for (i = 0; i < ncases ; i++) if (obs[i]) nevents++;
    if (nevents == 0 || nevents == ncases) { // this also captures ncases<=1
      // the "event" is always or never -> ROC will fail
      // return H=F=area=NA
      area[0] = NA_REAL ;
      for (th = 0; th < nthresh ; th++) F[th] = H[th] = NA_REAL ;
      return List::create(Rcpp::Named("thresholds") = thresholds,
                      Rcpp::Named("H") = H,
                      Rcpp::Named("F") = F,
                      Rcpp::Named("area") = area);
    }

    for (th = 0 ; th < nthresh ; th++) {
      //TODO: this double loop is rather inefficient if there are a lot of thresholds
      // only consider pred's that change for the given threshold and move a <-> b
      // this requires an ORDERED list of predictions (and thresholds)
      // so we could use the same rank vector as used in part 2 of this function (area)

      // make contingency table for a given probability threshold
      // we only need a, b
      a = b = 0;
      // NOTE: I prefer >= , not > . That is different from the verification package.
      //       but much more standard, I think.
      for (i = 0 ; i < ncases ; i++) {
        if (pred[i] > thresholds[th]) {
          if (obs[i]) a++ ;
          else b++;
        }
      }
      H[th] = a / nevents;
      F[th] = b / (ncases - nevents);
    }
    // For the best estimate of the area, we should use all possible thresholds
    // We use exactly the same formula as the "verification" package:
    // area = (mean(rank(pred)[obs]) - (n1+1)/2)/(n - n1)
    // so we need to get the same "rank" function
    // and make sure the rank for ties (in pred) is set to the average

    // This could be a separate function my_rank()
    NumericVector ranked(ncases) ;
    IntegerVector index = seq_len(ncases) - 1 ;
    // 1. sorting the index
    std::sort(index.begin(), index.end(), [&](int ix, int iy) { return pred[ix] < pred[iy]; } ) ;

    int j, rcount ;
    double mean_rank, pp;
    // 2. second pass: create rank vector
    i = 0;
    while (i < ncases) {
      j = i;
      mean_rank = 0.;
      pp = pred[index[i]];
      while (j < ncases && pred[index[j]] == pp ) { mean_rank += j++; }
      rcount = j - i ;

      for ( ; i < j ; i++) ranked[index[i]] = mean_rank / rcount + 1;
    }

    // according to Rcpp documentation "mean(ranked[obs==1])" should work
    // but for some reason it doesn't
    mean_rank = 0;
    for (i = 0 ; i< ncases ; i++) if (obs[i]) mean_rank += ranked[i];

    area[0] = (mean_rank/nevents - (nevents + 1.)/2.) / (ncases - nevents);


  // return a list
  return List::create(Rcpp::Named("thresholds") = thresholds,
                      Rcpp::Named("H") = H,
                      Rcpp::Named("F") = F,
                      Rcpp::Named("ranks") = ranked, // only for testing
                      Rcpp::Named("area") = area);
}


