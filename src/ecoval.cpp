// Economic value score

#include <Rcpp.h>
using namespace Rcpp;

//' Compute economic value score
//'
//' @param obs A vector observations (value 0,1)
//' @param pred A vector of probabilities [0,1].
//' @param costloss A vector of cost/loss ratios
//' @param thresholds A vector of threshold probabilities
//' @value a list of cl, value, Vmax, Venv, H, F, s, n
// [[Rcpp::export]]
List ecoval(NumericVector obs, NumericVector pred,
    NumericVector costloss,
    NumericVector thresholds ) {
    // TODO: make robust (check for NA, zero length vectors...)
    //       check that c_l and thresholds are ordered

    int ncl = costloss.length(), nthresh = thresholds.length();
    int ncases = obs.length();
    int i, nevents, cl, th;
    int a, b;
    double s;
//    if (c_l.length() == 0) c_l = seq_len(19) / 20. ;
//    if (thresholds.length() == 0) thresholds = seq_len(19) / 20. ;
    NumericVector c_l_s(ncl + 1); // reserve 1 extra

    // calculate base rate s
    // and add it to the list of cost/loss ratios
    nevents = 0;
    if (ncases > 0) {
      for (i = 0; i < ncases ; i++) if (obs[i]) nevents++;
      s = ((double) nevents) / ncases ;
      // check for s in costloss
      for (i=0 ; i < ncl && costloss[i] <  s ; i++) {
        c_l_s[i] = costloss[i] ;
      }

      if (i == ncl) {
        c_l_s[ncl] = s;
        ncl++;
      } else if (costloss[i] > s) {
        c_l_s[i++] = s;
        ncl++ ;
        for ( ; i < ncl ; i++) c_l_s[i] = costloss[i-1] ;
      } else {
        // s happens to be exactly in the c/l list already
        for ( ; i < ncl ; i++ ) c_l_s[i] = costloss[i] ;
      }
    }
    NumericVector Venv(ncl);
    NumericVector H(nthresh), F(nthresh);
    NumericMatrix value(ncl, nthresh);

    if (ncases == 0 || nevents == 0 || nevents == ncases || pred.length() != ncases) {
    //  Rcout << "obs is a constant vector (all true or false)" << std::endl
    //  Rcout << "lengths of obs and pred are different." << std::endl;

      for (th = 0 ; th < nthresh ; th++) {
        H[th] = NA_REAL;
        F[th] = NA_REAL;
        for (cl = 0 ; cl < ncl ; cl++) value(cl, th) = NA_REAL ;
      }
      for (cl = 0 ; cl < ncl ; cl++) Venv[cl] = NA_REAL ;
    }

    else {
      for (cl = 0 ; cl < ncl ; cl++) Venv[cl] = R_NegInf ;

      for (th = 0 ; th < nthresh ; th++) {
        // make (partial) contingency table for a given probability threshold
        a = b = 0. ;
        // In fact we don't even need c and d
        // c = d = 0;

        // TODO: with a lot of threshold values, this double loop becomes inefficient!
        // ---> use ordered list of predictions
        for (i = 0 ; i < ncases ; i++) {
          if (pred[i] > thresholds[th]) {
            if (obs[i]) a++ ;
            else b++;
          }
        }

        H[th] = ((double) a) / nevents;
        F[th] = ((double) b) / (ncases - nevents);

        // check for F=0, c_l=0,1
        for (cl = 0 ; cl < ncl ; cl++) {
          if (c_l_s[cl] < s) {
            value(cl, th) = (1. - F[th]) - s/(1. - s) * (1. - c_l_s[cl])/c_l_s[cl] * (1. - H[th]);
          }
          else {
            value(cl, th) = H[th] - (1. - s)/s * c_l_s[cl]/(1. - c_l_s[cl]) * F[th];
          }
//        if (value[cl, th] < 0.) value[cl, th] = 0.;
          // store "envelope" (largest value for a given c/l ratio)
          if (value(cl, th) > Venv[cl])  Venv[cl] = value(cl, th);
        }
      }
    }
    // max. value (at c/l = s)
    NumericVector Vmax = H - F ;
    // return
    return List::create(Rcpp::Named("cl") = c_l_s[seq_len(ncl)-1],
                      Rcpp::Named("value") = value,
                      Rcpp::Named("vmax") = Vmax,
                      Rcpp::Named("value_env") = Venv,
                      Rcpp::Named("H") = H,
                      Rcpp::Named("F") = F,
                      Rcpp::Named("s") = s,
                      Rcpp::Named("n") = ncases);
}
