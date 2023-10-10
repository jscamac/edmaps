// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// pointInPoly
bool pointInPoly(const arma::rowvec& point, const arma::mat& vertices);
RcppExport SEXP _edmaps_pointInPoly(SEXP pointSEXP, SEXP verticesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::rowvec& >::type point(pointSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type vertices(verticesSEXP);
    rcpp_result_gen = Rcpp::wrap(pointInPoly(point, vertices));
    return rcpp_result_gen;
END_RCPP
}
// pointsInPoly
std::vector<bool> pointsInPoly(const arma::mat& points, const arma::mat& vertices);
RcppExport SEXP _edmaps_pointsInPoly(SEXP pointsSEXP, SEXP verticesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type points(pointsSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type vertices(verticesSEXP);
    rcpp_result_gen = Rcpp::wrap(pointsInPoly(points, vertices));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_edmaps_pointInPoly", (DL_FUNC) &_edmaps_pointInPoly, 2},
    {"_edmaps_pointsInPoly", (DL_FUNC) &_edmaps_pointsInPoly, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_edmaps(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
