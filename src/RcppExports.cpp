// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// death_table
Rcpp::NumericMatrix death_table(Rcpp::DataFrame pop, Rcpp::NumericVector ages, Rcpp::NumericVector period);
RcppExport SEXP _IBMPopSim_death_table(SEXP popSEXP, SEXP agesSEXP, SEXP periodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type pop(popSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ages(agesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type period(periodSEXP);
    rcpp_result_gen = Rcpp::wrap(death_table(pop, ages, period));
    return rcpp_result_gen;
END_RCPP
}
// exposure_table
Rcpp::NumericMatrix exposure_table(Rcpp::DataFrame pop, Rcpp::NumericVector ages, Rcpp::NumericVector period);
RcppExport SEXP _IBMPopSim_exposure_table(SEXP popSEXP, SEXP agesSEXP, SEXP periodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type pop(popSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type ages(agesSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type period(periodSEXP);
    rcpp_result_gen = Rcpp::wrap(exposure_table(pop, ages, period));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_IBMPopSim_death_table", (DL_FUNC) &_IBMPopSim_death_table, 3},
    {"_IBMPopSim_exposure_table", (DL_FUNC) &_IBMPopSim_exposure_table, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_IBMPopSim(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
