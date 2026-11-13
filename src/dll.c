
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP two_opt(SEXP R_matrix, SEXP R_t);
extern SEXP two_opt_sym(SEXP R_matrix, SEXP R_t);
extern SEXP or_opt(SEXP R_matrix, SEXP R_t, SEXP R_max_len, SEXP R_sym);
extern SEXP three_opt(SEXP R_matrix, SEXP R_t, SEXP R_sym);
extern SEXP insertion_cost(SEXP R_matrix, SEXP R_order, SEXP R_k);
extern SEXP tour_length_dist(SEXP R_dist, SEXP R_order);
extern SEXP tour_length_matrix(SEXP R_matrix, SEXP R_order);

void R_init_TSP(DllInfo *dll) {

  const R_CallMethodDef CallEntries[] = {
    {"R_two_opt",             (DL_FUNC) two_opt,            2},
    {"R_two_opt_sym",		      (DL_FUNC) two_opt_sym,		    2},
    {"R_or_opt",              (DL_FUNC) or_opt,             4},
    {"R_three_opt",           (DL_FUNC) three_opt,          3},
    {"R_insertion_cost",		  (DL_FUNC) insertion_cost,		  3},
    {"R_tour_length_dist",    (DL_FUNC) tour_length_dist,   2},
    {"R_tour_length_matrix",  (DL_FUNC) tour_length_matrix, 2},
    {NULL, NULL, 0}
  };

  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

  R_RegisterCCallable("TSP", "R_two_opt",            (DL_FUNC) two_opt);
  R_RegisterCCallable("TSP", "R_two_opt_sym",        (DL_FUNC) two_opt_sym);
  R_RegisterCCallable("TSP", "R_or_opt",             (DL_FUNC) or_opt);
  R_RegisterCCallable("TSP", "R_three_opt",          (DL_FUNC) three_opt);
  R_RegisterCCallable("TSP", "R_insertion_cost",     (DL_FUNC) insertion_cost);
  R_RegisterCCallable("TSP", "R_tour_length_dist",   (DL_FUNC) tour_length_dist);
  R_RegisterCCallable("TSP", "R_tour_length_matrix", (DL_FUNC) tour_length_matrix);
}
