#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"

// only improvements >EPSILON are used
#define EPSILON 1.0e-7

// distance between two cities (1-based city ids)
#define DIST(a, b) matrix[(R_xlen_t)((a)-1) + (R_xlen_t)((b)-1) * (R_xlen_t)n]

// position in the cyclic tour
#define POS(x) (((x) % n + n) % n)

/* Or-opt (Or 1976)
 *
 * Moves segments of 1 to max_len consecutive cities to a different position
 * in the tour (in the same or reversed orientation) if this shortens the
 * tour. The tour is treated as a cycle.
 *
 * R_matrix ... matrix with distances
 * R_t ........ initial tour (1-based)
 * R_max_len .. maximum segment length
 * R_sym ...... TRUE if the distances are symmetric (segments may be reversed)
 *
 * inf and NA is not allowed!
 */

SEXP or_opt(SEXP R_matrix, SEXP R_t, SEXP R_max_len, SEXP R_sym) {

  int i, m, r, L, n, max_len, sym, improved, reversed, best_rev;
  int s1, sL, p, q, u, v, best_m, best_L, best_i;
  double remove_gain, gain, best_gain, add;
  double *matrix = REAL(R_matrix);
  // make a copy of the initial tour (Note: t is 1-based)
  int *t = INTEGER(R_t = PROTECT(duplicate(R_t)));

  n = INTEGER(GET_DIM(R_matrix))[0];
  max_len = asInteger(R_max_len);
  sym = asLogical(R_sym);

  if (LENGTH(R_t) != n)
    error("tour has invalid length");
  for (i = 0; i < n; i++)
    if (t[i] < 1 || t[i] > n)
      error("tour contains invalid values");
  if (max_len == NA_INTEGER || max_len < 1)
    error("max_len must be a positive integer");
  if (sym == NA_LOGICAL)
    error("sym must be TRUE or FALSE");

  // segment and target edge need at least 4 cities
  if (n < 4) {
    UNPROTECT(1);
    return R_t;
  }
  if (max_len > n - 2)
    max_len = n - 2;

  int *tmp = (int *) R_alloc(n, sizeof(int));

  do {
    improved = 0;

    // find the best move for each segment length and start position and
    // apply it directly
    for (L = 1; L <= max_len; L++) {
      for (i = 0; i < n; i++) {
        p  = t[POS(i - 1)];
        s1 = t[i];
        sL = t[POS(i + L - 1)];
        q  = t[POS(i + L)];

        remove_gain = DIST(p, s1) + DIST(sL, q) - DIST(p, q);
        if (remove_gain <= EPSILON)
          continue;

        best_gain = EPSILON;
        best_m = -1; best_rev = 0;

        // target edge (t[m], t[m+1]) outside of the segment and not (p,q)
        for (m = i + L; m <= i + n - 2; m++) {
          u = t[POS(m)];
          v = t[POS(m + 1)];

          for (reversed = 0; reversed <= (sym && L > 1 ? 1 : 0); reversed++) {
            if (!reversed)
              add = DIST(u, s1) + DIST(sL, v) - DIST(u, v);
            else
              add = DIST(u, sL) + DIST(s1, v) - DIST(u, v);

            gain = remove_gain - add;
            if (gain > best_gain) {
              best_gain = gain;
              best_m = m;
              best_rev = reversed;
            }
          }
        }

        if (best_m >= 0) {
          // rebuild the tour: all cities but the segment starting at q,
          // with the segment inserted after position best_m
          best_L = L; best_i = i;
          int w = 0;
          for (r = best_i + best_L; r <= best_i + n - 1; r++) {
            tmp[w++] = t[POS(r)];
            if (r == best_m) {
              int c;
              for (c = 0; c < best_L; c++)
                tmp[w++] = best_rev ?
                  t[POS(best_i + best_L - 1 - c)] : t[POS(best_i + c)];
            }
          }
          memcpy(t, tmp, n * sizeof(int));
          improved = 1;
        }
      }
      R_CheckUserInterrupt();
    }
  } while (improved);

  UNPROTECT(1);
  return R_t;
}
