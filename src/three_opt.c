#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"

// only improvements >EPSILON are used
#define EPSILON 1.0e-7

// distance between two cities (1-based city ids)
#define DIST(a, b) matrix[(R_xlen_t)((a)-1) + (R_xlen_t)((b)-1) * (R_xlen_t)n]

/* 3-opt (Bock 1958, Lin 1965)
 *
 * Removes three edges (a,b1), (b2,c1), (c2,d) which splits the tour into
 * the segments B = b1..b2 and C = c1..c2 and the rest of the tour (a..d).
 * All 7 ways to reconnect the segments are tried:
 *
 *   1: a B' C  d   (2-opt)       5: a C' B  d
 *   2: a B  C' d   (2-opt)       6: a C  B' d
 *   3: a B' C' d                 7: a C' B' d   (2-opt)
 *   4: a C  B  d   (no reversal; "or-3opt")
 *
 * For asymmetric distances only move 4 is used since it does not reverse
 * any segment.
 *
 * R_matrix ... matrix with distances
 * R_t ........ initial tour (1-based)
 * R_sym ...... TRUE if the distances are symmetric
 *
 * inf and NA is not allowed!
 */

SEXP three_opt(SEXP R_matrix, SEXP R_t, SEXP R_sym) {

  int i, j, k, n, c, w, sym, improved, move, best_move;
  int a, b1, b2, c1, c2, d;
  double removed, add[8], best_gain;
  double *matrix = REAL(R_matrix);
  // make a copy of the initial tour (Note: t is 1-based)
  int *t = INTEGER(R_t = PROTECT(duplicate(R_t)));

  n = INTEGER(GET_DIM(R_matrix))[0];
  sym = asLogical(R_sym);

  if (LENGTH(R_t) != n)
    error("tour has invalid length");
  for (i = 0; i < n; i++)
    if (t[i] < 1 || t[i] > n)
      error("tour contains invalid values");
  if (sym == NA_LOGICAL)
    error("sym must be TRUE or FALSE");

  if (n < 4) {
    UNPROTECT(1);
    return R_t;
  }

  int *tmp = (int *) R_alloc(n, sizeof(int));

  do {
    improved = 0;

    for (i = 0; i <= n - 3; i++) {
      for (j = i + 1; j <= n - 2; j++) {
        for (k = j + 1; k <= n - 1; k++) {
          a  = t[i];
          b1 = t[i + 1]; b2 = t[j];
          c1 = t[j + 1]; c2 = t[k];
          d  = t[(k + 1) % n];

          removed = DIST(a, b1) + DIST(b2, c1) + DIST(c2, d);

          best_gain = EPSILON;
          best_move = 0;

          add[4] = DIST(a, c1) + DIST(c2, b1) + DIST(b2, d);
          if (removed - add[4] > best_gain) {
            best_gain = removed - add[4]; best_move = 4;
          }

          if (sym) {
            add[1] = DIST(a, b2) + DIST(b1, c1) + DIST(c2, d);
            add[2] = DIST(a, b1) + DIST(b2, c2) + DIST(c1, d);
            add[3] = DIST(a, b2) + DIST(b1, c2) + DIST(c1, d);
            add[5] = DIST(a, c2) + DIST(c1, b1) + DIST(b2, d);
            add[6] = DIST(a, c1) + DIST(c2, b2) + DIST(b1, d);
            add[7] = DIST(a, c2) + DIST(c1, b2) + DIST(b1, d);
            for (move = 1; move <= 7; move++)
              if (removed - add[move] > best_gain) {
                best_gain = removed - add[move]; best_move = move;
              }
          }

          if (best_move == 0)
            continue;

          // rewrite positions i+1..k
          w = 0;
          switch (best_move) {
          case 1: // B' C
            for (c = j; c > i; c--) tmp[w++] = t[c];
            for (c = j + 1; c <= k; c++) tmp[w++] = t[c];
            break;
          case 2: // B C'
            for (c = i + 1; c <= j; c++) tmp[w++] = t[c];
            for (c = k; c > j; c--) tmp[w++] = t[c];
            break;
          case 3: // B' C'
            for (c = j; c > i; c--) tmp[w++] = t[c];
            for (c = k; c > j; c--) tmp[w++] = t[c];
            break;
          case 4: // C B
            for (c = j + 1; c <= k; c++) tmp[w++] = t[c];
            for (c = i + 1; c <= j; c++) tmp[w++] = t[c];
            break;
          case 5: // C' B
            for (c = k; c > j; c--) tmp[w++] = t[c];
            for (c = i + 1; c <= j; c++) tmp[w++] = t[c];
            break;
          case 6: // C B'
            for (c = j + 1; c <= k; c++) tmp[w++] = t[c];
            for (c = j; c > i; c--) tmp[w++] = t[c];
            break;
          case 7: // C' B'
            for (c = k; c > j; c--) tmp[w++] = t[c];
            for (c = j; c > i; c--) tmp[w++] = t[c];
            break;
          }
          memcpy(t + i + 1, tmp, w * sizeof(int));
          improved = 1;
        }
      }
      R_CheckUserInterrupt();
    }
  } while (improved);

  UNPROTECT(1);
  return R_t;
}
