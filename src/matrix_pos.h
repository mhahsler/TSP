
/* LT_POS to access a lower triangle matrix by C. Buchta
 * modified by M. Hahsler
 * n ... number of rows/columns
 * i,j ... column and row index [0, (n-1)]
 *
 * Note: does not cover the case i==j!
 *
 * long vectors: we make now sure that the index id R_xlen_t
 */

#ifndef LT_POS
#define LT_POS(n, i, j)					\
  (i)<(j) ? ((R_xlen_t)(n)*(i) - ((R_xlen_t)(i)+1)*(i)/2 + (j)-(i)) -1 \
          : ((R_xlen_t)(n)*(j) - (((R_xlen_t)(j)+1))*(j)/2 + (i)-(j)) -1
#endif

/*
 * access for matrix
 */

#ifndef M_POS
#define M_POS(rows, i, j)   \
    (i) + (R_xlen_t)(j)*(rows)
#endif


