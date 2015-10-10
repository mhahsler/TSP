
#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"

//#define TWOOPT_DEBUG 1

// only improvements >EPSILON are used
#define EPSILON 1.0e-7

/* 2-opt 
 *
 * R_matrix ... matrix with distances
 * R_t .. initial tour
 *
 * inf and NA is not allowed!
 *
 */


// this implementation also works for asymetric TSPs
SEXP two_opt(SEXP R_matrix, SEXP R_t) {
  
  int i, j, n;
  int swaps;
  int swap1, swap2;
  double imp, imp_tmp, imp_best;
  int tmp;
  double *matrix = REAL(R_matrix);
  // make a copy of the initial tour (Note: t is 1-based)
  int *t = INTEGER(R_t = PROTECT(duplicate(R_t))); 
  
  // check   
  n = INTEGER(GET_DIM(R_matrix))[0];
  if (LENGTH(R_t) != n)
    error("tour has invalid length");
  
  for (i = 0; i < n; i++)
    if (t[i] < 1 || t[i] > n)
      error("tour contains invalid values");
    
    
  // main loop
  do{
    swaps = 0;
    imp_best = 0.0;
    
    // find the swap with largest improvement
    // i is first city to swap (0-based index)
    for (i = 1; i < n-1; i++){
      imp = 0.0;
      
      // add gains for getting to the first city (i-1 -> i)
      imp += matrix[M_POS(n, t[i-1]-1, t[i]-1)];
      
      // gains by adding one more city to the swap
      imp += matrix[M_POS(n, t[i]-1, t[i+1]-1)];
        
      // j is the last city to swap (0-based index)
      for (j = i+1; j < n-1; j++){
        
        // gains by adding one more city to the swap
        imp += matrix[M_POS(n, t[j]-1, t[j+1]-1)];
        
        // increased cost for one more city
        imp -= matrix[M_POS(n, t[j]-1, t[j-1]-1)];
        
        // include gains from closing the tour (i-1 -> j and i -> j+1)
        imp_tmp = imp 
          - matrix[M_POS(n, t[i-1]-1, t[j]-1)] 
          - matrix[M_POS(n, t[i]-1, t[j+1]-1)];
        
        if(imp_tmp > EPSILON) {
          swaps++;
          if(imp_tmp > imp_best) {
            imp_best = imp_tmp;
            swap1 = i; swap2 = j;
          }
        }
      }
      
      // check the last city (needs distance to first city) 
      j = n-1;
      // increased cost for one more city
      imp -= matrix[M_POS(n, t[j]-1, t[j-1]-1)];
        
      // include gains from closing the tour (this time with first city)
      imp_tmp = imp 
        - matrix[M_POS(n, t[i-1]-1, t[j]-1)] 
        - matrix[M_POS(n, t[i]-1, t[0]-1)];
        
      // check improvement
        if(imp_tmp > EPSILON) {
          swaps++;
          if(imp_tmp > imp_best) {
            imp_best = imp_tmp;
            swap1 = i; swap2 = j;
          }
        }
      }

    
#ifdef TWOOPT_DEBUG
    printf("two_opt: %d possible swaps\n", swaps);
#endif        
    
    // reverse sub-tour
    if(swaps > 0){
      
#ifdef TWOOPT_DEBUG
      printf("two_opt: best improvement is %f\n", imp_best);
      printf("two_opt: reversing cities %d to %d\n", swap1+1, swap2+1);
#endif        
      for(i = 0; i < (swap2-swap1+1)/2; i++) { // +1 for even length
        tmp = t[swap1+i];
        t[swap1+i] = t[swap2-i];
        t[swap2-i] = tmp;
      }
    }
    
    R_CheckUserInterrupt();
  }while (swaps > 0);
  
  UNPROTECT(1);
  return R_t;
}


// this implementation only works for symetric TSPs
// Note: code is slightly faster than the code above but is not unused
SEXP two_opt_sym(SEXP R_matrix, SEXP R_t) {
  
  int i, j, n;
  int swaps;
  int swap1, swap2;
  double e1, e2, e1_swap, e2_swap;
  double imp, cur_imp;
  int tmp;
  double *matrix = REAL(R_matrix);
  // make a copy of the initial tour
  int *t = INTEGER(R_t = PROTECT(duplicate(R_t))); 
  
  // check   
  n = INTEGER(GET_DIM(R_matrix))[0];
  if (LENGTH(R_t) != n)
    error("tour has invalid length");
  
  for (i = 0; i < n; i++)
    if (t[i] < 1 || t[i] > n)
      error("tour contains invalid values");
    
    
    
    // main loop
    do{
      swaps = 0;
      imp = 0.0;
      
      // find the swap with largest improvement (works only for symetric TSPs)
      for (i = 0; i < (n-2); i++){
        e1 = matrix[M_POS(n, t[i]-1, t[i+1]-1)]; 
        
        for (j = (i+1); j < (n-1); j++){
          e2 = matrix[M_POS(n, t[j]-1, t[j+1]-1)];
          e1_swap = matrix[M_POS(n, t[i]-1, t[j]-1)];
          e2_swap = matrix[M_POS(n, t[i+1]-1, t[j+1]-1)];
          
          
          /* // handle pos inf (now handled in R code)
           if (e1_swap == R_PosInf || e2_swap == R_PosInf) 
           cur_imp = 0;
           else if (e1 == R_PosInf || e2 == R_PosInf) 
           cur_imp = R_PosInf;
           else */
          
          cur_imp = (e1+e2) - (e1_swap+e2_swap);
          
          if(cur_imp > 0) {
            swaps++;
            if(cur_imp > imp) {
              imp = cur_imp;
              swap1 = i+1; swap2 = j;
            }
          }
        }
        
        // check the last city (needs distance to first city) 
        e2 = matrix[M_POS(n, t[n-1]-1, t[0]-1)];
        e1_swap = matrix[M_POS(n, t[i]-1, t[n-1]-1)];
        e2_swap = matrix[M_POS(n, t[i+1]-1, t[0]-1)];
        cur_imp = (e1+e2) - (e1_swap+e2_swap);
        
        // check improvement
        if(cur_imp > 0) {
          swaps++;
          if(cur_imp > imp) {
            imp = cur_imp;
            swap1 = i+1; swap2 = n-1;
          }
        }
      }
      
#ifdef TWOOPT_DEBUG
      printf("two_opt: %d possible swaps\n", swaps);
#endif        
      
      // reverse sub-tour
      if(swaps > 0){
        
#ifdef TWOOPT_DEBUG
        printf("two_opt: best improvement is %f\n", imp);
        printf("two_opt: swapping %d to %d\n", swap1, swap2);
#endif        
        for(i = 0; i < (swap2-swap1+1)/2; i++) { // +1 for even length
          tmp = t[swap1+i];
          t[swap1+i] = t[swap2-i];
          t[swap2-i] = tmp;
        }
      }
      
      R_CheckUserInterrupt();
    }while (swaps >0);
    
    UNPROTECT(1);
    return R_t;
}
