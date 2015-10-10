#include <R.h>
#include <Rdefines.h>
#include "matrix_pos.h"

typedef enum {false = 0, true = 1} bool;

/*
 * Calculate the tour length given a distance matrix and a permuation vector
 */

SEXP tour_length_dist(SEXP R_dist, SEXP R_order) {

    double tour_length = 0.0;
    SEXP R_tour_length;
    double segment;
    bool posinf = false;
    bool neginf = false;
    
    int *order = INTEGER(R_order);
    int n = INTEGER(getAttrib(R_dist, install("Size")))[0];

    double *dist = REAL(R_dist);

    if (n != LENGTH(R_order)) 
        error("length of distance matrix and tour do not match");

    for (int i = 0; i < (n-1); i++) {
        segment = dist[LT_POS(n, order[i]-1, order[i+1]-1)];
        
        // check Inf
        if (segment == R_PosInf) posinf = true;
        else if (segment == R_NegInf) neginf = true;
        else tour_length += segment; 
    }

    // close tour
    segment = dist[LT_POS(n, order[n-1]-1, order[0]-1)];
        
    // check Inf
    if (segment == R_PosInf) posinf = true;
    else if (segment == R_NegInf) neginf = true;
    else tour_length += segment; 

    // inf
    if (posinf && neginf) tour_length = NA_REAL;
    else if (posinf) tour_length = R_PosInf;
    else if (neginf) tour_length = R_NegInf;

    // create R object
    PROTECT(R_tour_length = NEW_NUMERIC(1));
    REAL(R_tour_length)[0] = tour_length;
    UNPROTECT(1);

    return R_tour_length;
}

/*
 * Calculate tour length form a matrix
 */

SEXP tour_length_matrix(SEXP R_matrix, SEXP R_order) {

    double tour_length = 0.0;
    SEXP R_tour_length;
    double segment;
    bool posinf = false;
    bool neginf = false;
    int n = INTEGER(GET_DIM(R_matrix))[0];
    
    double *matrix = REAL(R_matrix);    
    int *order = INTEGER(R_order);

    if (n != LENGTH(R_order)) 
        error("length of distance matrix and tour do not match");

    for (int i = 0; i < (n-1); i++) {
        segment = matrix[M_POS(n, order[i]-1, order[i+1]-1)]; 
        
        // check inf first
        if (segment == R_PosInf) posinf = true;
        else if (segment == R_NegInf) neginf = true;
        else tour_length += segment; 
    }
    
    // close tour
    segment = matrix[M_POS(n, order[n-1]-1, order[0]-1)]; 
    
    // check inf first
    if (segment == R_PosInf) posinf = true;
    else if (segment == R_NegInf) neginf = true;
    else tour_length += segment; 

    // inf
    if (posinf && neginf) tour_length = NA_REAL;
    else if (posinf) tour_length = R_PosInf;
    else if (neginf) tour_length = R_NegInf;

    PROTECT(R_tour_length = NEW_NUMERIC(1));
    REAL(R_tour_length)[0] = tour_length;
    UNPROTECT(1);
    
    return R_tour_length;
}
