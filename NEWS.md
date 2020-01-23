# TSP 1.1-8 (01/23/2020)

## New Feature
* solve_TSP for ATSP gained parameter as_TSP to solve the ATSP reformulated as a TSP.
* Concorde and linkern can now solve ATSP using a reformulation as a TSP.
* cut_tour can now cut a tour into multiple paths.

# TSP 1.1-7 (05/22/2019)

## Bugfixes
* concorde_path now normalizes the path (translates . and ~).
* reformulate_ATSP_as_TSP now keeps the method attribute (i.e., used distance measure)
* TSP and ATSP gained parameter method to store the name of the used distance metric.
* Fixed read_TSPLIB for EDGE_WEIGHT_FORMAT of LOWER_ROW, LOWER_DIAG_ROW, UPPER_COL and UPPER_DIAG_COL 
  (reported by klukac).  

# TSP 1.1-6 (04/29/2018)

## Bugfixes
* Start for insertion algorithms is now coerced to integer.
* Fixed problem with TSP with 1 city on Win 32 on R 3.5.0.

# TSP 1.1-5 (02/21/2017)

* fixed TSP labels.
* fixed tour_length for ETSP and added tests.

# TSP 1.1-4 (2/21/2016)

* fixed bug in arbitrary insertion for TSPs with two or less cities
      (bug report by Shrinidhee Shevade).
* concorde and linkern help: exe argument was removed. The exe control
      argument for both methods in solve_TSP is now deprecated.
      Use concorde_path(path) instead.
* concorde and linkern gained a control argument verbose to
      suppress the output.

# TSP 1.1-3 (9/2/2015)

* two-opt now works correctly with asymmetric TSPs
      (bug report by Luis Martinez).

# TSP 1.1-2 (7/30/2015)

* Fixed imports for non-base packages.

# TSP 1.1-1 (5/15/2015)

* improved speed of C code.
* compatibility with new release of testthat

# TSP 1.1-0 (3/14/2015)

* default method is now arbitrary_insertion with two_opt refinement.
* we use foreach (use doParallel) to compute repetitions in parallel
* ETSP (Euclidean TSP) added.
* generic and arguments for tour_lengh have # Changed
      (fist argument is now a tour).
* method "2-opt" was renamed to "two_opt" so it can also be used as a
        proper variable name.
* solve_TSP gained methods "identity" and "random".
* solve_TSP gained options "repetition" and "two_opt".

# TSP 1.0-10 (2/3/2015)

* added check for argument cut in cut_tour
* Finding concord and linkern is now case
        insensitive (Reported by Mark Otto)

# TSP 1.0-9 (7/16/2014)

* Check for NAs in distances.
* +INF and -INF are now handled in solve_TSP.
* fixed single quotes in vignette.

# TSP 1.0-8 (9/6/2013)

* service release.

# TSP 1.0-7 (8/22/2012)

* Added PACKAGE argument to C calls.

# TSP 1.0-6 (11/29/2011)

* Fixed bug in read_TSPLIB.

# TSP 1.0-5 (11/10/2011)

* Changed constructor for TOUR to allow for method and tour_length.
* Bugfix: labels in creator ATSP() (reported by Tiffany Chen)

# TSP 1.0-4 (8/31/2011)

* Fixed bug with missing row/column labels in as.ATSP()
	(reported by Ian Deters)

# TSP 1.0-3 (5/26/2011)

* Service release

# TSP 1.0-2 (1/14/2011)

* Fixed Windows/R bug with temp files.
* Fixed the code for SpatialLines thanks to Roger Bivand.

# TSP 0.1-2 (9/18/2006)

* Initial release.
