# Changelog

## TSP 1.2.8 (xx/xx/2026)

- Modernized the test suite to testthat edition 3.
- Expanded solver, TSPLIB, boundary-case, and reproducibility tests;
  fixed zero-city handling for ATSP, ETSP, and random tours and one-city
  handling for simulated annealing.
- Added a `seed` control option for reproducible randomized repetitions
  across sequential and parallel foreach backends.
- The Concorde and linkern interfaces now check solver exit statuses and
  clean temporary and auxiliary files after both successful and failed
  runs.
- Fixed spelling and documentation.
- Fixed input-validation bugs in TSP(), ATSP(), tour_length.ETSP(), and
  validate tour indices before passing them to C.
- Fixed dll registration.

## TSP 1.2.7 (03/22/2026)

CRAN release: 2026-03-23

- Improved verbose output for method “sa.”
- Improved man page for solve_TSP (suggested by Henrik Bengtsson).
- Added TSPLIB support for formats GEO and ATT (by Benjamin
  Schwedinger).

## TSP 1.2.6 (11/26/2025)

CRAN release: 2025-11-27

- Added method sa for simulated annealing.
- Unknown parameters now create warnings.
- Added verbose output to insertion algorithms.
- Improved some man pages.
- Updated README.
- Bugfix: Fixed precision issue for concorde/linkern with small distance
  values.

## TSP 1.2-5 (05/27/2025)

CRAN release: 2025-05-27

- Changed Package description title.
- Bugfix: ETSP write issue for method Concorde. parameter precision is
  now used correctly (reported by: malipivo)

## TSP 1.2-4 (04/03/2023)

CRAN release: 2023-04-04

- removed dependency on maptools.

## TSP 1.2-3 (03/08/2023)

CRAN release: 2023-03-08

### New Features

- plot for ETSP gained parameter labels to plot city labels.

### Bug Fixes

- register S3 method n_of_cities.default

## TSP 1.2-2 (01/24/2023)

CRAN release: 2023-01-25

### Bug Fixes

- Fixed bool definition for C23.

## TSP 1.2-1 (07/14/2022)

CRAN release: 2022-07-14

### New Features

- added filter_ATSP_as_TSP_dummies().
- tour_length() now also accepts integer vectors instead of TOUR.

### Bug Fixes

- Small integer values are now left unscaled for Concorde.

## TSP 1.2-0 (02/21/2022)

CRAN release: 2022-02-21

### Internal Changes

- The package uses now roxygen.
- C code has now long vector support for dist.

## TSP 1.1-11 (10/06/2021)

CRAN release: 2021-10-06

### Bug Fixes

- cut_tour: fixed dropped city for multiple cut points (reported by
  RegularnaMatrica)

## TSP 1.1-10 (04/17/2020)

CRAN release: 2020-04-17

### Bug Fixes

- Fixed Linkern file management on Windows.
- Converting distances that only contain 0 and Inf (e.g., from an
  adjacency matrix) is now fixed for Concorde and Linkern.
- Reduced the maximum integer for Concorde to avoid overflows (reported
  by sarwanpasha).

## TSP 1.1-9 (02/02/2020)

CRAN release: 2020-02-21

- Maintenance release.

## TSP 1.1-8 (01/23/2020)

CRAN release: 2020-01-23

### New Feature

- solve_TSP for ATSP gained parameter as_TSP to solve the ATSP
  reformulated as a TSP.
- Concorde and linkern can now solve ATSP using a reformulation as a
  TSP.
- cut_tour can now cut a tour into multiple paths.

## TSP 1.1-7 (05/22/2019)

CRAN release: 2019-05-22

### Bug Fixes

- concorde_path now normalizes the path (translates . and ~).
- reformulate_ATSP_as_TSP now keeps the method attribute (i.e., used
  distance measure)
- TSP and ATSP gained parameter method to store the name of the used
  distance metric.
- Fixed read_TSPLIB for EDGE_WEIGHT_FORMAT of LOWER_ROW, LOWER_DIAG_ROW,
  UPPER_COL and UPPER_DIAG_COL (reported by klukac).

## TSP 1.1-6 (04/29/2018)

CRAN release: 2018-04-30

### Bug Fixes

- Start for insertion algorithms is now coerced to integer.
- Fixed problem with TSP with 1 city on Win 32 on R 3.5.0.

## TSP 1.1-5 (02/21/2017)

CRAN release: 2017-02-22

- fixed TSP labels.
- fixed tour_length for ETSP and added tests.

## TSP 1.1-4 (2/21/2016)

CRAN release: 2016-02-22

- fixed bug in arbitrary insertion for TSPs with two or less cities (bug
  report by Shrinidhee Shevade).
- concorde and linkern help: exe argument was removed. The exe control
  argument for both methods in solve_TSP is now deprecated. Use
  concorde_path(path) instead.
- concorde and linkern gained a control argument verbose to suppress the
  output.

## TSP 1.1-3 (9/2/2015)

CRAN release: 2015-09-03

- two-opt now works correctly with asymmetric TSPs (bug report by Luis
  Martinez).

## TSP 1.1-2 (7/30/2015)

CRAN release: 2015-07-01

- Fixed imports for non-base packages.

## TSP 1.1-1 (5/15/2015)

CRAN release: 2015-05-15

- improved speed of C code.
- compatibility with new release of testthat

## TSP 1.1-0 (3/14/2015)

CRAN release: 2015-03-15

- default method is now arbitrary_insertion with two_opt refinement.
- we use foreach (use doParallel) to compute repetitions in parallel
- ETSP (Euclidean TSP) added.
- generic and arguments for tour_length have \# Changed (first argument
  is now a tour).
- method “2-opt” was renamed to “two_opt” so it can also be used as a
  proper variable name.
- solve_TSP gained methods “identity” and “random”.
- solve_TSP gained options “repetition” and “two_opt”.

## TSP 1.0-10 (2/3/2015)

CRAN release: 2015-02-03

- added check for argument cut in cut_tour
- Finding concord and linkern is now case insensitive (Reported by Mark
  Otto)

## TSP 1.0-9 (7/16/2014)

CRAN release: 2014-07-16

- Check for NAs in distances.
- +INF and -INF are now handled in solve_TSP.
- fixed single quotes in vignette.

## TSP 1.0-8 (9/6/2013)

CRAN release: 2013-09-06

- service release.

## TSP 1.0-7 (8/22/2012)

CRAN release: 2012-08-22

- Added PACKAGE argument to C calls.

## TSP 1.0-6 (11/29/2011)

CRAN release: 2011-11-29

- Fixed bug in read_TSPLIB.

## TSP 1.0-5 (11/10/2011)

CRAN release: 2011-11-10

- Changed constructor for TOUR to allow for method and tour_length.
- Bug fixes

## TSP 1.0-4 (8/31/2011)

CRAN release: 2011-08-31

- Fixed bug with missing row/column labels in as.ATSP() (reported by Ian
  Deters)

## TSP 1.0-3 (5/26/2011)

CRAN release: 2011-05-26

- Service release

## TSP 1.0-2 (1/14/2011)

CRAN release: 2011-01-14

- Fixed Windows/R bug with temp files.
- Fixed the code for SpatialLines thanks to Roger Bivand.

## TSP 0.1-2 (9/18/2006)

CRAN release: 2006-09-18

- Initial release.
