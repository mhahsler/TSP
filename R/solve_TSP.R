#######################################################################
# TSP - Traveling Salesperson Problem
# Copyright (C) 2011 Michael Hahsler and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' TSP solver interface
#'
#' Common interface to all TSP solvers in this package.
#'
#' # TSP Methods
#'
#' Currently the following methods are available:
#' - __"identity", "random"__ return a tour representing the order in the data
#'   (identity order) or a random order. \[TSP, ATSP\]
#'
#' - __"nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion"__
#'   Nearest, farthest, cheapest and
#'   arbitrary insertion algorithms for a symmetric and asymmetric TSP
#'   (Rosenkrantz et al. 1977). \[TSP, ATSP\]
#'
#'   The distances between cities are stored in a distance matrix \eqn{D} with
#'   elements \eqn{d(i,j)}.  All insertion algorithms start with a tour
#'   consisting of an arbitrary city and choose in each step a city \eqn{k} not
#'   yet on the tour. This city is inserted into the existing tour between two
#'   consecutive cities \eqn{i} and \eqn{j}, such that \deqn{d(i,k) + d(k,j) -
#'   d(i,j)} is minimized. The algorithms stops when all cities are on the tour.
#'
#'   The nearest insertion algorithm chooses city \eqn{k} in each step as the
#'   city which is \emph{nearest} to a city on the tour.
#'
#'   For farthest insertion, the city \eqn{k} is chosen in each step as the city
#'   which is \emph{farthest} to any city on the tour.
#'
#'   Cheapest insertion chooses the city \eqn{k} such that the cost of inserting
#'   the new city (i.e., the increase in the tour's length) is minimal.
#'
#'   Arbitrary insertion chooses the city \eqn{k} randomly from all cities not
#'   yet on the tour.
#'
#'   Nearest and cheapest insertion tries to build the tour using cities which
#'   fit well into the partial tour constructed so far.  The idea behind behind
#'   farthest insertion is to link cities far away into the tour fist to
#'   establish an outline of the whole tour early.
#'
#'   Additional control options:
#'   - "start" index of the first city (default: a random city).
#'
#' - __"nn", "repetitive_nn"__ Nearest neighbor and repetitive
#'   nearest neighbor algorithms for symmetric and asymmetric TSPs (Rosenkrantz
#'   et al. 1977). \[TSP, ATSP\]
#'
#'   The algorithm starts with a tour containing a random city. Then the
#'   algorithm always adds to the last city on the tour the nearest not yet
#'   visited city. The algorithm stops when all cities are on the tour.
#'
#'   Repetitive nearest neighbor constructs a nearest neighbor tour for each city
#'   as the starting point and returns the shortest tour found.
#'
#'   Additional control options:
#'   - "start" index of the first city (default: a random city).
#'
#' - __"sa"__ Simulated Annealing for TSPs (Kirkpatrick et al, 1983) \[TSP, ATSP\]
#'
#'   A tour refinement method that uses simulated annealing with subtour 
#'   reversal as local move. The used optimizer is
#'   [stats::optim()] with method `"SANN"`. This method is 
#'   typically a lot slower than `"two_opt"` and requires parameter tuning for the 
#'   cooling schedule.
#'
#'   Additional control options:
#'   - "tour" an existing tour which should be improved.
#'     If no tour is given, a random tour is used.
#'   - "local_move" a function that creates a local move with the current 
#'      tour as the first and the TSP as the second parameter. Defaults to
#'      randomized subtour reversal.
#'   - "temp" initial temperature. Defaults to the length of the current tour divided
#'      by the number of cities.
#'   - "tmax" number of evaluations per temperature step. Default is 10.
#'   - "maxit" number of evaluations. Default is 10000 for speed, but larger values 
#'      will result in more competitive results.
#'   - "trace" set to 1 to print progress. 
#'   
#'   See [stats::optim()] for more details on the parameters.
#'   
#' - __"two_opt"__ Two edge exchange improvement procedure (Croes 1958). \[TSP, ATSP\]
#'
#'   This is a tour refinement procedure which systematically exchanges two edges
#'   in the graph represented by the distance matrix till no improvements are
#'   possible. Exchanging two edges is equal to reversing part of the tour. The
#'   resulting tour is called _2-optimal._
#'
#'   This method can be applied to tours created by other methods or used as its
#'   own method. In this case improvement starts with a random tour.
#'
#'   Additional control options:
#'   - "tour" an existing tour which should be improved.
#'     If no tour is given, a random tour is used.
#'   - "two_opt_repetitions" number of times to try two_opt with a
#'     different initial random tour (default: 1).
#'
#' - __"concorde"__ Concorde algorithm (Applegate et al. 2001). \[TSP, ETSP\]
#'
#'   Concorde is an advanced exact TSP solver for _symmetric_ TSPs
#'   based on branch-and-cut.
#'   ATSPs can be solved using [reformulate_ATSP_as_TSP()] done automatically
#'   with `as_TSP = TRUE`.
#'   The program is not included in this package and
#'   has to be obtained and installed separately.
#'
#'   Additional control options:
#'   - "exe" a character string containing the path to the executable (see [Concorde]).
#'   - "clo" a character string containing command line options for
#'      Concorde, e.g., `control = list(clo = "-B -v")`. See
#'      [concorde_help()] on how to obtain a complete list of available command
#'      line options.
#'   - "precision" an integer which controls the number of decimal
#'     places used for the internal representation of distances in Concorde. The
#'     values given in `x` are multiplied by \eqn{10^{precision}} before being
#'     passed on to Concorde. Note that therefore the results produced by Concorde
#'     (especially lower and upper bounds) need to be divided by
#'     \eqn{10^{precision}} (i.e., the decimal point has to be shifted
#'     `precision` placed to the left). The interface to Concorde uses
#'     [write_TSPLIB()].
#'   - "verbose" logical; `FALSE` suppresses the output printed to the terminal.
#'
#' - __"linkern"__ Concorde's Chained Lin-Kernighan heuristic (Applegate et al. 2003). \[TSP, ETSP\]
#'
#'   The Lin-Kernighan (Lin and Kernighan 1973) heuristic uses variable \eqn{k}
#'   edge exchanges to improve an initial tour.  The program is not included in
#'   this package and has to be obtained and installed separately (see [Concorde]).
#'
#'   Additional control options: see Concorde above.
#'
#' # Verbose Operation
#' 
#' Most implementations provide verbose output to monitor progress using the 
#' logical control parameter "verbose".
#' 
#' # Additional refinement and random restarts
#' 
#' Most constructive methods also accept the following extra control parameters:
#' 
#' * "two_opt": a logical indicating if two-opt refinement should be performed on the
#'   constructed tour.
#' * "rep": an integer indicating how many replications (random restarts) should be performed. 
#'
#' # Treatment of `NA`s and infinite values in `x`
#'
#' [TSP] and [ATSP] need to contain valid distances. `NA`s are not allowed. `Inf` is
#' allowed and can be used to model the missing edges in incomplete graphs
#' (i.e., the distance between the two objects is infinite) or infeasible connections.
#' Internally, `Inf` is replaced by a large value given by \eqn{max(x) + 2 range(x)}.
#' Note that the solution might still place the two objects next to each other
#' (e.g., if `x` contains several unconnected subgraphs) which results in
#' a path length of `Inf`. `-Inf` is replaced by \eqn{min(x) - 2 range(x)} and
#' can be used to encourage the solver to place two objects next to each other.
#'
#' # Parallel execution support
#'
#' All heuristics can be used with the control arguments `repetitions`
#' (uses the best from that many repetitions with random starts) and
#' `two_opt` (a logical indicating if two_opt refinement should be
#' performed). If several repetitions are done (this includes method
#' `"repetitive_nn"`) then \pkg{foreach} is used so they can be performed
#' in parallel on multiple cores/machines. To enable parallel execution an
#' appropriate parallel backend needs to be registered (e.g., load
#' \pkg{doParallel} and register it with [doParallel::registerDoParallel()]).
#'
#' # Solving ATSP and ETSP
#'
#' Some solvers (including Concorde) cannot directly solve [ATSP]
#' directly. `ATSP` can be reformulated as larger `TSP` and solved
#' this way. For convenience, `solve_TSP()` has an extra argument
#' `as_TSP` which can be set to `TRUE` to automatically solve the
#' `ATSP` reformulated as a `TSP` (see [reformulate_ATSP_as_TSP()]).
#'
#' Only methods "concorde" and "linkern" currently solve [ETSP]s directly.
#' For all other methods, ETSPs are converted into TSPs by creating a
#' distance matrix and then solved. Note: distance matrices can become 
#' very large leading to long memory issues and long computation times.
#'
#' @family TSP
#' @family TOUR
#'
#' @param x a TSP problem.
#' @param method method to solve the TSP (default: "arbitrary insertion"
#' algorithm with two_opt refinement.
#' @param control a list of arguments passed on to the TSP solver selected by
#' `method`.
#' @param as_TSP should the ATSP reformulated as a TSP for the solver?
#' @param ...  additional arguments are added to `control`.
#' @return An object of class [TOUR].
#' @author Michael Hahsler
#' @references
#' David Applegate, Robert Bixby, Vasek Chvatal, William Cook
#' (2001): TSP cuts which do not conform to the template paradigm,
#' Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
#' Springer.\doi{10.1007/3-540-45586-8_7}
#'
#' D. Applegate, W. Cook and A. Rohe (2003): Chained Lin-Kernighan for Large
#' Traveling Salesman Problems.  \emph{INFORMS Journal on Computing},
#' 15(1):82--92. \doi{10.1287/ijoc.15.1.82.15157}
#'
#' G.A. Croes (1958): A method for solving traveling-salesman problems.
#' \emph{Operations Research}, 6(6):791--812. \doi{10.1287/opre.6.6.791}
#'
#' Kirkpatrick, S., C. D. Gelatt, and M. P. Vecchi (1983): 
#' Optimization by Simulated Annealing. \emph{Science} 220 (4598): 671–80
#' \doi{10.1126/science.220.4598.671}
#'
#' S. Lin and B. Kernighan (1973): An effective heuristic algorithm for the
#' traveling-salesman problem. \emph{Operations Research}, 21(2): 498--516.
#' \doi{10.1287/opre.21.2.498}
#'
#' D.J. Rosenkrantz, R. E. Stearns, and Philip M. Lewis II (1977): An analysis
#' of several heuristics for the traveling salesman problem.  \emph{SIAM
#' Journal on Computing}, 6(3):563--581. \doi{10.1007/978-1-4020-9688-4_3}
#' @keywords optimize
#' @examples
#'
#' ## solve a simple Euclidean TSP (using the default method)
#' etsp <- ETSP(data.frame(x = runif(20), y = runif(20)))
#' tour <- solve_TSP(etsp)
#' tour
#' tour_length(tour)
#' plot(etsp, tour)
#'
#'
#' ## compare methods
#' data("USCA50")
#' USCA50
#' methods <- c("identity", "random", "nearest_insertion",
#'   "cheapest_insertion", "farthest_insertion", "arbitrary_insertion",
#'   "nn", "repetitive_nn", "two_opt", "sa")
#'
#' ## calculate tours
#' tours <- lapply(methods, FUN = function(m) solve_TSP(USCA50, method = m))
#' names(tours) <- methods
#'
#' ## use the external solver which has to be installed separately
#' \dontrun{
#' tours$concorde  <- solve_TSP(USCA50, method = "concorde")
#' tours$linkern  <- solve_TSP(USCA50, method = "linkern")
#' }
#'
#' ## register a parallel backend to perform repetitions in parallel
#' \dontrun{
#' library(doParallel)
#' registerDoParallel()
#' }
#'
#' ## add some tours using repetition and two_opt refinements
#' tours$'nn+two_opt' <- solve_TSP(USCA50, method = "nn", two_opt = TRUE)
#' tours$'nn+rep_10' <- solve_TSP(USCA50, method = "nn", rep = 10)
#' tours$'nn+two_opt+rep_10' <- solve_TSP(USCA50, method = "nn", two_opt = TRUE, rep = 10)
#' tours$'arbitrary_insertion+two_opt' <- solve_TSP(USCA50)
#'
#' ## show first tour
#' tours[[1]]
#'
#' ## compare tour lengths
#' opt <- 14497 # obtained by Concorde
#' tour_lengths <- c(sort(sapply(tours, tour_length), decreasing = TRUE),
#'   optimal = opt)
#' dotchart(tour_lengths / opt * 100 - 100, xlab = "percent excess over optimum")
#' @export
solve_TSP <- function(x,
  method = NULL,
  control = NULL,
  ...)
  UseMethod("solve_TSP")

## TSP
#' @rdname solve_TSP
#' @export
solve_TSP.TSP <- function(x,
  method = NULL,
  control = NULL,
  ...) {
  .solve_TSP(x, method, control, ...)
}

## ATSP
#' @rdname solve_TSP
#' @export
solve_TSP.ATSP <-
  function(x,
    method = NULL,
    control = NULL,
    as_TSP = FALSE,
    ...) {
    # force as_TSP for solvers that cannot deal with ATSPs
    m <- pmatch(tolower(method), c("concorde", "linkern"))
    if (!is.na(m) && length(m) > 0L && !as_TSP) {
      warning(
        "NOTE: Solver cannot solve the ATSP directly. Reformulating ATSP as TSP. Use 'as_TSP = TRUE' to supress this warning.\n"
      )
      as_TSP <- TRUE
    }

    # reformulate ATSP as TSP
    if (as_TSP) {
      x_atsp <- x
      x <- reformulate_ATSP_as_TSP(x_atsp)
    }

    tour <- .solve_TSP(x, method, control, ...)

    if (as_TSP)
      tour <- filter_ATSP_as_TSP_dummies(tour, atsp = x_atsp)

    tour
  }

## ETSP
#' @rdname solve_TSP
#' @export
solve_TSP.ETSP <- function(x,
  method = NULL,
  control = NULL,
  ...) {
  ## all but concorde and linkern can currently only do TSP
  ## TODO: Implement insertion, NN, etc directly for ETSP to avoid creating 
  ##       a large distance matrix
  m <- pmatch(tolower(method), c("concorde", "linkern"))
  if (length(m) == 0L || is.na(m)) {
    if (nrow(x) > 10000L)
      warning("Convertion of ETSP to TSP creates a distance matrix of size O(n^2). ", 
              "This operation may be slow or you may run out of memory. ",
              "Currently, only \"concorde\" and \"linkern\" can directly work with ETSP.", 
              immediate. = TRUE)
    x <- as.TSP(x)
  }

  .solve_TSP(x, method, control, ...)
}



## Deal with Inf: punish (-)Inf with max (min) +(-) 2*range
.replaceInf <- function(x, pInf = NULL, nInf = NULL) {
  if (any(is.infinite(x))) {
    range_x <- range(x, na.rm = TRUE, finite = TRUE)

    # data with only a single non-inf value.
    diff_range <- diff(range_x)
    if (diff_range == 0) {
      if (range_x[1] == 0)
        diff_range <- 1
      else
        diff_range <- range_x[1] * 2
    }

    if (is.null(pInf))
      pInf <- range_x[2] + 2 * diff_range
    if (is.null(nInf))
      nInf <- range_x[1] - 2 * diff_range
    x[x == Inf] <- pInf
    x[x == -Inf] <- nInf
  }
  x
}

## workhorse
.solve_TSP <- function(x,
  method = NULL,
  control = NULL,
  ...) {
  ## add ... to control
  control <- c(control, list(...))

  ## methods
  methods <- c(
    "identity",
    "random",
    "nearest_insertion",
    "farthest_insertion",
    "cheapest_insertion",
    "arbitrary_insertion",
    "nn",
    "repetitive_nn",
    ### deprecate use two_opt
    "2-opt",
    "two_opt",
    "concorde",
    "linkern",
    "sa"
  )

  ## default is arbitrary_insertion + two_opt
  if (is.null(method)) {
    method <- "arbitrary_insertion"
    control$two_opt <- TRUE
  } else
    method <- match.arg(tolower(method), methods)
    
  ## no rep or two_opt for these!
  if (method == "concorde" || method == "linkern") {
    if (control$rep %||% 1L > 1L || control$two_opt %||% FALSE)
      warning("control parameters rep and two_opt not available for methods concorde and linkern and are ignored!")
    control$rep <- NULL
    control$two_opt <- NULL
  }

  ## check for NAs
  if (any(is.na(x)))
    stop("NAs not allowed!")

  ## Inf
  x_ <- .replaceInf(x)

  ## work horses
  .solve_TSP_worker <- function(x_, method, control) {
    order <- switch(
      method,
      identity = seq(n_of_cities(x_)),
      random = sample(n_of_cities(x_)),
      nearest_insertion = tsp_insertion(x_, type = "nearest", control = control),
      farthest_insertion = tsp_insertion(x_, type = "farthest", control = control),
      cheapest_insertion = tsp_insertion(x_, type = "cheapest", control = control),
      #      arbitrary_insertion = tsp_insertion(x_, type = "arbitrary", control = control),
      arbitrary_insertion = tsp_insertion_arbitrary(x_, control = control),
      nn = tsp_nn(x_, control = control),
      repetitive_nn = tsp_repetitive_nn(x_, control = control),
      two_opt = tsp_two_opt(x_, control = control),
      '2-opt' = tsp_two_opt(x_, control = control),
      concorde = tsp_concorde(x_, control = control),
      linkern = tsp_linkern(x_, control = control),
      sa = tsp_SA(x_, control = control)
    )
  
    ### do refinement two_opt
    if (!is.null(control[["two_opt"]]) && control[["two_opt"]]) {
      order <- tsp_two_opt(x_, control = c(control, list(tour = order)))
      method <- paste(method , "+two_opt", sep = "")
    }

    TOUR(order, method = method, tsp = x)
  }

  ## do rep?
  n <- control$rep %||% 1L

  ## reps are handled internally!
  if (method == "repetitive_nn")
    n <- 1L

  if (n == 1L)
    return(.solve_TSP_worker(x_, method, control))

  #l <- replicate(n, .solve_TSP_worker(x_, method, control), simplify = FALSE)
  l <-
    foreach(i = 1:n) %dopar% .solve_TSP_worker(x_, method, control)

  l <- l[[which.min(sapply(l, attr, "tour_length"))]]
  attr(l, "method") <-
    paste(attr(l, "method"), "_rep_", n, sep = "")

  return(l)
}
