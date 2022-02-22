#######################################################################
# TSP - Traveling Salesperson Problem
# Copyrigth (C) 2011 Michael Hahsler and Kurt Hornik
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
#' **TSP Methods**
#'
#' Currently the following methods are available:
#' - "identity", "random" return a tour representing the order in the data
#'   (identity order) or a random order.
#' - "nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion"
#'   Nearest, farthest, cheapest and
#'   arbitrary insertion algorithms for a symmetric and asymmetric TSP
#'   (Rosenkrantz et al. 1977).
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
#' - "nn", "repetitive_nn" Nearest neighbor and repetitive
#'   nearest neighbor algorithms for symmetric and asymmetric TSPs (Rosenkrantz
#'   et al. 1977).
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
#' - "two_opt" Two edge exchange improvement procedure (Croes 1958).
#'
#'   This is a tour refinement procedure which systematically exchanges two edges
#'   in the graph represented by the distance matrix till no improvements are
#'   possible. Exchanging two edges is equal to reversing part of the tour. The
#'   resulting tour is called \emph{2-optimal.}
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
#' - "concorde" Concorde algorithm (Applegate et al. 2001).
#'
#'   Concorde is an advanced exact TSP solver for \emph{only symmetric} TSPs
#'   based on branch-and-cut.  The program is not included in this package and
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
#'
#' - "linkern" Concorde's Chained Lin-Kernighan heuristic (Applegate et al. 2003).
#'
#'   The Lin-Kernighan (Lin and Kernighan 1973) heuristic uses variable \eqn{k}
#'   edge exchanges to improve an initial tour.  The program is not included in
#'   this package and has to be obtained and installed separately (see [Concorde]).
#'
#'   Additional control options: see Concorde above.
#'
#' **Treatment of `NA`s and infinite values in `x`**
#'
#' [TSP] and
#' [ATSP] contain distances and `NA`s are not allowed. `Inf` is
#' allowed and can be used to model the missing edges in incomplete graphs
#' (i.e., the distance between the two objects is infinite). Internally,
#' `Inf` is replaced by a large value given by \eqn{max(x) + 2 range(x)}.
#' Note that the solution might still place the two objects next to each other
#' (e.g., if `x` contains several unconnected subgraphs) which results in
#' a path length of `Inf`.
#'
#' **Parallel execution support**
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
#' **Solving ATSP and ETSP**
#'
#' Some solvers (including Concorde) cannot directly solve [ATSP]
#' directly. `ATSP` can be reformulated as larger `TSP` and solved
#' this way. For convenience, `solve_TSP()` has an extra argument
#' `as_TSP` which can be set to `TRUE` to automatically solve the
#' `ATSP` reformulated as a `TSP` (see [reformulate_ATSP_as_TSP()]).
#'
#' Only methods "concorde" and "linkern" can solve [ETSP]s directly.
#' For all other methods, ETSPs are currently converted into TSPs by creating a
#' distance matrix and then solved.
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
#' Springer.
#'
#' D. Applegate, W. Cook and A. Rohe (2003): Chained Lin-Kernighan for Large
#' Traveling Salesman Problems.  \emph{INFORMS Journal on Computing,
#' 15(1):82--92.}
#'
#' G.A. Croes (1958): A method for solving traveling-salesman problems.
#' \emph{Operations Research, 6(6):791--812.}
#'
#' S. Lin and B. Kernighan (1973): An effective heuristic algorithm for the
#' traveling-salesman problem. \emph{Operations Research, 21(2): 498--516.}
#'
#' D.J. Rosenkrantz, R. E. Stearns, and Philip M. Lewis II (1977): An analysis
#' of several heuristics for the traveling salesman problem.  \emph{SIAM
#' Journal on Computing, 6(3):563--581.}
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
#'   "nn", "repetitive_nn", "two_opt")
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
#' tours$'nn+two_opt' <- solve_TSP(USCA50, method="nn", two_opt=TRUE)
#' tours$'nn+rep_10' <- solve_TSP(USCA50, method="nn", rep=10)
#' tours$'nn+two_opt+rep_10' <- solve_TSP(USCA50, method="nn", two_opt=TRUE, rep=10)
#' tours$'arbitrary_insertion+two_opt' <- solve_TSP(USCA50)
#'
#' ## show first tour
#' tours[[1]]
#'
#' ## compare tour lengths
#' opt <- 14497 # obtained by Concorde
#' tour_lengths <- c(sort(sapply(tours, tour_length), decreasing = TRUE),
#'   optimal = opt)
#' dotchart(tour_lengths/opt*100-100, xlab = "percent excess over optimum")
#' @export
solve_TSP <- function(x, method = NULL, control = NULL, ...)
  UseMethod("solve_TSP")

## TSP
#' @rdname solve_TSP
#' @export
solve_TSP.TSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(x, method, control, ...)
}

## ATSP
#' @rdname solve_TSP
#' @export
solve_TSP.ATSP <- function(x, method = NULL, control = NULL, as_TSP = FALSE, ...) {

  m <- pmatch(tolower(method), c("concorde", "linkern"))
  if(!as_TSP && !is.na(m) && length(m) > 0L) {
    warning("NOTE: Solver cannot solve the ATSP directly. Reformulating ATSP as TSP. Use 'as_TSP = TRUE' to supress this warning.\n")
    as_TSP <- TRUE
  }

  # reformulate ATSP as TSP
  if(as_TSP) {
    x_atsp <- x
    x <- reformulate_ATSP_as_TSP(x_atsp)
  }

  tour <- .solve_TSP(x, method, control, ...)

  if(as_TSP) {
    tour <- TOUR(tour[tour<=n_of_cities(x_atsp)], method = attr(tour, "method"), tsp = x_atsp)
    # Tour may be reversed
    tour_rev  <- TOUR(rev(tour), method = attr(tour, "method"), tsp = x_atsp)
    if(tour_length(tour) > tour_length(tour_rev)) tour <- tour_rev
  }

  tour
}

## ETSP
#' @rdname solve_TSP
#' @export
solve_TSP.ETSP <- function(x, method = NULL, control = NULL, ...) {

  ## all but concorde and linkern can only do TSP
  m <- pmatch(tolower(method), c("concorde", "linkern"))
  if(length(m) == 0L || is.na(m)) x <- as.TSP(x)

  .solve_TSP(x, method, control, ...)
}



## Deal with Inf: punish (-)Inf with max (min) +(-) 2*range
.replaceInf <- function(x, pInf = NULL, nInf = NULL) {
  if(any(is.infinite(x))) {
    range_x <- range(x, na.rm = TRUE, finite = TRUE)

    # data with only a single non-inf value.
    diff_range <- diff(range_x)
    if(diff_range == 0) {
      if(range_x[1] == 0) diff_range <- 1
      else diff_range <- range_x[1] * 2
    }

    if(is.null(pInf)) pInf <- range_x[2] + 2*diff_range
    if(is.null(nInf)) nInf <- range_x[1] - 2*diff_range
    x[x == Inf] <- pInf
    x[x == -Inf] <- nInf
  }
  x
}

## workhorse
.solve_TSP <- function(x, method = NULL, control = NULL, ...) {

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
    "2-opt", ### deprecated
    "two_opt",
    "concorde",
    "linkern"
  )

  ## default is arbitrary_insertion + two_opt
  if(is.null(method)) {
    method <- "arbitrary_insertion"
    if(is.null(control$two_opt))
      control <- c(control, list(two_opt = TRUE))
  } else method <- match.arg(tolower(method), methods)


  ## check for NAs
  if(any(is.na(x))) stop("NAs not allowed!")

  ## Inf
  x_ <- .replaceInf(x)

  ## work horses
  .solve_TSP_worker <- function(x_, method, control) {
    order <- switch(method,
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
      linkern = tsp_linkern(x_, control = control)
    )

    ### do refinement two_opt
    if(!is.null(control$two_opt) && control$two_opt) {
      order <- tsp_two_opt(x_, control = c(control, list(tour = order)))
      method <- paste(method , "+two_opt", sep = "")
    }

    TOUR(order, method=method, tsp=x)
  }

  ## do rep?
  if(!is.null(control$rep)) n <- control$rep
  else n <- 1L

  if(method == "concorde" || method == "linkern") {
    n <- 1L
    control$two_opt <- NULL ## no two_opt for these!
  }
  if(method == "repetitive_nn") n <- 1L

  if(n==1L) return(.solve_TSP_worker(x_, method, control))

  #l <- replicate(n, .solve_TSP_worker(x_, method, control), simplify = FALSE)
  l <- foreach(i = 1:n) %dopar% .solve_TSP_worker(x_, method, control)


  l <- l[[which.min(sapply(l, attr, "tour_length"))]]
  attr(l, "method") <- paste(attr(l, "method"), "_rep_", n, sep="")
  return(l)
}
