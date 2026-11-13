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
#' TSP supports tour construction methods, tour improvement heuristics and 
#' an interface to Concorde (a state-of-the-art external solver).
#' Tour construction methods create a tour from scratch, typically using a 
#' construction heuristic. Tour improvement heuristics take an existing tour and
#' try to improve it. If no initial tour is provided for the improvement 
#' heuristic, then an initial tour (e.g., a random tour) is automatically created.
#' 
#' Most heuristic methods accept the following extra control parameters in addition
#' to the parameters specified below:
#'
#' * "rep": an integer indicating how many replications (random restarts) should be performed.
#'  The best result is returned. The replications can be performed in parallel 
#'  (see Section Parallel Execution Support). 
#' * "opt": a character string selecting a local-search refinement that is
#'   performed on the constructed tour. This is just for convenience so a
#'   constructive heuristic can be followed by refinement in a single call.
#'   Available values are `"two"` (2-opt), `"or"` (Or-opt), `"three"` (3-opt),
#'   and `"two+or"` (2-opt and Or-opt alternately until neither improves the
#'   tour). The default is no refinement. See the methods
#'   `"two_opt"`, `"or_opt"` and `"three_opt"` below. 
#' * "two_opt": __deprecated__. Use `opt = "two"` instead.
#' * "verbose": a logical. Most implementations provide verbose output to monitor progress.
#'   This argument can also be helpful to see what heuristics are used and what the
#'   default parameters are.
#'
#' ## Tour Construction Methods
#' 
#' These methods construct tours from scratch. 
#' The available tour construction methods are:
#'
#' - __"identity", "random"__ return a tour representing the order in the data
#'   (identity order) or a random order. \[TSP, ATSP\]
#'
#' - __"nearest_insertion", "farthest_insertion", "cheapest_insertion", "arbitrary_insertion"__
#'   Nearest, farthest, cheapest and
#'   arbitrary insertion algorithms for symmetric and asymmetric TSPs
#'   (Rosenkrantz et al. 1977). \[TSP, ATSP\]
#'
#'   The distances between cities are stored in a distance matrix \eqn{D} with
#'   elements \eqn{d(i,j)}.  All insertion algorithms start with a tour
#'   consisting of an arbitrary city and choose in each step a city \eqn{k} not
#'   yet on the tour. This city is inserted into the existing tour between two
#'   consecutive cities \eqn{i} and \eqn{j}, such that \deqn{d(i,k) + d(k,j) -
#'   d(i,j)} is minimized. The algorithm stops when all cities are on the tour.
#'
#'   The nearest insertion algorithm chooses city \eqn{k} in each step as the
#'   city which is \emph{nearest} to a city on the tour.
#'
#'   For farthest insertion, the city \eqn{k} is chosen in each step as the city
#'   which is \emph{farthest} from any city on the tour.
#'
#'   Cheapest insertion chooses the city \eqn{k} such that the cost of inserting
#'   the new city (i.e., the increase in the tour's length) is minimal.
#'
#'   Arbitrary insertion chooses the city \eqn{k} randomly from all cities not
#'   yet on the tour.
#'
#'   Nearest and cheapest insertion try to build the tour using cities that
#'   fit well into the partial tour constructed so far. The idea behind
#'   farthest insertion is to link distant cities into the tour first to
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
#' ## Tour Improvement Heuristics
#'
#' Tour improvement methods take a tour as the argument `tour` and try to improve the 
#' tour. If no tour is provided, a random tour is used as the initial tour.
#' 
#' The following tour improvement heuristics are available:
#'
#' - __"sa"__ Simulated Annealing for TSPs (Kirkpatrick et al, 1983) \[TSP, ATSP\]
#'
#'   A tour refinement method that uses simulated annealing with subtour 
#'   reversal as a local move. The optimizer is
#'   [stats::optim()] with method `"SANN"`. This method is 
#'   typically a lot slower than `"two_opt"` and requires parameter tuning for the 
#'   cooling schedule.
#'   
#'   __Note:__ For speed, a relatively small number of evaluations (`maxit`) is 
#'   used by default. A much larger value will result in more competitive results.
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
#'   - "maxit" number of evaluations. Default is 10000.
#'   - "trace" set to 1 to print progress. 
#'   
#'   See [stats::optim()] for more details on the parameters.
#'   
#' - __"two_opt"__ Two-edge exchange improvement procedure (Croes 1958). \[TSP, ATSP\]
#'
#'   This is a tour refinement procedure which systematically exchanges two edges
#'   in the graph represented by the distance matrix until no improvements are
#'   possible. Exchanging two edges is equal to reversing part of the tour. The
#'   resulting tour is called _2-optimal._ This heuristic takes \eqn{O(n^2)} time.
#'
#'   Additional control options:
#'   - "tour" an existing tour which should be improved.
#'     If no tour is given, a random tour is used.
#'   - "two_opt_repetitions" number of times to try with random restarts 
#'    (default: 1).
#'
#' - __"or_opt"__ Or-opt segment insertion (Or 1976). \[TSP, ATSP\]
#'
#'   This is a tour refinement procedure which moves segments of 1 to
#'   `max_segment` consecutive cities to a better position in the tour until
#'   no improvement is possible. For symmetric problems, segments may also be
#'   reinserted in reverse order. Or-opt can move a misplaced city or small
#'   group of cities in a single step, which 2-opt cannot. It is often also
#'   used as an additional improvement step after "two_opt" (see Examples).
#'   This heuristic also takes \eqn{O(n^2)} time.
#'
#'   Additional control options:
#'   - "tour" an existing tour which should be improved.
#'     If no tour is given, a random tour is used.
#'   - "max_segment" maximum number of cities in a moved segment (default: 3).
#'
#' - __"three_opt"__ Three-edge exchange improvement procedure
#'   (Bock 1958; Lin 1965). \[TSP, ATSP\]
#'
#'   This is a tour refinement procedure which removes three edges and
#'   reconnects the three resulting parts of the tour in the best possible way
#'   until no improvements are possible. The resulting tour is called
#'   _3-optimal._ The search checks all triples of edges and each pass
#'   takes \eqn{O(n^3)} time, so it is only practical for small to medium
#'   problems. For ATSPs, only the reconnection that does not reverse any part
#'   of the tour is used.
#'
#'   Additional control options:
#'   - "tour" an existing tour which should be improved.
#'     If no tour is given, a random tour is used.
#'
#'  ## State-of-the-art Solvers Interfaces
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
#'     `precision` places to the left). The interface to Concorde uses
#'     [write_TSPLIB()].
#'   - "seed" an optional whole number between 0 and `.Machine$integer.max`
#'     that is passed on to Concorde's `-s` option to make its randomized
#'     components reproducible. __If no seed is specified, Concorde is still
#'     seeded:__ one random number is drawn from R's random number generator
#'     and passed on as the seed. Therefore, calling [set.seed()] before
#'     `solve_TSP()` also makes Concorde reproducible (see Section Reproducible
#'     Results). Do not combine `seed` with `-s` in `clo`.
#'   - "verbose" logical; `FALSE` suppresses the output printed to the terminal.
#'
#' - __"linkern"__ Concorde's Chained Lin-Kernighan heuristic (Applegate et al. 2003). \[TSP, ETSP\]
#'
#'   The Lin-Kernighan (Lin and Kernighan 1973) heuristic uses variable \eqn{k}
#'   edge exchanges to improve an initial tour.  The program is not included in
#'   this package and has to be obtained and installed separately (see [Concorde]).
#'
#'   Additional control options: see Concorde above. This includes `seed`, which
#'   is passed on to linkern's `-s` option and is drawn from R's random number
#'   generator if it is not specified.
#'
#' # Treatment of `NA`s and Infinite Values in `x`
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
#' # Solving ATSP and ETSP
#'
#' Some solvers (including Concorde) cannot solve [ATSP] directly.
#' An `ATSP` can be reformulated as a larger `TSP` and solved
#' this way. For convenience, `solve_TSP()` has an extra argument
#' `as_TSP` which can be set to `TRUE` to automatically solve the
#' `ATSP` reformulated as a `TSP` (see [reformulate_ATSP_as_TSP()]).
#'
#' Only methods "concorde" and "linkern" currently solve [ETSP]s directly.
#' For all other methods, ETSPs are converted into TSPs by creating a
#' distance matrix and then solved. Note: distance matrices can become 
#' very large, leading to high memory usage and long computation times.
#'
#' # Parallel Execution Support
#'
#' If several repetitions are performed (this includes method
#' `"repetitive_nn"`) then \pkg{foreach} is used so they can be performed
#' in parallel on multiple cores/machines. To enable parallel execution an
#' appropriate parallel backend needs to be registered
#' (e.g., [doParallel::registerDoParallel()]).
#'
#' # Reproducible Results
#'
#' All randomized methods implemented in this package (e.g., `"random"`,
#' `"nn"`, `"arbitrary_insertion"`, `"sa"`, and repetitions with `rep`) use
#' R's global random number generator. Call [set.seed()] before `solve_TSP()` to
#' make the results reproducible, and note that every call advances the global
#' random-number stream. If a parallel backend is used for repetitions 
#' (`rep > 1` and `"repetitive_nn"`), one random number
#' per repetition is drawn from the global random number generator of the main
#' R process and used as the seed of the iteration.
#'
#' Concorde and linkern are external programs with their own random number
#' generators. The programs are seeded with with one random number is
#' drawn from R's global random number generator. This means that 
#' [set.seed()] also makes their results reproducible. 
#' In the unlikely evenet, that a specific seed value for these programs is needed, 
#' then it can be set via the control option `seed` 
#' (a whole number between 0 and `.Machine$integer.max`, e.g., 
#' `control = list(seed = 1234)`), which is passed on to the program via its
#' `-s` command line option. 
#'
#' @family solver
#'
#' @param x a TSP problem.
#' @param method method to solve the TSP (default: "arbitrary insertion"
#' construction followed by the "two_opt" improvement heuristic).
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
#' F. Bock (1958): An algorithm for solving traveling-salesman and related
#' network optimization problems. Unpublished manuscript presented at the 14th
#' ORSA meeting, Saint Louis.
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
#' S. Lin (1965): Computer solutions of the traveling salesman problem.
#' \emph{Bell System Technical Journal}, 44(10):2245--2269.
#' \doi{10.1002/j.1538-7305.1965.tb04146.x}
#'
#' I. Or (1976): Traveling salesman-type combinatorial problems and their
#' relation to the logistics of regional blood banking. PhD thesis,
#' Northwestern University, Evanston, IL.
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
#' ## manually run a construction followed by an improvement heuristic by 
#' ## calling solve_TSP() twice.
#' tour_constr <- solve_TSP(etsp, method = "nn")
#' tour_constr
#' 
#' tour_improved <- solve_TSP(etsp, method = "sa", tour = tour_constr)
#' tour_improved
#'
#' ## chain improvement heuristics: 2-opt, then Or-opt, then 3-opt
#' tour_2 <- solve_TSP(etsp, method = "two_opt", tour = tour_constr)
#' tour_or <- solve_TSP(etsp, method = "or_opt", tour = tour_2)
#' tour_3 <- solve_TSP(etsp, method = "three_opt", tour = tour_or)
#' c(nn = tour_length(tour_constr), two_opt = tour_length(tour_2),
#'   or_opt = tour_length(tour_or), three_opt = tour_length(tour_3))
#'
#' ## compare methods
#' data("USCA50")
#' USCA50
#' methods <- c("identity", "random", "nearest_insertion",
#'   "cheapest_insertion", "farthest_insertion", "arbitrary_insertion",
#'   "nn", "repetitive_nn", "two_opt", "or_opt", "three_opt", "sa")
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
#' tours$'nn+two_opt' <- solve_TSP(USCA50, method = "nn", opt = "two")
#' tours$'nn+two_opt+or_opt' <- solve_TSP(USCA50, method = "nn", opt = "two+or")
#' tours$'nn+rep_10' <- solve_TSP(USCA50, method = "nn", rep = 10)
#' tours$'nn+two_opt+rep_10' <- solve_TSP(USCA50, method = "nn", opt = "two", rep = 10)
#' tours$'arbitrary_insertion+two_opt' <- solve_TSP(USCA50)
#'
#' ## show first tour
#' tours[[1]]
#'
#' ## compare tour lengths
#' opt <- 14497 # obtained by Concorde
#' tour_lengths <- sort(sapply(tours, tour_length), decreasing = TRUE)
#' dotchart(tour_lengths / opt * 100 - 100, xlab = "percent excess over optimum")
#' abline(v = 0, col = "red")
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
        "NOTE: Solver cannot solve the ATSP directly. Reformulating ATSP as TSP. Use 'as_TSP = TRUE' to suppress this warning.\n"
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
      warning("Conversion of ETSP to TSP creates a distance matrix of size O(n^2). ",
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

  ## two_opt is deprecated in favor of opt
  ## note: use [[ ]] since $ would partially match two_opt_repetitions
  if (!is.null(control[["two_opt"]])) {
    .validate_scalar(control[["two_opt"]], "two_opt", "logical")
    message("Control parameter 'two_opt' is deprecated. Use opt = \"two\" instead.")
    if (control[["two_opt"]] && is.null(control$opt))
      control$opt <- "two"
    control[["two_opt"]] <- NULL
  }
  .validate_opt(control$opt)

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
    "or_opt",
    "three_opt",
    "concorde",
    "linkern",
    "sa"
  )

  ## default is arbitrary_insertion + two_opt
  if (is.null(method)) {
    method <- "arbitrary_insertion"
    control$opt <- control$opt %||% "two"
  } else
    method <- match.arg(tolower(method), methods)

  ## no rep or opt for these!
  if (method == "concorde" || method == "linkern") {
    if (control$rep %||% 1L > 1L || !is.null(control$opt))
      warning("control parameters rep and opt not available for methods concorde and linkern and are ignored!")
    control$rep <- NULL
    control$opt <- NULL
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
      identity = tsp_identity(x_, control),
      random = tsp_random(x_, control),
      nearest_insertion = tsp_insertion(x_, type = "nearest", control = control),
      farthest_insertion = tsp_insertion(x_, type = "farthest", control = control),
      cheapest_insertion = tsp_insertion(x_, type = "cheapest", control = control),
      #      arbitrary_insertion = tsp_insertion(x_, type = "arbitrary", control = control),
      arbitrary_insertion = tsp_insertion_arbitrary(x_, control = control),
      nn = tsp_nn(x_, control = control),
      repetitive_nn = tsp_repetitive_nn(x_, control = control),
      two_opt = tsp_two_opt(x_, control = control),
      '2-opt' = tsp_two_opt(x_, control = control),
      or_opt = tsp_or_opt(x_, control = control),
      three_opt = tsp_three_opt(x_, control = control),
      concorde = tsp_concorde(x_, control = control),
      linkern = tsp_linkern(x_, control = control),
      sa = tsp_SA(x_, control = control)
    )
  
    ### do refinement
    if (!is.null(control$opt)) {
      refine <- function(f, order)
        f(x_, control = c(control[intersect(names(control), c("verbose", "max_segment"))],
          list(tour = order)))

      order <- switch(
        control$opt,
        two = refine(tsp_two_opt, order),
        or = refine(tsp_or_opt, order),
        three = refine(tsp_three_opt, order),
        "two+or" = {
          ## alternate until neither neighborhood improves the tour
          repeat {
            old <- order
            order <- refine(tsp_or_opt, refine(tsp_two_opt, order))
            if (identical(as.integer(order), as.integer(old))) break
          }
          order
        }
      )

      method <- paste0(method, "+", switch(
        control$opt,
        two = "two_opt",
        or = "or_opt",
        three = "three_opt",
        "two+or" = "two_opt+or_opt"
      ))
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

  ## Each repetition gets a seed drawn from the RNG of the main R process.
  ## This makes the repetitions reproducible with set.seed() independent of
  ## the foreach backend and the number of workers.
  rs <- .draw_repetition_seeds(n)
  with_seed <- .with_rng_seed ## local copy so it is exported to workers

  #l <- replicate(n, .solve_TSP_worker(x_, method, control), simplify = FALSE)
  i <- 0L ## for R CMD check (no global binding for i)
  l <-
    foreach(i = 1:n) %dopar% {
      with_seed(rs$seeds[i], rs$kind,
        .solve_TSP_worker(x_, method, control))
    }

  l <- l[[which.min(sapply(l, attr, "tour_length"))]]
  attr(l, "method") <-
    paste(attr(l, "method"), "_rep_", n, sep = "")

  return(l)
}
