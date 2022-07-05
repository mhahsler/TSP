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


#' Reformulate a ATSP as a symmetric TSP
#'
#' A ATSP can be formulated as a symmetric TSP by doubling the number of cities
#' (Jonker and Volgenant 1983). The solution of the TSP also represents the
#' solution of the original ATSP.
#'
#' To reformulate a [ATSP] as a [TSP], for each city a dummy city (e.g, for 'New
#' York' a dummy city 'New York*') is added. Between each city and its
#' corresponding dummy city a very small (or negative) distance with value
#' `cheap` is used.
#' To ensure that the solver places each cities always occurs in the
#' solution together with its dummy city, this cost has to be much smaller than
#' the distances in the TSP.
#' The original distances are used
#' between the cities and the dummy cities, where each city is responsible for
#' the distance going to the city and the dummy city is responsible for the
#' distance coming from the city. The distances between all cities and the
#' distances between all dummy cities are set to `infeasible`, a very
#' large value which prevents the solver from using these links.
#' We use infinite values here and [solve_TSP()] treats them appropriately.
#'
#' `filter_ATSP_as_TSP_dummies()` can be used to extract the solution for the original
#' ATSP from the tour found for an ATSP reformulated as a TSP. Note that the symmetric TSP
#' tour does not reveal the direction for the ATSP. The filter function computed the
#' tour length for both directions and returns the shorter tour.
#'
#' [solve_TSP()] has a parameter `as_TSP` which preforms the reformulation and
#' filtering the dummy cities automatically.
#'
#' **Note on performance:** Doubling the problem size is a performance issue especially
#' has a negative impact on solution quality for heuristics. It should only be used
#' together with Concorde when the optimal solution is required. Most heuristics can solve
#' ATSPs directly with good solution quality.
#' @family TSP
#'
#' @param x an [ATSP].
#' @param infeasible value for infeasible connections.
#' @param cheap value for distance between a city and its corresponding dummy
#' city.
#' @param tour a [TOUR] created for a ATSP reformulated as a TSP.
#' @param atsp the original [ATSP].
#' @return
#' `reformulate_ATSP_as_TSP()` returns a [TSP] object.
#' `filter_ATSP_as_TSP_dummies()` returns a [TOUR] object.
#' @author Michael Hahsler
#' @references Jonker, R. and Volgenant, T. (1983): Transforming asymmetric
#' into symmetric traveling salesman problems, _Operations Research
#' Letters,_ 2, 161--163.
#' @keywords optimize
#' @examples
#' data("USCA50")
#'
#' ## set the distances from anywhere to Austin to zero which makes it an ATSP
#' austin <- which(labels(USCA50) == "Austin, TX")
#' atsp <- as.ATSP(USCA50)
#' atsp[, austin] <- 0
#' atsp
#'
#' ## reformulate as a TSP (by doubling the number of cities with dummy cities marked with *)
#' tsp <- reformulate_ATSP_as_TSP(atsp)
#' tsp
#'
#' ## create tour for the TSP. You should use Concorde to find the optimal solution.
#' # tour_tsp <- solve_TSP(tsp, method = "concorde")
#' # The standard heuristic is bad for this problem. We use it here because
#' #   Concord may not be installed.
#' tour_tsp <- solve_TSP(tsp)
#' head(labels(tour_tsp), n = 10)
#' tour_tsp
#' # The tour length is -Inf since it includes cheap links
#' #  from a city to its dummy city.
#'
#' ## get the solution for the original ATSP by filtering out the dummy cities.
#' tour_atsp <- filter_ATSP_as_TSP_dummies(tour_tsp, atsp = atsp)
#' tour_atsp
#' head(labels(tour_atsp), n = 10)
#'
#' ## This process can also be done automatically by using as_TSP = TRUE:
#' # solve_TSP(atsp, method = "concorde", as_TSP = TRUE)
#'
#' ## The default heuristic can directly solve ATSPs with results close to the
#' #  optimal solution of 12715.
#' solve_TSP(atsp, control = list(rep = 10))
#' @export
reformulate_ATSP_as_TSP <-
  function(x,
    infeasible = Inf,
    cheap = -Inf) {
    if (!inherits(x, "ATSP"))
      stop("x is not an ATSP object!")

    method <- attr(x, "method")
    m <- as.matrix(x)

    ### check all but the diagonal for cheap!
    if (cheap >= min(m[-seq(1, nrow(m)^2, nrow(m)+1)]))
      stop("cheap needs to be strictly smaller than the smallest distance in the ATSP!")
    if (infeasible < max(m))
      stop("infeasible needs to be larger than the largest distance in the ATSP!")

    ## scale matrix and add cheap links
    diag(m) <- cheap

    tsp <- rbind(cbind(matrix(
      infeasible, ncol = ncol(m), nrow = nrow(m)
    ), t(m)),
      cbind(m, matrix(
        infeasible, ncol = nrow(m), nrow = ncol(m)
      )))

    ## create labels (* for virtual cities)
    lab <- c(labels(x), paste(labels(x), "*", sep = ""))
    dimnames(tsp) <- list(lab, lab)
    attr(tsp, "method") <- method

    ## return as TSP
    TSP(tsp)
  }

#' @rdname reformulate_ATSP_as_TSP
#' @export
filter_ATSP_as_TSP_dummies <- function(tour, atsp) {

  ### Note: the tour can be in reverse order.
  t1 <-
    TOUR(tour[as.integer(tour) <= n_of_cities(atsp)], method = attr(tour, "method"), tsp = atsp)
  t2 <-
    TOUR(rev(tour[as.integer(tour) <= n_of_cities(atsp)]),
      method = attr(tour, "method"),
      tsp = atsp)

  if (attr(t1, "tour_length") > attr(t2, "tour_length"))
    t2
  else
    t1
}
