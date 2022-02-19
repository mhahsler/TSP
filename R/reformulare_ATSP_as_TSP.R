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



#' Reformulate a ATSP as a symmetric TSP
#'
#' A ATSP can be formulated as a symmetric TSP by doubling the number of cities
#' (Jonker and Volgenant 1983). The solution of the TSP also represents the
#' solution of the original ATSP.
#'
#' To reformulate a [ATSP] as a [TSP], for each city a dummy city (e.g, for 'New
#' York' a dummy city 'New York*') is added. Between each city and its
#' corresponding dummy city a negative or very small distance with value
#' `cheap` is used.  This makes sure that each cities always occurs in the
#' solution together with its dummy city.  The original distances are used
#' between the cities and the dummy cities, where each city is responsible for
#' the distance going to the city and the dummy city is responsible for the
#' distance coming from the city. The distances between all cities and the
#' distances between all dummy cities are set to `infeasible`, a very
#' large value which makes the infeasible.
#'
#' @family TSP
#'
#' @param x an [ATSP].
#' @param infeasible value for infeasible connections.
#' @param cheap value for distance between a city and its corresponding dummy
#' city.
#' @return a [TSP] object.
#' @author Michael Hahsler
#' @references Jonker, R. and Volgenant, T. (1983): Transforming asymmetric
#' into symmetric traveling salesman problems, \emph{Operations Research
#' Letters, 2, 161--163.}
#' @keywords optimize
#' @examples
#' data("USCA50")
#'
#' ## set the distances towards Austin to zero which makes it a ATSP
#' austin <- which(labels(USCA50) == "Austin, TX")
#' atsp <- as.ATSP(USCA50)
#' atsp[, austin] <- 0
#'
#' ## reformulate as a TSP
#' tsp <- reformulate_ATSP_as_TSP(atsp)
#' labels(tsp)
#'
#' ## create tour (now you could use Concorde or LK)
#' tour_atsp <- solve_TSP(tsp, method="nn")
#' head(labels(tour_atsp), n = 10)
#' tour_atsp
#' ## Note that the tour has a lenght of -Inf since the reformulation created
#' ## some -Inf distances
#'
#' ## filter out the dummy cities (we specify tsp so the tour lenght is
#' ## recalculated)
#' tour <- TOUR(tour_atsp[tour_atsp <= n_of_cities(atsp)], tsp = atsp)
#' tour
#' @export
reformulate_ATSP_as_TSP <- function(x, infeasible = Inf, cheap = -Inf) {
    if(!inherits(x, "ATSP")) stop("x is not an ATSP object!")

    method <- attr(x, "method")
    m <- as.matrix(x)

    ## scale matrix and add cheap links
    diag(m) <- cheap

    tsp <- rbind(
        cbind(matrix(infeasible, ncol = ncol(m), nrow = nrow(m)), t(m)),
        cbind(m, matrix(infeasible, ncol = nrow(m), nrow = ncol(m)))
    )

    ## create labels (* for virtual cities)
    lab <- c(labels(x), paste(labels(x), "*", sep = ""))
    dimnames(tsp) <- list(lab, lab)
    attr(tsp, "method") <- method


    ## return as TSP
    TSP(tsp)
}
