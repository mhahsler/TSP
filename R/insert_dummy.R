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


## insert a dummy city
##generic


#' Insert dummy cities into a distance matrix
#'
#' Inserts dummy cities into a TSP problem.  A
#' dummy city has the same, constant distance (0) to all other cities and is
#' infinitely far from other dummy cities. A dummy city can be used to
#' transform a shortest Hamiltonian path problem (i.e., finding an optimal
#' linear order) into a shortest Hamiltonian cycle problem which can be solved
#' by a TSP solvers (Garfinkel 1985).
#'
#' Several dummy cities can be used together with a TSP solvers to perform
#' rearrangement clustering (Climer and Zhang 2006).
#'
#' The dummy cities are inserted after the other cities in `x`.
#'
#' A `const` of 0 is guaranteed to work if the TSP finds the optimal
#' solution. For heuristics returning suboptimal solutions, a higher
#' `const` (e.g., `2 * max(x)`) might provide better results.
#'
#' @family TSP
#'
#' @param x an object with a TSP problem.
#' @param n number of dummy cities.
#' @param const distance of the dummy cities to all other cities.
#' @param inf distance between dummy cities.
#' @param label labels for the dummy cities. If only one label is given, it is
#' reused for all dummy cities.
#' @returns returns an object of the same class as `x`.
#'
#' @author Michael Hahsler
#' @references Sharlee Climer, Weixiong Zhang (2006): Rearrangement Clustering:
#' Pitfalls, Remedies, and Applications, \emph{Journal of Machine Learning
#' Research} \bold{7}(Jun), pp. 919--943.
#'
#' R.S. Garfinkel (1985): Motivation and modelling (chapter 2). In: E. L.
#' Lawler, J. K. Lenstra, A.H.G. Rinnooy Kan, D.  B. Shmoys (eds.) The
#' traveling salesman problem - A guided tour of combinatorial optimization,
#' Wiley & Sons.
#' @keywords manip
#' @examples
#' ## Example 1: Find a short Hamiltonian path
#' set.seed(1000)
#' x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])
#'
#' tsp <- TSP(dist(x))
#'
#' ## add a dummy city to cut the tour into a path
#' tsp <- insert_dummy(tsp, label = "cut")
#' tour <- solve_TSP(tsp)
#' tour
#'
#' plot(x)
#' lines(x[cut_tour(tour, cut = "cut"),])
#'
#'
#' ## Example 2: Rearrangement clustering of the iris dataset
#' set.seed(1000)
#' data("iris")
#' tsp <- TSP(dist(iris[-5]))
#'
#' ## insert 2 dummy cities to creates 2 clusters
#' tsp_dummy <- insert_dummy(tsp, n = 3, label = "boundary")
#'
#' ## get a solution for the TSP
#' tour <- solve_TSP(tsp_dummy)
#'
#' ## plot the reordered distance matrix with the dummy cities as lines separating
#' ## the clusters
#' image(tsp_dummy, tour)
#' abline(h = which(labels(tour)=="boundary"), col = "red")
#' abline(v = which(labels(tour)=="boundary"), col = "red")
#'
#' ## plot the original data with paths connecting the points in each cluster
#' plot(iris[,c(2,3)], col = iris[,5])
#' paths <- cut_tour(tour, cut = "boundary")
#' for(p in paths) lines(iris[p, c(2,3)])
#'
#' ## Note: The clustering is not perfect!
#' @export
insert_dummy <- function(x, n = 1, const = 0, inf = Inf, label = "dummy")
  UseMethod("insert_dummy")


## use insert dummy from ATSP
#' @rdname insert_dummy
#' @export
insert_dummy.TSP <- function(x, n = 1, const = 0, inf = Inf, label = "dummy") {
    x <- insert_dummy(ATSP(x), n, const, inf, label)
    TSP(x)
}

#' @rdname insert_dummy
#' @export
insert_dummy.ATSP <- function(x, n = 1, const = 0, inf = Inf, label = "dummy") {

    method <- attr(x, "method")

    n <- as.integer(n)
    p <- n_of_cities(x)

    if(length(label) == 1 && n > 1) label = rep(label, n)

    ## add dummy rows/columns
    x <- cbind(x, matrix(const, ncol = n, nrow = p,
            dimnames = list(NULL, label)))
    x <- rbind(x, matrix(const, ncol = p+n, nrow = n,
            dimnames = list(label, NULL)))

    ## place inf between dummies
    if(n>1) {
        x[(p+1):(p+n), (p+1):(p+n)] <- inf
        diag(x[(p+1):(p+n), (p+1):(p+n)]) <- 0
    }

    attr(x, "method") <- method
    ATSP(x)
}

#' @rdname insert_dummy
#' @export
insert_dummy.ETSP <- function(x, n = 1, const = 0, inf = Inf, label = "dummy")
  stop("Dummy cities cannot be used with ETSP! Convert the problem into a TSP.")
