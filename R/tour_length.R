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

#' Calculate the length of a tour
#'
#' Calculate the length of a [TOUR] for a [TSP].
#'
#' If no `tsp` is specified, then the tour length stored in `x` as
#' attribute `"tour_length"` is returned.  If `tsp` is given then the
#' tour length is recalculated using the specified TSP problem.
#'
#' If a distance in the tour is infinite, the result is also infinite. If the
#' tour contains positive and negative infinite distances then the method
#' returns `NA`.
#'
#' @family TOUR
#'
#' @param x a TSP problem or a [TOUR].
#' @param order an object of class `TOUR`
#' @param tsp as TSP object.
#' @param ... further arguments are currently unused.
#' @author Michael Hahsler
#' @keywords optimize
#' @examples
#'
#' data("USCA50")
#'
#' ## original order
#' tour_length(solve_TSP(USCA50, method="identity"))
#'
#' ## length of a manually created (random) tour
#' tour <- TOUR(sample(seq(n_of_cities(USCA50))))
#' tour
#' tour_length(tour)
#' tour_length(tour, USCA50)
#' @export
tour_length <- function(x, ...)
  UseMethod("tour_length")

#' @rdname tour_length
#' @export
tour_length.TSP <- function(x, order, ...) {
  n <- n_of_cities(x)
  if (missing(order))
    order <- 1:n

  .Call(R_tour_length_dist, x, order)
}

#' @rdname tour_length
#' @export
tour_length.ATSP <- function(x, order, ...) {
  n <- n_of_cities(x)
  if (missing(order))
    order <- 1:n

  .Call(R_tour_length_matrix, x, order)
}

### faster for small n but takes O(n^2) memory
#tour_length.ETSP <- function(x, order) tour_length(as.TSP(x), order)

#' @rdname tour_length
#' @export
tour_length.ETSP <- function(x, order, ...) {
  n <- n_of_cities(x)
  if (n != nrow(x))
    stop("x and order do not have the same number of cities!")

  if (missing(order))
    order <- 1:n

  as.numeric(sum(sapply(
    1:(n - 1),
    FUN = function(i)
      dist(x[order[c(i, i + 1)], , drop = FALSE])
  )) +
      dist(x[order[c(n, 1)], , drop = FALSE]))
}

#' @rdname tour_length
#' @export
tour_length.TOUR <- function(x, tsp = NULL, ...) {
  if (is.null(tsp)) {
    len <- attr(x, "tour_length")
    if (is.null(len))
      len <- NA
    return(len)
  }

  tour_length(x = tsp, order = x)
}

#' @rdname tour_length
#' @export
tour_length.integer <- function(x, tsp = NULL, ...) {
  if (is.null(tsp))
    return(NA)

  tour_length(x = tsp, order = x)
}
