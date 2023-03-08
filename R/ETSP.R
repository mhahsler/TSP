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


#' Class ETSP -- Euclidean traveling salesperson problem
#'
#' Constructor to create an instance of a Euclidean traveling salesperson
#' problem (TSP) represented by city coordinates and some auxiliary methods.
#'
#' Objects of class `ETSP` are internally represented as a `matrix`
#' objects (use `as.matrix()` to get the `matrix` object).
#'
#' @family TSP
#'
#' @param x,object an object (data.frame or matrix) to be converted into a
#' `ETSP` or, for the methods, an object of class `ETSP`.
#' @param labels optional city labels. If not given, labels are taken from
#' `x`.
#' @param col color scheme for image.
#' @param order order of cities for the image as an integer vector or an object
#' of class [TOUR].
#' @param tour,y a tour to be visualized.
#' @param tour_lty,tour_col line type and color for tour.
#' @param labels logical; plot city labels.
#' @param ... further arguments are passed on.
#' @returns
#' - `ETSP()` returns `x` as an object of class `ETSP`.
#' - `n_of_cities()` returns the number of cities in `x`.
#' - `labels()` returns a vector with the names of the cities in `x`.
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' ## create a random ETSP
#' n <- 20
#' x <- data.frame(x = runif(n), y = runif(n), row.names = LETTERS[1:n])
#' etsp <- ETSP(x)
#' etsp
#'
#' ## use some methods
#' n_of_cities(etsp)
#' labels(etsp)
#'
#' ## plot ETSP and solution
#' tour <- solve_TSP(etsp)
#' tour
#'
#' plot(etsp, tour, tour_col = "red")
#' @export
ETSP <- function(x, labels = NULL) {
  if(inherits(x, "ETSP")) return(x)
  x <- as.ETSP(x)
  if(!is.null(labels)) rownames(x) <- labels
  x
}

## coercion
#' @rdname ETSP
#' @export
as.ETSP <- function(x) UseMethod("as.ETSP")


#' @rdname ETSP
#' @export
as.ETSP.matrix <- function(x){
  mode(x) <- "numeric"
  if(is.null(rownames(x))) rownames(x) <- 1:nrow(x)

  class(x) <- c("ETSP", class(x))
  x
}

#' @rdname ETSP
#' @export
as.ETSP.data.frame <- function(x){
  as.ETSP(as.matrix(x))
}

#' @rdname ETSP
#' @export
as.TSP.ETSP <- function(x) TSP(dist(x))

#' @rdname ETSP
#' @export
as.matrix.ETSP <- function(x, ...) {
  unclass(x)
}

## print
#' @rdname ETSP
#' @export
print.ETSP <- function(x, ...) {
  cat("object of class", sQuote(class(x)[1]), "\n")
  cat(n_of_cities(x), "cities", "(Euclidean TSP)\n")
}


## number of cities
#' @rdname ETSP
#' @export
n_of_cities.ETSP <- function(x) nrow(x)

## labels
#' @rdname ETSP
#' @export
labels.ETSP <- function(object, ...) rownames(object)

## image
#' @rdname ETSP
#' @export
image.ETSP <- function(x, order, col = gray.colors(64), ...) {
  p <- n_of_cities(x)
  if(missing(order)) order <- 1:p

  x <- as.TSP(x)

  graphics::image.default(1:p, 1:p, as.matrix(x)[order, order],
    col = col, ...)
}

#' @rdname ETSP
#' @importFrom graphics text
#' @export
plot.ETSP <- function(x, y = NULL, tour = NULL, tour_lty = 2, tour_col = 2, labels = TRUE, ...) {
  x <- as.matrix(x)
  plot(x, y = NULL, ...)
  if(!is.null(y)) tour <- TOUR(y)
  if(!is.null(tour)) polygon(x[tour,], lty = tour_lty, border = tour_col)
  if(labels) text(x, y = NULL, labels = rownames(x), pos = 3)
}
