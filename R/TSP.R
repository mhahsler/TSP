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


#' Class TSP -- Symmetric traveling salesperson problem
#'
#' Constructor to create an instance of a symmetric traveling salesperson
#' problem (TSP) and some auxiliary methods.
#'
#' Objects of class `TSP` are internally represented as `dist`
#' objects (use [as.dist()] to get the `dist` object).
#'
#' Not permissible paths can be set to a distance of `+Inf`. `NA`s are not allowed and `-Inf` will lead
#' to the algorithm only being able to find an admissible tour, but not the best one.
#'
#' @family TSP
#'
#' @param x,object an object (currently `dist` or a symmetric matrix) to
#' be converted into a `TSP` or, for the methods, an object of class
#' `TSP`.
#' @param labels optional city labels. If not given, labels are taken from
#' `x`.
#' @param method optional name of the distance metric. If `x` is a
#' `dist` object, then the method is taken from that object.
#' @param col color scheme for image.
#' @param order order of cities for the image as an integer vector or an object
#' of class [TOUR].
#' @param ... further arguments are passed on.
#' @returns
#' - `TSP()` returns `x` as an object of class `TSP`.
#' - `n_of_cities()` returns the number of cities in `x`.
#' - `labels()` returns a vector with the names of the cities in `x`.
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' data("iris")
#' d <- dist(iris[-5])
#'
#' ## create a TSP
#' tsp <- TSP(d)
#' tsp
#'
#' ## use some methods
#' n_of_cities(tsp)
#' labels(tsp)
#' image(tsp)
#' @export
TSP <- function(x, labels = NULL, method = NULL) {
  if (inherits(x, "TSP"))
    return(x)
  x <- as.TSP(x)
  if (!is.null(labels))
    attr(x, "Labels") <- labels
  if (!is.null(method))
    attr(x, "method") <- method
  x
}

## coercion
#' @rdname TSP
#' @export
as.TSP <- function(x)
  UseMethod("as.TSP")

#' @rdname TSP
#' @export
as.TSP.dist <- function(x) {
  ## make sure we have a upper triangle matrix w/o diagonal
  x <- as.dist(x, diag = FALSE, upper = FALSE)

  ## make sure we have labels
  if (is.null(attr(x, "Labels")))
    attr(x, "Labels") <- c(1:n_of_cities(x))

  if (any(is.nan(x)))
    stop(paste(sQuote("NAs"), "not supported"))

  ## make sure data is numeric
  mode(x) <- "numeric"
  class(x) <- c("TSP", class(x))
  x
}

#' @rdname TSP
#' @export
as.TSP.matrix <- function(x) {
  if (!isSymmetric(x))
    stop("TSP requires a symmetric matrix")

  method <- attr(x, "method")
  x <- as.dist(x, diag = FALSE, upper = FALSE)
  attr(x, "method") <- method

  ## make sure we have labels
  if (is.null(attr(x, "Labels")))
    attr(x, "Labels") <- c(1:n_of_cities(x))

  if (any(is.nan(x)))
    stop(paste(sQuote("NAs"), "not supported"))

  ## make sure data is numeric
  mode(x) <- "numeric"
  class(x) <- c("TSP", class(x))
  x
}

#' @rdname TSP
#' @param m a TSP object to be converted to a [dist] object.
#' @export
as.dist.TSP <- function(m, ...) {
  class(m) <- "dist"
  as.dist(m, ...)
}


## print
#' @rdname TSP
#' @export
print.TSP <- function(x, ...) {
  method <- attr(x, "method")
  if (is.null(method))
    method <- "unknown"

  cat("object of class", sQuote(class(x)[1]), "\n")
  cat(n_of_cities(x),
    "cities",
    paste("(distance ", sQuote(method), ")", sep = ""),
    "\n")
}



## generic for n_of_cities
#' @rdname TSP
#' @export
n_of_cities <- function(x)
  UseMethod("n_of_cities")

## number of cities
#' @rdname TSP
#' @export
n_of_cities.TSP <- function(x)
  attr(x, "Size")

#' @export
n_of_cities.default <- n_of_cities.TSP

## labels
#' @rdname TSP
#' @export
labels.TSP <- function(object, ...)
  attr(object, "Labels")

## image
#' @rdname TSP
#' @export
image.TSP <- function(x, order, col = gray.colors(64), ...) {
  p <- n_of_cities(x)
  if (missing(order))
    order <- 1:p

  graphics::image.default(1:p, 1:p, as.matrix(x)[order, order],
    col = col, ...)
}
