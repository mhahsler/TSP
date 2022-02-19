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

#' Class TOUR -- Solution to a traveling salesperson problem
#'
#' Class to store the solution of a TSP. Objects of this class are returned by
#' TSP solvers in this package.  Essentially, an object of class `TOUR` is
#' a permutation vector containing the order of cities to visit.
#'
#' Since an object of class `TOUR` is an integer vector, it can be
#' subsetted as an ordinary vector or coerced to an integer vector using
#' `as.integer()`. It also contains the names of the objects as labels.
#' Additionally, `TOUR` has the following attributes: `"method"`,
#' `"tour_length"`.
#'
#' For most functions, e.g., [tour_length()] or [image.TSP()], the
#' `TSP/ATSP` object used to find the tour is still needed, since the tour
#' does not contain the distance information.
#'
#' @family TOUR
#'
#' @param x an integer permutation vector or, for the methods an object of
#' class [TOUR].
#' @param object data (an integer vector) which can be coerced to `TOUR`.
#' @param method character string; method used to create the tour.
#' @param tsp `TSP` object the tour applies to. If available then the tour
#' will include the tour length. Also the labels of the cities will be
#' available in the tour (otherwise the labels of `x` are used).
#' @param ... further arguments are passed on.
#' @author Michael Hahsler
#' @keywords classes
#' @examples
#' TOUR(1:10)
#'
#' ## calculate a tour
#' data("USCA50")
#' tour <- solve_TSP(USCA50)
#' tour
#'
#' ## get permutation vector
#' as.integer(tour)
#'
#' ## get tour length directly from tour
#' attr(tour, "tour_length")
#'
#' ## show labels
#' labels(tour)
#' @export
TOUR <- function(x, method=NA, tsp=NULL){
  if(inherits(x, "TOUR")) return(x)

  x <- as.TOUR(x)
  attr(x, "method") <- as.character(method)
  if(!is.null(tsp)){
    attr(x, "tour_length") <- tour_length(x, tsp)
    names(x) <- labels(tsp)[x]
  }

  x
}

## coercion
#' @rdname TOUR
#' @export
as.TOUR <- function(object) UseMethod("as.TOUR")

#' @rdname TOUR
#' @export
as.TOUR.numeric <-  function(object){
  l <- labels(object)	    ### preserve lables
  object <- as.integer(object)
  names(object) <- l
  as.TOUR(object)
}

#' @rdname TOUR
#' @export
as.TOUR.integer <- function(object){

  ## check tour
  if(any(object < 1) || any(object > length(object)) || any(is.na(object)))
    stop("tour contains illegal elements.")

  if(any(duplicated(object))) stop("tour indices are not unique.")

  class(object) <- c("TOUR", class(object))
  object
}


#' @rdname TOUR
#' @export
print.TOUR <- function(x, ...){

  cat("object of class", sQuote(class(x)[1]), "\n")
  cat("result of method", sQuote(attr(x, "method")), "for",
    length(x), "cities\n")
  if(!is.null(attr(x, "tour_length")))
    cat("tour length:", attr(x, "tour_length"), "\n")
  else
    cat("tour length: unknown\n")
}
