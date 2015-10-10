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



## create a Euclidean TSP problem
ETSP <- function(x, labels = NULL) {
  if(inherits(x, "ETSP")) return(x)
  x <- as.ETSP(x)
  if(!is.null(labels)) rownames(x) <- labels
  x
}

## coercion
as.ETSP <- function(x) UseMethod("as.ETSP")
as.ETSP.matrix <- function(x){
  mode(x) <- "numeric"
  if(is.null(rownames(x))) rownames(x) <- 1:nrow(x)
  
  class(x) <- c("ETSP", class(x))
  x
}

as.ETSP.data.frame <- function(x){
  as.ETSP(as.matrix(x))
}

as.TSP.ETSP <- function(x) TSP(dist(x))

as.matrix.ETSP <- function(x, ...) {
  unclass(x)
}

## print
print.ETSP <- function(x, ...) {
  cat("object of class", sQuote(class(x)[1]), "\n")
  cat(n_of_cities(x), "cities", "(Euclidean TSP)\n")
}


## number of cities
n_of_cities.ETSP <- function(x) nrow(x)

## labels
labels.ETSP <- function(object, ...) rownames(object)

## image
image.ETSP <- function(x, order, col = gray.colors(64), ...) {
  p <- n_of_cities(x)
  if(missing(order)) order <- 1:p
  
  x <- as.TSP(x)
  
  graphics::image.default(1:p, 1:p, as.matrix(x)[order, order], 
    col = col, ...)
}

plot.ETSP <- function(x, y = NULL, tour = NULL, tour_lty = 2, tour_col = 1,...) {
  x <- as.matrix(x)
  plot(x, y = NULL, ...)
  if(!is.null(y)) tour <- TOUR(y)
  if(!is.null(tour)) polygon(x[tour,], lty = tour_lty, border = tour_col)
}
