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

## generic
tour_length <- function(x, ...) UseMethod("tour_length")

tour_length.TOUR <- function(x, tsp = NULL, ...) {
  if(is.null(tsp)) { 
    len <- attr(x, "tour_length")
    if(is.null(len)) len <- NA
    return(len)
  }
    
  tour_length(x = tsp, order = x)
}

tour_length.TSP <- function(x, order, ...) {
  
  n <- n_of_cities(x)
  if(missing(order)) order <- 1:n
  
  .Call("tour_length_dist", x, order, PACKAGE="TSP")
}

tour_length.ATSP <- function(x, order, ...) {
  
  n <- n_of_cities(x)
  if(missing(order)) order <- 1:n
  
  .Call("tour_length_matrix", x, order, PACKAGE="TSP")
}


tour_length.ETSP <- function(x, order, ...) {
  n <- n_of_cities(x)
  if(n != nrow(x)) stop("x and order do not have the same number of cities!")
  
  if(missing(order)) order <- 1:n
  
  tl <- 0
  for(i in 1:(n-1)) {
    tl <- tl + dist(x[order[i:(i+1)],])
  }
  
  tl <- tl + dist(rbind(x[order[n]], x[order[1]]))
  
  as.numeric(tl)
}

### faster for small n but takes O(n^2) memory
#tour_length.ETSP <- function(x, order) tour_length(as.TSP(x), order)