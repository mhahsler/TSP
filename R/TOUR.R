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
as.TOUR <- function(object) UseMethod("as.TOUR")
as.TOUR.numeric <-  function(object){
  l <- labels(object)	    ### preserve lables
  object <- as.integer(object)
  names(object) <- l
  as.TOUR(object)
}
as.TOUR.integer <- function(object){
  
  ## check tour
  if(any(object < 1) || any(object > length(object)) || any(is.na(object))) 
    stop("tour contains illegal elements.")
  
  if(any(duplicated(object))) stop("tour indices are not unique.")
  
  class(object) <- c("TOUR", class(object))
  object
}



## helper for tour

print.TOUR <- function(x, ...){
  
  cat("object of class", sQuote(class(x)[1]), "\n")
  cat("result of method", sQuote(attr(x, "method")), "for", 
    length(x), "cities\n")
  if(!is.null(attr(x, "tour_length")))
    cat("tour length:", attr(x, "tour_length"), "\n")
  else 
    cat("tour length: unknown\n")
}
