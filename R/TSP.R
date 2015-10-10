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



## create a TSP problem
TSP <- function(x, labels = NULL) {
    if(inherits(x, "TSP")) return(x)
    x <- as.TSP(x)
    if(!is.null(labels)) attr(x, "Labels") <- labels
    x
}

## coercion
as.TSP <- function(x) UseMethod("as.TSP")
as.TSP.dist <- function(x){
    ## make sure we have a upper triangle matrix w/o diagonal
    x <- as.dist(x, diag = FALSE, upper = FALSE)
    
    ## make sure we have labels
    if(is.null(attr(x, "Lables"))) attr(x, "Lables") <- c(1:n_of_cities(x))

    if(any(is.nan(x))) stop(paste(sQuote("NAs"), "not supported"))
    
    ## make sure data is numeric
    mode(x) <- "numeric"
    class(x) <- c("TSP", class(x))
    x
}

as.TSP.matrix <- function(x){
    if(!isSymmetric(x)) stop("TSP requires a symmetric matrix")

    method <- attr(x, "method")
    x <- as.dist(x, diag = FALSE, upper = FALSE)
    attr(x, "method") <- method
    
    ## make sure we have labels
    if(is.null(attr(x, "Lables"))) 
    attr(x, "Lables") <- c(1:n_of_cities(x))

    if(any(is.nan(x))) stop(paste(sQuote("NAs"), "not supported"))
    
    ## make sure data is numeric
    mode(x) <- "numeric"
    class(x) <- c("TSP", class(x))
    x
}

as.dist.TSP <- function(m, ...) {
  class(m) <- "dist"
  m
}
  

## print
print.TSP <- function(x, ...) {
    method <- attr(x, "method")
    if(is.null(method)) method <- "unknown"
    
    cat("object of class", sQuote(class(x)[1]), "\n")
    cat(n_of_cities(x), "cities", 
        paste("(distance ", sQuote(method),")", sep=""), "\n")
}


## number of cities
n_of_cities.TSP <- function(x) attr(x, "Size")

## generic for n_of_cities
n_of_cities <- function(x) UseMethod("n_of_cities")
n_of_cities.default <- n_of_cities.TSP

## labels
labels.TSP <- function(object, ...) attr(object, "Labels")

## image
image.TSP <- function(x, order, col = gray.colors(64), ...) {
    p <- n_of_cities(x)
    if(missing(order)) order <- 1:p
    
    graphics::image.default(1:p, 1:p, as.matrix(x)[order, order], 
			    col = col, ...)
}
