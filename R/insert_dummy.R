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
insert_dummy <- function(x, n = 1, const = 0, inf = Inf, label = "dummy") 
  UseMethod("insert_dummy")


## TODO: if n > 1 the distances between the cities have to be inf...
insert_dummy.TSP.old <- function(x, n = 1, const = 0, inf = Inf, 
    label = "dummy") {
    
    if(n < 1) stop(paste(sQuote("n"),"has to be >1"))
    
    n <- as.integer(n)
    p <- n_of_cities(x)

    d <- c(rep(const, sum(p:(p+n-1))), x)
    p <- p + n
    
    ## place inf between dummies
    ## access a dist object for i < j <= p is
    ## dist[p*(i-1) - i*(i-1)/2 + j-i]
    if(n>1) {
        for(i in 1:(n-1)) {
            for(j in (i+1):n) {
                d[p*(i-1) - i*(i-1)/2 + j-i] <- inf
            }
        }
    }    
    
    attributes(d) <- attributes(x)
    attr(d, "Size") <- p
    ## the following line has problems with R-devel
    attr(d, "Labels") <- c(rep("dummy", n), attr(x, "Labels"))
    d
}

## use insert dummy from ATSP
insert_dummy.TSP <- function(x, n = 1, const = 0, inf = Inf, label = "dummy") {
    x <- insert_dummy(ATSP(x), n, const, inf, label)
    TSP(x)
}

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

insert_dummy.ETSP <- function(x, n = 1, const = 0, inf = Inf, label = "dummy") 
  stop("Dummy cities cannot be used with ETSP!")