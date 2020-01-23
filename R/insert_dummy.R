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
  stop("Dummy cities cannot be used with ETSP! Convert the problem into a TSP.")
