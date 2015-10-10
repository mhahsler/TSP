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



## create a TSP form an ATSP by doubling the cities

reformulate_ATSP_as_TSP <- function(x, infeasible = Inf, cheap = -Inf) {
    if(!is(x, "ATSP")) stop("x is not an ATSP object!")
    
    m <- as.matrix(x)
    
    ## scale matrix and add cheap links
    diag(m) <- cheap

    tsp <- rbind(
        cbind(matrix(infeasible, ncol = ncol(m), nrow = nrow(m)), t(m)),
        cbind(m, matrix(infeasible, ncol = nrow(m), nrow = ncol(m)))
    )

    ## create labels (* for virtual cities)
    lab <- c(labels(x), paste(labels(x), "*", sep = ""))
    dimnames(tsp) <- list(lab, lab)
    
    ## return as TSP
    TSP(tsp)
}

