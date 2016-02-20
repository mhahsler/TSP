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



## nearest neighbor algorithm

tsp_nn <- function(x, control = NULL) {
    ## parameter x comes checked from solve_TSP/solve_ATSP

    n <- n_of_cities(x)

    ## we use a matrix for now (coveres TSP and ATSP)
    x <- as.matrix(x)

    control <- .get_parameters(control, list(
      start = sample(n, 1)
    ))
    start <- control$start
    if(start < 0 || start > n)
      stop(paste("illegal value for", sQuote("start")))

    placed <- logical(n)
    order <- integer(n)

    ## place first city
    current <- start
    order[1L] <- current
    placed[current] <- TRUE

    while(length(rest <- which(!placed)) > 0L) {
        ## nearest <- rest[which.min(x[current,rest])]
        ## which.min has problems with Inf
        ## so we can break ties randomly now too
        x_sub <- x[current, rest]
        current <- rest[which(x_sub == min(x_sub, na.rm = TRUE))]
        if(length(current) > 1L) current <- sample(current, 1)

        ## place city
        order[n + 1L - length(rest)] <- current
        placed[current] <- TRUE
    }

    order
}

## repetitive NN

tsp_repetitive_nn <- function(x, control){
  n <- n_of_cities(x)

  #tours <- lapply(1:n, function(i) tsp_nn(x, control = list(start = i)))
  ## no backend would warn!
  i <- 0L ## for R CMD check (no global binding for i)
  suppressWarnings(
    tours <- foreach(i = 1:n) %dopar% tsp_nn(x, control = list(start = i))
  )

  lengths <- sapply(tours, FUN = function(i) tour_length(x, i))

  tours[[which.min(lengths)]]
}
