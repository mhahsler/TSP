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



## Insertion algorithms
## (Rosenkrantz et al. 1977)

tsp_insertion <- function(x, type = "nearest", control = NULL){

  ## since sample has an annoying convenience feature for
  ## lenght(x) == 1
  choose1 <- function(x) if(length(x) > 1) sample(x, 1) else x

  ## this is slower than which.min and which.max but works also
  ## correctly for only values Inf in x and breaks ties randomly
  choose1_min <- function(x) choose1(which(x == min(x)))
  choose1_max <- function(x) choose1(which(x == max(x)))

  types <- c("nearest", "farthest", "cheapest", "arbitrary")
  type_num <- pmatch(type, types)
  if(is.na(type_num)) stop(paste("Unknown insertion type: ", sQuote(type)))

  ## x comes checked form solve_TSP/solve_ATSP
  n <- n_of_cities(x)

  ## we use a matrix for now (covers TSP and ATSP)
  asym <- inherits(x, "ATSP")
  x <- as.matrix(x)

  ## place first city
  control <- .get_parameters(control, list(
      start = sample(n, 1)
    ), method = paste0(types[type_num], "_insertion"))
  start <- as.integer(control$start)
  if(start < 0 || start > n)
    stop(paste("illegal value for", sQuote("start")))

  placed <- logical(n)
  placed[start] <- TRUE
  order <- c(start)

  ## place other cities
  while(any(placed == FALSE)) {

    ## find city to be inserted
    ks <- which(!placed)
    js <- which(placed)

    ## nearest / farthest
    if(type_num < 3) {
      m <- x[ks,js, drop = FALSE]

      ## for the asymmetric case we have to take distances
      ## from and to the city into account
      if(asym){
        m <- cbind(m, t(x)[ks,js, drop = FALSE])
      }

      ds <- sapply(1:length(ks), FUN =
          function(i)  min(m[i, , drop = FALSE]))

      ## nearest/farthest insertion
      winner_index <- if(type_num == 1) choose1_min(ds)
      else choose1_max(ds)

      k <- ks[winner_index]
    }

    ## cheapest
    else if(type_num == 3) {
      winner_index <- choose1_min(sapply(ks, FUN =
          function(k) min(.Call(R_insertion_cost, x, order, k)) ))
      k <- ks[winner_index]

      ## we look for the optimal insertion place for k again later
      ## this is not necessary, but it is more convenient
      ## to reuse the code for the other insertion algorithms for now.
    }

    ## random
    else if(type_num == 4) k <- choose1(ks)

    ## just in case
    else stop("unknown insertion type")

    ## do insertion
    placed[k] <- TRUE

    if(length(order) == 1) order <- append(order, k)
    else {
      pos <- choose1_min(.Call(R_insertion_cost, x, order, k))
      order <- append(order, k, after = pos)
    }
  }

  if (control$verbose)
    cat("All cities placed.\n\n")

  order
}

### faster arbitrary insertion (random sampling takes care of breaking ties)
tsp_insertion_arbitrary <- function(x, control = NULL){
  ## x comes checked form solve_TSP/solve_ATSP
  n <- n_of_cities(x)

  control <- .get_parameters(control, list(), method = "arbitrary_insertion")

  ## we use a matrix for now (covers TSP and ATSP)
  x <- as.matrix(x)

  ## deal with special cases
  if(nrow(x) == 1) return(1L)
  if(nrow(x) == 2) return(sample(1:2))

  x[is.na(x)] <- Inf

  ## random order
  rorder <- sample(n)
  x <- x[rorder, rorder]

  ## FIXME: specify start city

  ## place first two cities
  order <- integer(n)
  order[1:2] <- 1:2

  ## place other cities
  for(i in 3:n) {
    pos <- which.min(.Call(R_insertion_cost, x, order[1:(i-1L)], i)) + 1L
    order[((pos):i)+1L] <- order[(pos):i]
    order[pos] <- i
  }

  if (control$verbose)
    cat("All cities placed.\n\n")

  rorder[order]
}

