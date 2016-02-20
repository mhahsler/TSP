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
solve_TSP <- function(x, method = NULL, control = NULL, ...)
  UseMethod("solve_TSP")

## TSP
solve_TSP.TSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(x, method, control, ...)
}

## ATSP
solve_TSP.ATSP <- function(x, method = NULL, control = NULL, ...) {
  .solve_TSP(x, method, control, ...)
}

## ETSP
solve_TSP.ETSP <- function(x, method = NULL, control = NULL, ...) {

  ## all but concorde and linkern can only do TSP
  m <- pmatch(tolower(method), c("concorde", "linkern"))
  if(length(m) == 0L || is.na(m)) x <- as.TSP(x)

  .solve_TSP(x, method, control, ...)
}



## Deal with Inf: punish (-)Inf with max (min) +(-) 2*range
.replaceInf <- function(x, pInf = NULL, nInf = NULL) {
  if(any(is.infinite(x))) {
    range_x <- range(x, na.rm = TRUE, finite = TRUE)
    if(is.null(pInf)) pInf <- range_x[2] + 2* diff(range_x)
    if(is.null(nInf)) nInf <- range_x[1] - 2* diff(range_x)
    x[x == Inf] <- pInf
    x[x == -Inf] <- nInf
  }
  x
}

## workhorse
.solve_TSP <- function(x, method = NULL, control = NULL, ...) {

  ## add ... to control
  control <- c(control, list(...))

  ## methods
  methods <- c(
    "identity",
    "random",
    "nearest_insertion",
    "farthest_insertion",
    "cheapest_insertion",
    "arbitrary_insertion",
    "nn",
    "repetitive_nn",
    "2-opt", ### deprecated
    "two_opt",
    "concorde",
    "linkern"
  )

  ## default is arbitrary_insertion + two_opt
  if(is.null(method)) {
    method <- "arbitrary_insertion"
    if(is.null(control$two_opt))
      control <- c(control, list(two_opt = TRUE))
  } else method <- match.arg(tolower(method), methods)


  ## check for NAs
  if(any(is.na(x))) stop("NAs not allowed!")

  ## Inf
  x_ <- .replaceInf(x)

  ## work horses
  .solve_TSP_worker <- function() {
    order <- switch(method,
      identity = seq(n_of_cities(x_)),
      random = sample(n_of_cities(x_)),
      nearest_insertion = tsp_insertion(x_, type = "nearest", control = control),
      farthest_insertion = tsp_insertion(x_, type = "farthest", control = control),
      cheapest_insertion = tsp_insertion(x_, type = "cheapest", control = control),
      #      arbitrary_insertion = tsp_insertion(x_, type = "arbitrary", control = control),
      arbitrary_insertion = tsp_insertion_arbitrary(x_, control = control),
      nn = tsp_nn(x_, control = control),
      repetitive_nn = tsp_repetitive_nn(x_, control = control),
      two_opt = tsp_two_opt(x_, control = control),
      '2-opt' = tsp_two_opt(x_, control = control),
      concorde = tsp_concorde(x_, control = control),
      linkern = tsp_linkern(x_, control = control)
    )

    ### do refinement two_opt
    if(!is.null(control$two_opt) && control$two_opt) {
      order <- tsp_two_opt(x_, control = c(control, list(tour = order)))
      method <- paste(method , "+two_opt", sep = "")
    }

    TOUR(order, method=method, tsp=x)
  }

  ## do rep?
  if(!is.null(control$rep)) n <- control$rep
  else n <- 1L

  if(method == "concorde" || method == "linkern") {
    n <- 1L
    control$two_opt <- NULL ## no two_opt for these!
  }
  if(method == "repetitive_nn") n <- 1L

  if(n==1L) return(.solve_TSP_worker())

  #l <- replicate(n, .solve_TSP_worker(), simplify = FALSE)
  l <- foreach(i = 1:n) %dopar% .solve_TSP_worker()


  l <- l[[which.min(sapply(l, attr, "tour_length"))]]
  attr(l, "method") <- paste(attr(l, "method"), "_rep_", n, sep="")
  return(l)
}
