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



## simulated annealing


# Subtour reversal
# FIXME: when i>j then we should revert the beginning 
# and the end of the tour instead!
tsp_SA_reversal <- function(tour, x) {
  n <- length(tour)
  # Pick two random distinct positions
  pos <- sample(1:n, 2, replace = FALSE)
  i <- pos[1]
  j <- pos[2]
  
  # Swap the cities at these positions
  new_tour <- tour
  new_tour[i:j] <- rev(tour[i:j])
  return(new_tour)
}

# A simple swap operator is commonly used for TSP
tsp_SA_swap <- function(tour, x) {
  n <- length(tour)
  # Pick two random distinct positions
  pos <- sample(1:n, 2, replace = FALSE)
  i <- pos[1]
  j <- pos[2]
  
  # Swap the cities at these positions
  new_tour <- tour
  new_tour[i] <- tour[j]
  new_tour[j] <- tour[i]
  return(new_tour)
}

tsp_SA_mixed <- function(tour, x) {
  if (stats::runif(1) > .5) tsp_SA_reversal(tour, x)
  else tsp_SA_swap(tour, x)
}

tsp_SA <- function(x, control = NULL){

  control <- .get_parameters(control, list(
    tour = NULL,
    local_move = tsp_SA_reversal,
    temp = NULL, # Initial temperature
    tmax = 10, # number of evaluations per temperature step
    maxit = 10000, # Number of iterations 
    trace = 0
  ), method = "SA")

  initial_tour <- as.TOUR(control$tour %||% sample(n_of_cities(x)))
  control$temp <- control$temp %||% 
    tour_length(x, initial_tour) / n_of_cities(x)
  
  cost_function <- function(tour, x) tour_length(x, as.integer(tour))
    
  ret <- stats::optim(
    par = initial_tour,
    fn = cost_function,
    gr = control$local_move,
    method = "SANN",
    x = x, # Pass additional argument to fn and gr
    control = list(
      temp = control$temp,
      tmax = control$tmax,
      maxit = control$maxit,
      trace = control$trace
    )
  )

  TOUR(as.integer(ret$par), "sa", x)
}
