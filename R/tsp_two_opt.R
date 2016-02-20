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



## heuristic to improve a tour using exchanges of 2 edges.

tsp_two_opt <- function(x, control = NULL){

  control <- .get_parameters(control, list(
    tour = NULL,
    rep = 1
  ))

  ## improve a given tour or create a random tour
  ## we use a function since for rep >1 we want several random
  ## initial tours
  initial <- function() {
    if(!is.null(control$tour)) as.integer(control$tour)
    else sample(n_of_cities(x))
  }

  xx <- as.matrix(x)

  if(control$rep > 1) {
    tour <- replicate(control$rep, .Call("two_opt", xx, initial(),
      PACKAGE="TSP"), simplify = FALSE)
    lengths <- sapply(tour, FUN = function(t) tour_length(x, t))
    tour <- tour[[which.min(lengths)]]
  }else tour <- .Call("two_opt", xx, initial(), PACKAGE="TSP")

  tour
}
