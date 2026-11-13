#######################################################################
# TSP - Traveling Salesperson Problem
# Copyright (C) 2011 Michael Hahsler and Kurt Hornik
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


## heuristic to improve a tour using exchanges of 3 edges. For asymmetric
## problems only the reconnection without reversed segments is used.

tsp_three_opt <- function(x, control = NULL){

  control <- .get_parameters(control, list(
    tour = NULL
  ), method = "three_opt")

  ## improve a given tour or start from a random tour
  tour <- if (!is.null(control$tour)) as.integer(control$tour)
    else sample(n_of_cities(x))

  tour <- .Call(R_three_opt, as.matrix(x), tour, !inherits(x, "ATSP"))

  if (control$verbose)
    cat("Three-opt done.\n\n")

  tour
}
