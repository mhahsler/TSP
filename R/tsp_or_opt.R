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


## heuristic to improve a tour by moving segments of up to max_segment cities
## (Or 1976). Segments are only reversed for symmetric problems.

tsp_or_opt <- function(x, control = NULL){

  control <- .get_parameters(control, list(
    tour = NULL,
    max_segment = 3L
  ), method = "or_opt")

  .validate_scalar(control$max_segment, "max_segment", integer = TRUE, lower = 1)

  ## improve a given tour or start from a random tour
  tour <- if (!is.null(control$tour)) as.integer(control$tour)
    else sample(n_of_cities(x))

  tour <- .Call(R_or_opt, as.matrix(x), tour, as.integer(control$max_segment),
    !inherits(x, "ATSP"))

  if (control$verbose)
    cat("Or-opt done.\n\n")

  tour
}
