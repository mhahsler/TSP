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

tsp_identity <- function(x, control) {
  control <- .get_parameters(control, list(), method = "identity")
  seq_len(n_of_cities(x))
}


tsp_random <- function(x, control) {
  control <- .get_parameters(control, list(), method = "random")
  sample.int(n_of_cities(x))
}