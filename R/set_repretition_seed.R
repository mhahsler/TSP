#######################################################################
# Set repetition Seeds
# Copyright (C) 2011-2016 Michael Hahsler
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


## Use a separate deterministic seed for each foreach iteration. This avoids
## relying on the RNG behavior of the registered foreach backend.
.set_repetition_seed <- function(seed, repetition) {
  if (!is.null(seed)) {
    iteration_seed <- (as.double(seed) + repetition - 1) %%
      (.Machine$integer.max + 1)
    set.seed(iteration_seed)
  }
}