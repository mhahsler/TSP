#######################################################################
# Run code with a given random number seed
# Copyright (C) 2011-2026 Michael Hahsler
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


## Repetitions are run with foreach. To make them reproducible with set.seed()
## on the main R process independent of the registered backend and the number of
## workers, the main process draws one seed per repetition from its own RNG
## (.draw_repetition_seeds()) and each iteration runs with its own seed
## (.with_rng_seed()).

## draw n seeds from the RNG of the main process (advances the global stream)
.draw_repetition_seeds <- function(n) {
  list(seeds = sample.int(.Machine$integer.max, n), kind = RNGkind())
}

## Evaluate expr with the RNG seeded with seed. The RNG kind is set to the kind
## used by the main process (workers may use different defaults). The previous
## RNG state is restored afterwards, so a sequential backend (where iterations
## run in the main process) does not change the global RNG state.
.with_rng_seed <- function(seed, kind, expr) {
  env <- globalenv()
  old <- if (exists(".Random.seed", envir = env, inherits = FALSE))
    get(".Random.seed", envir = env, inherits = FALSE)
  else
    NULL

  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = env, inherits = FALSE))
        rm(".Random.seed", envir = env)
    } else
      assign(".Random.seed", old, envir = env)
  }, add = TRUE)

  set.seed(seed, kind = kind[1], normal.kind = kind[2], sample.kind = kind[3])
  expr
}
