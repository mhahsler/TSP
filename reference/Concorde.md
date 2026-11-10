# Using the Concorde TSP Solver

The Concorde TSP Solver package contains several solvers. Currently,
interfaces to the Concorde solver (Applegate et al. 2001), one of the
most advanced and fastest TSP solvers using branch-and-cut, and the
Chained Lin-Kernighan (Applegate et al. 2003) implementation are
provided in TSP. Concorde can solve
[TSP](https://michael.hahsler.net/TSP/reference/TSP.md)s and
[ETSP](https://michael.hahsler.net/TSP/reference/ETSP.md)s directly.
[ATSP](https://michael.hahsler.net/TSP/reference/ATSP.md)s are
reformulated as larger TSPs and then solved.

## Usage

``` r
concorde_path(path)

concorde_help()

linkern_help()
```

## Arguments

- path:

  a character string with the path to the directory where the
  executables are installed.

## Value

`concorde_path()` returns the path to the executable. The other
functions return nothing.

## Details

Note: Concorde and linkern natively solve
[ETSP](https://michael.hahsler.net/TSP/reference/ETSP.md)s (Euclidean
TSPs given as coordinates). The coordinates are passed on to the solvers
as they are (after scaling to integers, see below), so no distance
matrix needs to be calculated and the memory use grows only linearly
with the number of cities. The other methods in
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
first convert an
[ETSP](https://michael.hahsler.net/TSP/reference/ETSP.md) into a
distance matrix.

## Installation of Concorde

The Concorde TSP Solver is freely available for academic research. It is
not included in the TSP R package and has to be obtained separately from
the [Concorde download
page](https://www.math.uwaterloo.ca/tsp/concorde/downloads/downloads.htm).
Either download the precompiled executables and place them in a suitable
directory (make sure they are executable), or you can get the source
code and compile the program on your own. TSP needs to know where the
executables are. There are two options:

1.  use `concorde_path()` to set the path to the directory containing
    the executables for concorde and linkern, or

2.  make sure that the executables are in the search path stored in the
    `PATH` environment variable (see
    [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html)).

## Using Concorde for [`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)

[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
uses
[`write_TSPLIB()`](https://michael.hahsler.net/TSP/reference/TSPLIB.md)
to write the TSP for Concorde and tries to find the appropriate
`precision` value (digits after the decimal point) to convert the
provided distances into the needed integer value range. The `precision`
value can also be specified in `control` in
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
with method Concorde. Warning messages will alert the user if the
conversion to integer values results in rounding errors that are worse
than what is specified in the `precision` control parameter.

For Euclidean problems
([ETSP](https://michael.hahsler.net/TSP/reference/ETSP.md)), the
coordinates are shifted to start at zero and multiplied by
`10^precision`, because Concorde rounds the calculated distances to
integers. To avoid integer overflows in Concorde, the scaling is reduced
(with a warning) so that no scaled coordinate exceeds \\10^7\\.
[`write_TSPLIB()`](https://michael.hahsler.net/TSP/reference/TSPLIB.md)
gives a warning if cities end up with identical coordinates (e.g.,
because rounding made distinct cities identical, i.e., their distances
became smaller than 1); increase `precision` or rescale the data in this
case.

Concorde and linkern have their own random number generators, which
[`set.seed()`](https://rdrr.io/r/base/Random.html) cannot reach. Their
seed can be set with the optional control parameter `seed`, e.g.,
`control = list(seed = 1234)`. It is passed on to the programs (option
`-s`) and has to be a whole number between 0 and `.Machine$integer.max`.
If no `seed` is specified, one random number is drawn from R's random
number generator and used as the seed, so calling
[`set.seed()`](https://rdrr.io/r/base/Random.html) before
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
also makes these methods reproducible (this advances R's global
random-number stream by one draw).

To get a list of all available command line options which can be used
via the `clo` option for `solve_TSP` use `concorde_help()` and
`linkern_help()`. Several options (-x, -o, -N, -Q) are not available via
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
since they are used by the interface.

If Concorde takes too long, then you can interrupt
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
using `Esc/CTRL-C`. On most operating systems, this will also terminate
the Concorde executable. If Concorde keeps running, then you can kill
the 'concorde' process via your operating system.

## References

Concorde home page, <https://www.math.uwaterloo.ca/tsp/concorde/>

David Applegate, Robert Bixby, Vasek Chvatal, William Cook (2001): TSP
cuts which do not conform to the template paradigm, Computational
Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer-Verlag.

David Applegate and William Cook and Andre Rohe (2003): Chained
Lin-Kernighan for Large Traveling Salesman Problems, *INFORMS Journal on
Computing*, **15**, 82–92.

## See also

Other solver:
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)

## Author

Michael Hahsler

## Examples

``` r

if (FALSE) { # \dontrun{
## see if Concorde is correctly installed
concorde_path()


## set path to the Concorde executable if it is not in the search PATH
## Example:
## concorde_path("~/concorde/")

concorde_help()

data("USCA312")

## run Concorde in verbose mode (-v) with fast cuts only (-V)
## Note: use the control parameter verbose = FALSE to suppress Concorde's output
set.seed(1234)
solve_TSP(USCA312, method = "concorde", control = list(clo = "-v -V"))

## Concorde and linkern can directly solve Euclidean TSPs (ETSP). The
## coordinates are used directly, so no large distance matrix is calculated.
etsp <- ETSP(data.frame(x = runif(20), y = runif(20)))
tour <- solve_TSP(etsp, method = "concorde", control = list(verbose = FALSE))
tour
plot(etsp, tour)
} # }
```
