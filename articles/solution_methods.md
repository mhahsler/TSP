# TSP solution methods

This vignette uses the R package **TSP** (Hahsler and Hornik 2007).
Please cite the package and the accompanying Journal of Statistical
Software paper (see `citation("TSP")`) when you use it in your work.

Finding the shortest tour is NP-hard, so the number of tours that would
have to be checked for an exact solution grows factorially with the
number of cities. For this reason, **TSP** offers fast heuristics that
find good (but not necessarily optimal) tours and an interface to the
exact solver Concorde. This vignette describes the implemented methods
and shows how to choose and compare them. All methods are called through
the same function:

``` r

solve_TSP(x, method, control, ...)
```

where `x` is a `TSP`, `ATSP` or `ETSP` problem, `method` selects the
algorithm, and additional arguments (or the list `control`) set
method-specific parameters. The result is a `TOUR` object. See
[`?solve_TSP`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
for the complete list of parameters.

## Overview of the methods

| Method argument | Algorithm | Type | Problems | Extra parameters |
|:---|:---|:---|:---|:---|
| `"identity"`, `"random"` | Order in the data / random order | construction | TSP, ATSP | none |
| `"nn"` | Nearest neighbor | construction | TSP, ATSP | `start` |
| `"repetitive_nn"` | Nearest neighbor from every start city | construction | TSP, ATSP | `start` |
| `"nearest_insertion"` | Nearest insertion | construction | TSP, ATSP | `start` |
| `"farthest_insertion"` | Farthest insertion | construction | TSP, ATSP | `start` |
| `"cheapest_insertion"` | Cheapest insertion | construction | TSP, ATSP | `start` |
| `"arbitrary_insertion"` | Arbitrary insertion | construction | TSP, ATSP | `start` |
| `"two_opt"` | 2-opt exchange | improvement | TSP, ATSP | `tour`, `two_opt_repetitions` |
| `"sa"` | Simulated annealing | improvement | TSP, ATSP | `tour`, `local_move`, `temp`, `tmax`, `maxit`, `trace` |
| `"concorde"` | Concorde (exact, branch-and-cut) | external solver | TSP, ETSP | `clo`, `precision`, `keep_files`, `verbose`, `seed` |
| `"linkern"` | Chained Lin-Kernighan | external solver | TSP, ETSP | `clo`, `precision`, `keep_files`, `verbose`, `seed` |

The extra parameters are passed in `control` or directly as arguments to
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md).
In addition, all heuristics accept the common parameters `rep`,
`two_opt`, `seed` and `verbose` described below. Concorde and linkern do
not support `rep` and `two_opt`. See
[`?solve_TSP`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
and [`?Concorde`](https://michael.hahsler.net/TSP/reference/Concorde.md)
for details on each parameter.

Most heuristics accept the following common control parameters:

- `rep`: the number of random restarts. The best tour found is returned.
- `two_opt`: if `TRUE`, the constructed tour is refined by 2-opt.
- `start`: the first city for nearest neighbor and insertion algorithms.
- `seed`: a non-negative integer to make randomized methods and
  repetitions reproducible.
- `verbose`: show progress and the parameters that are used.

If no method is given,
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md)
uses arbitrary insertion followed by 2-opt improvement. Construction
heuristics build a tour from scratch, while improvement heuristics start
with an existing tour (given as `tour`) and try to shorten it. If no
tour is supplied to an improvement heuristic, a random tour is used.

## Tour construction heuristics

### Nearest neighbor

The nearest neighbor algorithm (Rosenkrantz et al. 1977) is a simple
greedy procedure. It starts with a randomly chosen city (or the city
given by `start`) and repeatedly appends the nearest city that has not
been visited yet. It is very fast, but typically has to make long
“jumps” at the end of the tour to reach the remaining cities.

The repetitive variant (`"repetitive_nn"`) runs the algorithm once from
every city and returns the shortest tour found.

### Insertion algorithms

All insertion algorithms (Rosenkrantz et al. 1977) start with a tour
consisting of a single city and repeatedly insert a city \\k\\ that is
not yet on the tour between two consecutive tour cities \\i\\ and \\j\\
so that the increase in tour length

\\d(i,k) + d(k,j) - d(i,j)\\

is minimal. They differ in how the city to be inserted is chosen:

- **Nearest insertion** chooses the city closest to any city on the
  tour.
- **Farthest insertion** chooses the city farthest from the tour. This
  first establishes an outline of the whole tour and fills in details
  later. In practice, farthest insertion often produces better tours
  than the other insertion variants.
- **Cheapest insertion** chooses the city with the lowest insertion
  cost.
- **Arbitrary insertion** chooses a random city. It is the fastest
  variant.

For distances that obey the triangle inequality, nearest and cheapest
insertion are related to Prim’s minimum spanning tree algorithm (Prim
1957) and guarantee tours that are at most twice the optimal length.

## Tour improvement heuristics

### 2-opt

Two-opt (Croes 1958) repeatedly removes two edges from the tour and
reconnects the two resulting paths in the only other possible way (which
is equivalent to reversing a part of the tour) whenever this shortens
the tour. The procedure stops at a *2-optimal* tour, i.e., a local
optimum for this neighborhood. `"two_opt_repetitions"` can be used to
restart the algorithm from random tours.

### Simulated annealing

Simulated annealing (Kirkpatrick et al. 1983) uses random local moves
(reversal of a random sub-tour) and accepts moves that make the tour
longer with a probability that decreases as the temperature is lowered.
This allows the search to escape local optima. The implementation uses
[`stats::optim()`](https://rdrr.io/r/stats/optim.html) with method
`"SANN"`. The method is usually much slower than 2-opt and needs tuning
of the cooling schedule (`temp`, `tmax`, `maxit`). By default, only
10,000 evaluations are used to keep the run time short, so a larger
`maxit` is needed for competitive results.

### Improving an existing tour

Improvement heuristics can be applied to any tour, including a tour that
was created outside of **TSP**. For example, we can refine a nearest
neighbor tour with 2-opt and then use a long simulated annealing run for
further improvement.

``` r

library(TSP)
data(USCA50)

nn <- solve_TSP(USCA50, method = "nn", start = 1)
opt2 <- solve_TSP(USCA50, method = "two_opt", tour = nn)
sa <- solve_TSP(USCA50, method = "sa", tour = opt2, maxit = 50000, seed = 1234)

c(nn = tour_length(nn), two_opt = tour_length(opt2), sa = tour_length(sa))
#>      nn two_opt      sa 
#>   17032   16341   15008
```

A plain permutation vector can be converted with
[`as.TOUR()`](https://michael.hahsler.net/TSP/reference/TOUR.md) and its
length can be calculated for a given problem with
`tour_length(USCA50, order)`.

## State-of-the-art solvers

Concorde (Applegate et al. 2006) is currently one of the best exact
solvers for symmetric TSPs. It uses the branch-and-cut method (Padberg
and Rinaldi 1990) which iteratively tightens a linear programming
relaxation of the problem. The Chained Lin-Kernighan heuristic
(Applegate et al. 2003), an extension of the Lin-Kernighan heuristic
(Lin and Kernighan 1973), is a very fast heuristic that finds excellent
tours for large instances. Both are accessed via the `"concorde"` and
`"linkern"` methods, but require that the Concorde executables are
installed separately. See
[`vignette("concorde")`](https://michael.hahsler.net/TSP/articles/concorde.md)
for details.

## Parallel execution and reproducibility

Repetitions (including `"repetitive_nn"`) are run using the **foreach**
package and can be executed in parallel by registering a backend before
calling
[`solve_TSP()`](https://michael.hahsler.net/TSP/reference/solve_TSP.md).

``` r

library(doParallel)
registerDoParallel()
solve_TSP(USCA50, method = "nn", two_opt = TRUE, rep = 100, seed = 1234)
```

With `seed` set, the result does not depend on the backend or the number
of workers. For Concorde and linkern, the seed is passed on to the
executables (option `-s`; it must be a whole number between 0 and
`.Machine$integer.max`). If no seed is given, one is drawn from R’s
random number generator, so
[`set.seed()`](https://rdrr.io/r/base/Random.html) also makes these two
methods reproducible. Note that specifying `seed` calls
[`set.seed()`](https://rdrr.io/r/base/Random.html) and replaces the
global random-number state, while without `seed` the global stream is
used and advanced as usual.

## Asymmetric and Euclidean problems

All heuristics above work directly with asymmetric problems (`ATSP`).
Solvers that only handle symmetric problems use an automatic
reformulation as a symmetric TSP with twice as many cities (see
[`?reformulate_ATSP_as_TSP`](https://michael.hahsler.net/TSP/reference/reformulate_ATSP_as_TSP.md)).
Euclidean problems (`ETSP`) are converted into distance matrices for the
built-in heuristics, which needs memory proportional to the square of
the number of cities. Only `"concorde"` and `"linkern"` work with `ETSP`
objects directly.

## References

Applegate, David, Robert Bixby, Vasek Chvátal, and William Cook. 2006.
*Concorde TSP Solver*.
<https://www.math.uwaterloo.ca/tsp/concorde.html>.

Applegate, David, William Cook, and Andre Rohe. 2003. “Chained
Lin-Kernighan for Large Traveling Salesman Problems.” *INFORMS Journal
on Computing* 15 (1): 82–92.
<https://doi.org/10.1287/ijoc.15.1.82.15157>.

Croes, G. A. 1958. “A Method for Solving Traveling-Salesman Problems.”
*Operations Research* 6 (6): 791–812.
<https://doi.org/10.1287/opre.6.6.791>.

Hahsler, Michael, and Kurt Hornik. 2007. “TSP – Infrastructure for the
Traveling Salesperson Problem.” *Journal of Statistical Software* 23
(2): 1–21. <https://doi.org/10.18637/jss.v023.i02>.

Kirkpatrick, S., C. D. Gelatt, and M. P. Vecchi. 1983. “Optimization by
Simulated Annealing.” *Science* 220 (4598): 671–80.
<https://doi.org/10.1126/science.220.4598.671>.

Lin, S., and B. W. Kernighan. 1973. “An Effective Heuristic Algorithm
for the Traveling-Salesman Problem.” *Operations Research* 21 (2):
498–516. <https://doi.org/10.1287/opre.21.2.498>.

Padberg, M., and G. Rinaldi. 1990. “Facet Identification for the
Symmetric Traveling Salesman Polytope.” *Mathematical Programming*
(Secaucus, NJ, USA) 47 (2): 219–57.

Prim, R. C. 1957. “Shortest Connection Networks and Some
Generalisations.” *Bell System Technical Journal* 36: 1389–401.

Rosenkrantz, Daniel J., Richard E. Stearns, and Philip M. Lewis II.
1977. “An Analysis of Several Heuristics for the Traveling Salesman
Problem.” *SIAM Journal on Computing* 6 (3): 563–81.
<https://doi.org/10.1007/978-1-4020-9688-4_3>.
