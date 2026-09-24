# Reformulate an ATSP as a symmetric TSP

An ATSP can be formulated as a symmetric TSP by doubling the number of
cities (Jonker and Volgenant 1983). The solution of the TSP also
represents the solution of the original ATSP.

## Usage

``` r
reformulate_ATSP_as_TSP(x, infeasible = Inf, cheap = -Inf)

filter_ATSP_as_TSP_dummies(tour, atsp)
```

## Arguments

- x:

  an [ATSP](http://michael.hahsler.net/TSP/reference/ATSP.md).

- infeasible:

  value for infeasible connections.

- cheap:

  value for distance between a city and its corresponding dummy city.

- tour:

  a [TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md) created for
  an ATSP reformulated as a TSP.

- atsp:

  the original [ATSP](http://michael.hahsler.net/TSP/reference/ATSP.md).

## Value

`reformulate_ATSP_as_TSP()` returns a
[TSP](http://michael.hahsler.net/TSP/reference/TSP.md) object.
`filter_ATSP_as_TSP_dummies()` returns a
[TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md) object.

## Details

To reformulate an
[ATSP](http://michael.hahsler.net/TSP/reference/ATSP.md) as a
[TSP](http://michael.hahsler.net/TSP/reference/TSP.md), a dummy city
(e.g., for 'New York' a dummy city 'New York\*') is added. Between each
city and its corresponding dummy city a very small (or negative)
distance with value `cheap` is used. To ensure that each city always
occurs in the solution together with its dummy city, this cost has to be
much smaller than the distances in the TSP. The original distances are
used between the cities and the dummy cities, where each city is
responsible for the distance going to the city and the dummy city is
responsible for the distance coming from the city. The distances between
all cities and the distances between all dummy cities are set to
`infeasible`, a very large value which prevents the solver from using
these links. We use infinite values here and
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)
treats them appropriately.

`filter_ATSP_as_TSP_dummies()` can be used to extract the solution for
the original ATSP from the tour found for an ATSP reformulated as a TSP.
Note that the symmetric TSP tour does not reveal the direction for the
ATSP. The filter function computes the tour length for both directions
and returns the shorter tour.

[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)
has a parameter `as_TSP` that performs the reformulation and filters the
dummy cities automatically.

**Note on performance:** Doubling the problem size reduces performance
and can have a particularly negative impact on solution quality for
heuristics. It should be used only together with Concorde when the
optimal solution is required. Most heuristics can solve ATSPs directly
with good solution quality.

## References

Jonker, R. and Volgenant, T. (1983): Transforming asymmetric into
symmetric traveling salesman problems, *Operations Research Letters,* 2,
161–163.

## See also

Other TSP: [`ATSP()`](http://michael.hahsler.net/TSP/reference/ATSP.md),
[`Concorde`](http://michael.hahsler.net/TSP/reference/Concorde.md),
[`ETSP()`](http://michael.hahsler.net/TSP/reference/ETSP.md),
[`TSP()`](http://michael.hahsler.net/TSP/reference/TSP.md),
[`TSPLIB`](http://michael.hahsler.net/TSP/reference/TSPLIB.md),
[`insert_dummy()`](http://michael.hahsler.net/TSP/reference/insert_dummy.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)

## Author

Michael Hahsler

## Examples

``` r
data("USCA50")

## set the distances from anywhere to Austin to zero which makes it an ATSP
austin <- which(labels(USCA50) == "Austin, TX")
atsp <- as.ATSP(USCA50)
atsp[, austin] <- 0
atsp
#> object of class ‘ATSP’  (asymmetric TSP) 
#> 50 cities (distance ‘euclidean’) 

## reformulate as a TSP (by doubling the number of cities with dummy cities marked with *)
tsp <- reformulate_ATSP_as_TSP(atsp)
tsp
#> object of class ‘TSP’ 
#> 100 cities (distance ‘euclidean’) 

## create tour for the TSP. You should use Concorde to find the optimal solution.
# tour_tsp <- solve_TSP(tsp, method = "concorde")
# The standard heuristic is bad for this problem. We use it here because
#   Concord may not be installed.
tour_tsp <- solve_TSP(tsp)
head(labels(tour_tsp), n = 10)
#>  [1] "Austin, TX"      "Austin, TX*"     "Billings, MT"    "Billings, MT*"  
#>  [5] "Bellingham, WA"  "Bellingham, WA*" "Anchorage, AK"   "Anchorage, AK*" 
#>  [9] "Brandon, MB"     "Brandon, MB*"   
tour_tsp
#> object of class ‘TOUR’ 
#> result of method ‘arbitrary_insertion+two_opt’ for 100 cities
#> tour length: -Inf 
# The tour length is -Inf since it includes cheap links
#  from a city to its dummy city.

## get the solution for the original ATSP by filtering out the dummy cities.
tour_atsp <- filter_ATSP_as_TSP_dummies(tour_tsp, atsp = atsp)
tour_atsp
#> object of class ‘TOUR’ 
#> result of method ‘arbitrary_insertion+two_opt’ for 50 cities
#> tour length: 30963 
head(labels(tour_atsp), n = 10)
#>  [1] "Austin, TX"      "Billings, MT"    "Bellingham, WA"  "Anchorage, AK"  
#>  [5] "Brandon, MB"     "Calgary, AB"     "Bakersfield, CA" "Berkeley, CA"   
#>  [9] "Bismarck, ND"    "Bay City, MI"   

## This process can also be done automatically by using as_TSP = TRUE:
# solve_TSP(atsp, method = "concorde", as_TSP = TRUE)

## The default heuristic can directly solve ATSPs with results close to the
#  optimal solution of 12715.
solve_TSP(atsp, control = list(rep = 10))
#> Warning: executing %dopar% sequentially: no parallel backend registered
#> object of class ‘TOUR’ 
#> result of method ‘arbitrary_insertion+two_opt_rep_10’ for 50 cities
#> tour length: 12962 
```
