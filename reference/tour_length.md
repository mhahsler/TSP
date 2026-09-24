# Calculate the length of a tour

Calculate the length of a
[TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md) for a
[TSP](http://michael.hahsler.net/TSP/reference/TSP.md).

## Usage

``` r
tour_length(x, ...)

# S3 method for class 'TSP'
tour_length(x, order, ...)

# S3 method for class 'ATSP'
tour_length(x, order, ...)

# S3 method for class 'ETSP'
tour_length(x, order, ...)

# S3 method for class 'TOUR'
tour_length(x, tsp = NULL, ...)

# S3 method for class 'integer'
tour_length(x, tsp = NULL, ...)
```

## Arguments

- x:

  a TSP problem or a
  [TOUR](http://michael.hahsler.net/TSP/reference/TOUR.md).

- ...:

  further arguments are currently unused.

- order:

  an object of class `TOUR`

- tsp:

  a TSP object.

## Details

If no `tsp` is specified, then the tour length stored in `x` as
attribute `"tour_length"` is returned. If `tsp` is given then the tour
length is recalculated using the specified TSP problem.

If a distance in the tour is infinite, the result is also infinite. If
the tour contains positive and negative infinite distances then the
method returns `NA`.

## See also

Other TOUR:
[`TOUR()`](http://michael.hahsler.net/TSP/reference/TOUR.md),
[`cut_tour()`](http://michael.hahsler.net/TSP/reference/cut_tour.md),
[`solve_TSP()`](http://michael.hahsler.net/TSP/reference/solve_TSP.md)

## Author

Michael Hahsler

## Examples

``` r

data("USCA50")

## original order
tour_length(solve_TSP(USCA50, method="identity"))
#> [1] 59321

## length of a manually created (random) tour
tour <- TOUR(sample(seq(n_of_cities(USCA50))))
tour
#> object of class ‘TOUR’ 
#> result of method ‘NA’ for 50 cities
#> tour length: unknown
tour_length(tour)
#> [1] NA
tour_length(tour, USCA50)
#> [1] 55323
```
