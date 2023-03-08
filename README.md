
# <img src="man/figures/logo.svg" align="right" height="139" /> R package TSP - Traveling Salesperson Problem (TSP)

[![CRAN
version](http://www.r-pkg.org/badges/version/TSP)](https://CRAN.R-project.org/package=TSP)
[![stream r-universe
status](https://mhahsler.r-universe.dev/badges/TSP)](https://mhahsler.r-universe.dev/TSP)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/TSP)](https://CRAN.R-project.org/package=TSP)

This package provides the basic infrastructure and some algorithms for
the traveling salesman problems (symmetric, asymmetric and Euclidean
TSPs). The package provides some simple algorithms and an interface to
the [Concorde TSP solver](http://www.math.uwaterloo.ca/tsp/concorde/)
and its implementation of the Chained-Lin-Kernighan heuristic.

## Installation

**Stable CRAN version:** Install from within R with

``` r
install.packages("TSP")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/TSP)

``` r
install.packages("TSP", repos = "https://mhahsler.r-universe.dev")
```

## Usage

Load a data set with 312 cities (USA and Canada) and create a TSP
object.

``` r
library("TSP")
data("USCA312")

tsp <- TSP(USCA312)
tsp
```

    ## object of class 'TSP' 
    ## 312 cities (distance 'euclidean')

Find a tour using the default heuristic.

``` r
tour <- solve_TSP(tsp)
tour
```

    ## object of class 'TOUR' 
    ## result of method 'arbitrary_insertion+two_opt' for 312 cities
    ## tour length: 41389

Show the first few cities in the tour.

``` r
head(tour, n = 10)
```

    ##     Jacksonville, FL      Gainesville, FL    Daytona Beach, FL 
    ##                  127                  101                   72 
    ##          Orlando, FL            Tampa, FL Saint Petersburg, FL 
    ##                  190                  275                  234 
    ##         Sarasota, FL  West Palm Beach, FL            Miami, FL 
    ##                  247                  296                  164 
    ##         Key West, FL 
    ##                  136

An online example application of TSP can be found on
[shinyapps](https://shrinidhee.shinyapps.io/SimpleTSP).

## References

- Michael Hahsler and Kurt Hornik, [TSP - Infrastructure for the
  Traveling Salesperson
  Problem,](http://dx.doi.org/10.18637/jss.v023.i02) *Journal of
  Statistical Software,* 22(2), 2007.
- [TSP package
  vignette](https://cran.r-project.org/package=TSP/vignettes/TSP.pdf)
  with complete examples.
- [Reference manual](https://cran.r-project.org/package=TSP/TSP.pdf)
- [Concorde TSP solver home
  page.](http://www.math.uwaterloo.ca/tsp/concorde/)
