# TSP - Traveling Salesperson Problem - R package

[![CRAN version](http://www.r-pkg.org/badges/version/TSP)](https://cran.r-project.org/package=TSP)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/TSP)](https://cran.r-project.org/package=TSP)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/TSP.svg?branch=master)](https://travis-ci.org/mhahsler/TSP)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/TSP?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/TSP)

This package provides the 
basic infrastructure and some algorithms for the traveling
salesman problems (symmetric, asymmetric and Euclidean TSPs). 
The package provides some simple algorithms and
an interface to the Concorde TSP solver and its implementation of the 
Chained-Lin-Kernighan heuristic. 

## Installation

__Stable CRAN version:__ install from within R with
```R
install.packages("TSP")
```
__Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/TSP/build/artifacts) or install from GitHub (needs devtools).
```R 
install_git("mhahsler/TSP")
```

## Usage

Load a data set with 312 cities (USA and Canada) and create a TSP object. 
```R
R> library("TSP")
R> data("USCA312")
 
R> tsp <- TSP(USCA312)
R> tsp
```

```
object of class 'TSP'
312 cities (distance   'euclidean')
```

Find a tour using the default heuristic. 
```R
tour <- solve_TSP(tsp)
tour
```

```
object of class 'TOUR' 
result of method 'arbitrary_insertion+two_opt' for 312 cities
tour length: 40621
```

An online example application of TSP can be found on [shinyapps](https://shrinidhee.shinyapps.io/SimpleTSP).

## References

* Michael Hahsler and Kurt Hornik, [TSP - Infrastructure for the Traveling Salesperson Problem,](http://dx.doi.org/10.18637/jss.v023.i02) _Journal of Statistical Software,_ 22(2), 2007.
* [TSP package vignette](https://cran.r-project.org/package=TSP/vignettes/TSP.pdf) with complete examples.
* [Reference manual](https://cran.r-project.org/package=TSP/TSP.pdf)
* [Concorde TSP solver home page](http://www.tsp.gatech.edu/concorde.html)
