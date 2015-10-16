# TSP - Traveling Salesperson Problem - R package

[![CRAN version](http://www.r-pkg.org/badges/version/TSP)](http://cran.r-project.org/web/packages/TSP/index.html)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/TSP)](http://cran.r-project.org/web/packages/TSP/index.html)
[![Travis-CI Build Status](https://travis-ci.org/mhahsler/TSP.svg?branch=master)](https://travis-ci.org/mhahsler/TSP)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mhahsler/TSP?branch=master&svg=true)](https://ci.appveyor.com/project/mhahsler/TSP)

This package provides the 
basic infrastructure and some algorithms for the traveling
salesman problems (symmetric, asymmetric and Euclidean TSPs). 
The package provides some simple algorithms and
an interface to the Concorde TSP solver and its implementation of the 
Chained-Lin-Kernighan heuristic. 

## Installation

* __Stable CRAN version:__ install from within R.
* __Current development version:__ Download package from [AppVeyor](https://ci.appveyor.com/project/mhahsler/TSP/build/artifacts) or install via `intall_github("mhahsler/TSP")` (requires R package `devtools`) 

## Example

This example loads a data set with 312 cities (USA and Canada) and 
```R
## load library and read data
R> library("TSP")
R> data("USCA312")
 
## create a TSP object from the data 
R> tsp <- TSP(USCA312)
R> tsp

object of class 'TSP'
312 cities (distance   'euclidean')
   
## find a tour using the default heuristic 
R> tour <- solve_TSP(tsp)
R> tour
    
object of class 'TOUR' 
result of method 'arbitrary_insertion+two_opt' for 312 cities
tour length: 40621
```

An online example application of TSP can be found on [shinyapps](https://shrinidhee.shinyapps.io/SimpleTSP).

## Further Information

* Michael Hahsler and Kurt Hornik, [TSP - Infrastructure for the Traveling Salesperson Problem,](http://dx.doi.org/10.18637/jss.v023.i02) _Journal of Statistical Software,_ 22(2), 2007.
* [TSP package vignette](http://cran.r-project.org/web/packages/TSP/vignettes/TSP.pdf) with complete examples.
* [Reference manual](http://cran.r-project.org/web/packages/TSP/TSP.pdf)
* [Concorde TSP solver home page](http://www.tsp.gatech.edu/concorde.html)


