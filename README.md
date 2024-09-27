
# <img src="man/figures/logo.svg" align="right" height="139" /> R package TSP - Infrastructure for the Traveling Salesperson Problem

[![r-universe
status](https://mhahsler.r-universe.dev/badges/TSP)](https://mhahsler.r-universe.dev/TSP)
[![Package on
CRAN](https://www.r-pkg.org/badges/version/TSP)](https://CRAN.R-project.org/package=TSP)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/TSP)](https://CRAN.R-project.org/package=TSP)
[![Anaconda.org](https://anaconda.org/conda-forge/r-tsp/badges/version.svg)](https://anaconda.org/conda-forge/r-tsp)

## Introduction

This package provides the basic infrastructure and some algorithms for
the traveling salesman problems (symmetric, asymmetric and Euclidean
TSPs). The package provides some simple algorithms and an interface to
the [Concorde TSP solver](http://www.math.uwaterloo.ca/tsp/concorde/)
and its implementation of the Chained-Lin-Kernighan heuristic.

The following R packages use `TSP`:
[archetypes](https://CRAN.R-project.org/package=archetypes),
[cholera](https://CRAN.R-project.org/package=cholera),
[colorpatch](https://CRAN.R-project.org/package=colorpatch),
[condvis](https://CRAN.R-project.org/package=condvis),
[CRTspat](https://CRAN.R-project.org/package=CRTspat),
[ForagingOrg](https://CRAN.R-project.org/package=ForagingOrg),
[isocir](https://CRAN.R-project.org/package=isocir),
[jocre](https://CRAN.R-project.org/package=jocre),
[MLCOPULA](https://CRAN.R-project.org/package=MLCOPULA),
[nilde](https://CRAN.R-project.org/package=nilde),
[nlnet](https://CRAN.R-project.org/package=nlnet),
[PairViz](https://CRAN.R-project.org/package=PairViz),
[pencopulaCond](https://CRAN.R-project.org/package=pencopulaCond),
[SCORPIUS](https://CRAN.R-project.org/package=SCORPIUS),
[sensitivity](https://CRAN.R-project.org/package=sensitivity),
[seriation](https://CRAN.R-project.org/package=seriation),
[sfnetworks](https://CRAN.R-project.org/package=sfnetworks),
[tspmeta](https://CRAN.R-project.org/package=tspmeta),
[VineCopula](https://CRAN.R-project.org/package=VineCopula),
[vines](https://CRAN.R-project.org/package=vines)

To cite package ‘TSP’ in publications use:

> Hahsler M, Hornik K (2007). “TSP - Infrastructure for the traveling
> salesperson problem.” *Journal of Statistical Software*, *23*(2),
> 1-21. ISSN 1548-7660, <doi:10.18637/jss.v023.i02>
> <https://doi.org/10.18637/jss.v023.i02>.

    @Article{,
      title = {TSP -- {I}nfrastructure for the traveling salesperson problem},
      author = {Michael Hahsler and Kurt Hornik},
      year = {2007},
      journal = {Journal of Statistical Software},
      volume = {23},
      number = {2},
      pages = {1--21},
      doi = {10.18637/jss.v023.i02},
      month = {December},
      issn = {1548-7660},
    }

## Installation

**Stable CRAN version:** Install from within R with

``` r
install.packages("TSP")
```

**Current development version:** Install from
[r-universe.](https://mhahsler.r-universe.dev/TSP)

``` r
install.packages("TSP",
    repos = c("https://mhahsler.r-universe.dev",
              "https://cloud.r-project.org/"))
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
    ## tour length: 40959

Show the first few cities in the tour.

``` r
head(tour, n = 10)
```

    ##   Bakersfield, CA        Fresno, CA      Stockton, CA      Berkeley, CA 
    ##                17                99               268                26 
    ##       Oakland, CA San Francisco, CA      San Jose, CA    Santa Cruz, CA 
    ##               186               241               242               245 
    ##          Hilo, HI      Honolulu, HI 
    ##               119               120

An online example application of TSP can be found on
[shinyapps](https://shrinidhee.shinyapps.io/SimpleTSP).

## Help and Bug Reports

You can find Q&A’s and ask your own questions at
<https://stackoverflow.com/search?q=TSP+R>

Please submit bug reports to <https://github.com/mhahsler/TSP/issues>

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
