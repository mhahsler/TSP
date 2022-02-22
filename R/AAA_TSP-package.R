#' @title `r packageDescription("TSP")$Package`: `r packageDescription("TSP")$Title`
#'
#'
#' @description Basic infrastructure and some algorithms for the traveling salesperson problem (also traveling salesman problem; TSP). The package provides some simple algorithms and an interface to the Concorde TSP solver and its implementation of the Chained-Lin-Kernighan heuristic. The code for [Concorde](https://www.math.uwaterloo.ca/tsp/concorde/) itself is not included in the package and has to be obtained separately.
#'
#' @references Michael Hahsler and Kurt Hornik. TSP -- Infrastructure for the traveling salesperson problem. Journal of Statistical Software, 23(2):1--21, December 2007. \doi{10.18637/jss.v023.i02}
#'
#' @section Key functions:
#' - [solve_TSP()]
#'
#' @author Michael Hahsler
#' @docType package
#' @name TSP-package
#'
#' @importFrom stats as.dist dist
#' @importFrom utils read.table write.table head tail
#' @importFrom grDevices gray.colors
#' @importFrom graphics image.default plot polygon
#' @importFrom foreach foreach "%dopar%"

#' @useDynLib TSP, .registration=TRUE
NULL
