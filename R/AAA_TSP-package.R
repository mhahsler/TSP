#' @title `r packageDescription("TSP")$Package`: `r packageDescription("TSP")$Title`
#'
#' @description `r packageDescription("TSP")$Description`
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
