#' @keywords internal
#'
#' @section Key functions:
#' - [solve_TSP()]
#'
#' @importFrom stats as.dist dist
#' @importFrom utils read.table write.table head tail
#' @importFrom grDevices gray.colors
#' @importFrom graphics image.default plot polygon
#' @importFrom foreach foreach "%dopar%"
#'
#' @useDynLib TSP, .registration=TRUE
"_PACKAGE"
