#######################################################################
# TSP - Traveling Salesperson Problem
# Copyrigth (C) 2011 Michael Hahsler and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' Read and write TSPLIB files
#'
#' Reads and writes TSPLIB format files. TSPLIB files can be used by most TSP
#' solvers. Sample instances for the TSP in TSPLIB format are available on the
#' TSPLIB homepage (see references).
#'
#' In the TSPLIB format distances are represented by integer values. Therefore,
#' if `x` contains `double` values (which is normal in R) the values
#' given in `x` are multiplied by \eqn{10^{precision}} before coercion to
#' `integer`. Note that therefore all results produced by programs using
#' the TSPLIB file as input need to be divided by \eqn{10^{precision}} (i.e.,
#' the decimal point has to be shifted `precision` placed to the left).
#'
#' Currently only the following `EDGE_WEIGHT_TYPE`s are implemented:
#' `EXPLICIT`, `EUC_2D` and `EUC_3D`.
#'
#' @name TSPLIB
#' @aliases TSPLIB
#' @family TSP
#'
#' @param x an object with a TSP problem.
#' `NA`s are not allowed.
#' @param file file name or a [connection].
#' @param precision controls the number of decimal places used to represent
#' distances (see details).  If `x` already is `integer`, this
#' argument is ignored and `x` is used as is.
#' @param inf replacement value for `Inf` (TSPLIB format cannot handle
#' `Inf`). If `inf` is `NULL`, a large value of \eqn{max(x) + 2
#' range(x)} (ignoring infinite entries) is used.
#' @param neg_inf replacement value for `-Inf`.  If no value is specified,
#' a small value of \eqn{min(x) - 2 range(x)} (ignoring infinite entries) is
#' used.
#' @returns returns an object of class `TSP` or
#' `ATSP`.
#' @author Michael Hahsler
#' @references TSPLIB home page,
#' \url{http://comopt.ifi.uni-heidelberg.de/software/TSPLIB95/}
#' @keywords file
#' @examples
#'
#' ## Drilling problem from TSP
#' drill <- read_TSPLIB(system.file("examples/d493.tsp", package = "TSP"))
#' drill
#' tour <- solve_TSP(drill, method = "nn", two_opt = TRUE)
#' tour
#' plot(drill, tour, cex=.6, col = "red", pch= 3, main = "TSPLIB: d493")
#'
#'
#' ## Write and read data in TSPLIB format
#' x <- data.frame(x=runif(5), y=runif(5))
#'
#' ## create TSP, ATSP and ETSP (2D)
#' tsp <- TSP(dist(x))
#' atsp <- ATSP(dist(x))
#' etsp <- ETSP(x[,1:2])
#'
#' write_TSPLIB(tsp, file="example.tsp")
#' #file.show("example.tsp")
#' r <- read_TSPLIB("example.tsp")
#' r
#'
#' write_TSPLIB(atsp, file="example.tsp")
#' #file.show("example.tsp")
#' r <- read_TSPLIB("example.tsp")
#' r
#'
#' write_TSPLIB(etsp, file="example.tsp")
#' #file.show("example.tsp")
#' r <- read_TSPLIB("example.tsp")
#' r
#'
#' ## clean up
#' unlink("example.tsp")
#' @export
read_TSPLIB <- function(file, precision = 0) {

  ## TSP or ATSP
  type <- NULL

  lines <- readLines(file)

  ## get info
  metadata <- grep(":", lines)

  info <- list()
  lapply(strsplit(lines[metadata], "[[:space:]]*:[[:space:]]*"),
    FUN = function(x) {
      x[2] <- sub("[[:space:]]*$","",x[2]) ## kill trailing spaces
      info[[toupper(x[1])]] <<- toupper(x[2])
    })

  ## check
  if(substr(info$TYPE, 1, 3) == "TSP") type <- "TSP"
  else if(substr(info$TYPE, 1, 3) == "ATS") type <- "ATSP"
  else stop ("Currently the only implemented TYPEs are TSP and ATS(P)!")

  dim <- as.integer(info$DIMENSION)

  if(info$EDGE_WEIGHT_TYPE == "EXPLICIT") {
    ## get data
    data_start <- grep("EDGE_WEIGHT_SECTION", lines, ignore.case = TRUE)
    if(length(data_start) == 0) stop("EDGE_WEIGHT_SECTION missing")

    data <- lines[(data_start+1):length(lines)]
    data <- sub("EOF", "", data, ignore.case = TRUE) ## kill optional EOF
    data <- sub("^[[:space:]]*", "", data)## kill leading spaces
    data <- strsplit(paste(data, collapse = " "), "[[:space:]]+")[[1]]

    ## remove everything after the data
    if(info$EDGE_WEIGHT_FORMAT == "FULL_MATRIX")
      data <- data[1:(dim^2)]
    else if(info$EDGE_WEIGHT_FORMAT == "UPPER_ROW"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_COL"
      || info$EDGE_WEIGHT_FORMAT == "UPPER_COL"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_ROW")
      data <- data[1:(dim*(dim-1)/2)]
    else if(info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_ROW"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_COL"
      || info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_COL"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_ROW")
      data <- data[1:(dim*(dim-1)/2 + dim)]

    data <- as.numeric(data)

    if(precision != 0) data <- data / 10^precision

    ## ATSP
    if(type == "ATSP") {
      if(info$EDGE_WEIGHT_FORMAT == "FULL_MATRIX"){
        ## todo: find out if FULL_MATRIX is row or column oriented?
        data <- matrix(data, ncol = dim)
      }else stop("ATSP needs EDGE_WEIGHT_FORMAT FULL_MATRIX!")

      return(ATSP(data))
    }

    ## TSP
    ## we have only symmetric data here!
    if(info$EDGE_WEIGHT_FORMAT == "FULL_MATRIX") {
      data <- as.dist(matrix(data, ncol = dim))

    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_ROW"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_COL") {
      class(data) <- "dist"
      attr(data, "Size")  <- dim
      attr(data, "Diag")  <- FALSE
      attr(data, "Upper") <- FALSE

    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_COL"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_ROW") {

      m <- matrix(NA, nrow = dim, ncol = dim)
      m[upper.tri(m, diag = FALSE)] <- data
      data <- as.dist(t(m))

    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_ROW"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_COL") {
      class(data) <- "dist"
      attr(data, "Size")  <- dim
      attr(data, "Diag")  <- TRUE
      attr(data, "Upper") <- FALSE

      data <- as.dist(data, diag = FALSE)

    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_COL"
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_ROW") {

      m <- matrix(NA, nrow = dim, ncol = dim)
      m[upper.tri(m, diag = TRUE)] <- data
      data <- as.dist(t(m))


    }else stop("The specified EDGE_WEIGHT_FORMAT is not implemented!")

    return(TSP(data))

  } else if (info$EDGE_WEIGHT_TYPE == "EUC_2D" ||
      info$EDGE_WEIGHT_TYPE == "EUC_2D") {

    data_start <- grep("NODE_COORD_SECTION", lines, ignore.case = TRUE)
    if(length(data_start) == 0) stop("NODE_COORD_SECTION missing")

    data <- lines[(data_start+1):(data_start+dim)]
    data <- matrix(as.numeric(unlist(strsplit(data, split="\\s+"))),
      nrow = dim, byrow = TRUE)
    data <- data[,-1]
    return(ETSP(data))

  }
  stop("EDGE_WEIGHT_TYPE not implemented! Implemented types are EXPLICIT, EUC_2D and EUC_3D")
}

#' @rdname TSPLIB
#' @export
write_TSPLIB <- function(x, file, precision = 6, inf = NULL, neg_inf = NULL)
  UseMethod("write_TSPLIB")


## write a simple TSPLIB format file from an object of class TSP
## (contains a dist object or a symmetric matrix)

## TSP has data as integer
#' @rdname TSPLIB
#' @export
write_TSPLIB.TSP <- function(x, file, precision = 6,
  inf = NULL, neg_inf = NULL) {

  ## prepare data (NA, Inf)
  if(any(is.na(x))) stop("NAs not allowed!")
  x <- .replaceInf(x, inf, neg_inf)

  ## Concorde can handle UPPER_ROW and dist (lower triangle matrix)
  ## is symmetric.
  format <- "EDGE_WEIGHT_FORMAT: UPPER_ROW"

  zz <- file(file, "w")

  cat("NAME: TSP",
    "COMMENT: Generated by write_TSPLIB (R-package TSP)",
    "TYPE: TSP",
    paste("DIMENSION:", n_of_cities(x)),
    "EDGE_WEIGHT_TYPE: EXPLICIT",
    format,
    file = zz, sep = "\n")

  ## only integers can be used as weights
  if(storage.mode(x) != "integer" && precision != 0) x <- x * 10^precision

  x <- suppressWarnings(as.integer(x))
  if(any(is.na(x))) stop("Integer overflow, please reduce precision.")

  cat("EDGE_WEIGHT_SECTION", x, file = zz, sep = "\n")
  cat("EOF", file = zz, sep = "\n")

  close(zz)
}


#' @rdname TSPLIB
#' @export
write_TSPLIB.ATSP <- function(x, file, precision = 6, inf = NULL, neg_inf = NULL) {

  ## prepare data (NA, Inf)
  if(any(is.na(x))) stop("NAs not allowed!")
  x <- .replaceInf(x, inf, neg_inf)

  format <- "EDGE_WEIGHT_FORMAT: FULL_MATRIX"

  zz <- file(file, "w")

  cat("NAME: ATSP",
    "COMMENT: Generated by write_TSPLIB (R package TSP)",
    "TYPE: ATSP",
    paste("DIMENSION:", n_of_cities(x)),
    "EDGE_WEIGHT_TYPE: EXPLICIT",
    format,
    file = zz, sep = "\n")


  ## only integers can be used as weights
  if(storage.mode(x) != "integer") x <- x * 10^precision

  x <- suppressWarnings(as.integer(x))
  if(any(is.na(x))) stop("integer overflow, please reduce precision.")

  cat("EDGE_WEIGHT_SECTION", x, file = zz, sep = "\n")
  cat("EOF", file = zz, sep = "\n")

  close(zz)
}


## ETSP use data as real
#' @rdname TSPLIB
#' @export
write_TSPLIB.ETSP <- function(x, file, precision = 6,
  inf = NULL, neg_inf = NULL) {

  if(any(is.na(x))) stop("NAs are not allowed!")
  if(any(!is.finite(x))) stop("Only finite values allowed!")

  if(ncol(x) == 2) type <- "EUC_2D"
  else if(ncol(x) == 3) type <- "EUC_3D"
  else stop("Only EUC_2D and EUC_3D supported.")

  zz <- file(file, "w")

  cat("NAME: ETSP",
    "COMMENT: Generated by write_TSPLIB (R package TSP)",
    "TYPE: TSP",
    paste("DIMENSION:", n_of_cities(x)),
    paste("EDGE_WEIGHT_TYPE:", type),
    file = zz, sep = "\n")

  ## fix row names
  rownames(x) <- NULL
  x <- do.call(data.frame, lapply(1:ncol(x), FUN =
      function(i) sprintf(paste("%0.", precision, "e", sep=""), x[,i])))

  cat("NODE_COORD_SECTION", file = zz, sep = "\n")
  write.table(x, quote=FALSE, col.names = FALSE, file = zz)
  cat("EOF", file = zz, sep = "\n")

  close(zz)
}
