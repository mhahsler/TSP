#######################################################################
# TSP - Traveling Salesperson Problem
# Copyright (C) 2011 Michael Hahsler and Kurt Hornik
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



#' Using the Concorde TSP Solver
#'
#' The Concorde TSP Solver package contains several solvers.  Currently,
#' interfaces to the Concorde solver (Applegate et al. 2001), one of the most
#' advanced and fastest TSP solvers using branch-and-cut, and the Chained
#' Lin-Kernighan (Applegate et al. 2003) implementation are provided in
#' \pkg{TSP}. Concorde can solve [TSP]s and [ETSP]s directly. [ATSP]s are
#' reformulated as larger TSP's and then solved.
#'
#'
#' # Installation of Concorde
#'
#' The Concorde TSP Solver is freely available for academic research.
#' It is not included in the \pkg{TSP} R package and has
#' to be obtained separately from the
#' [Concorde download page](https://www.math.uwaterloo.ca/tsp/concorde/downloads/downloads.htm).
#' Either download the precompiled executables and place them in a suitable
#' directory (make sure they are executable), or you can get the source code and
#' compile the program on your own. \pkg{TSP} needs to know where the executables are.
#' There are two options:
#' 1. use `concorde_path()` to set the path to the
#'   directory containing the executables for concorde and linkern, or
#' 2. make
#'   sure that the executables are in the search path stored in the `PATH`
#'   environment variable (see [Sys.setenv()]).
#'
#' # Using Concorde for `solve_TSP()`
#'
#' [solve_TSP()] uses [write_TSPLIB()] to write the TSP for
#' Concorde and tries to find the appropriate `precision` value (digits
#' after the decimal point) to convert the provided distances into the needed
#' integer value range. The `precision` value can also be specified in
#' `control` in [solve_TSP()] with method Concorde. Warning
#' messages will alert the user if the conversion to integer values results
#' into rounding errors that are worse then what is specified in the
#' `precision` control parameter.
#'
#' To get a list of all available command line options which can be used via
#' the `clo` option for `solve_TSP` use `concorde_help()` and
#' `linkern_help()`.  Several options (\option{-x}, \option{-o},
#' \option{-N}, \option{-Q}) are not available via [solve_TSP()] since they
#' are used by the interface.
#'
#' If Concorde takes too long, then you can interrupt [solve_TSP()] 
#' using `Esc/CTRL-C`. On most operating systems, this will also
#' terminate the Concorde executable. If Concorde keeps running, then you can 
#' kill the 'concorde' process via your operating system.
#'
#' @family TSP
#'
#' @name Concorde
#' @aliases Concorde concorde concorde_path concorde_help linkern_help
#'
#' @param path a character string with the path to the directory where the
#' executables are installed.
#' @returns `concorde_path()` returns the path to the executable. Others functions: Nothing.
#' @author Michael Hahsler
#' @references Concorde home page,
#' \url{https://www.math.uwaterloo.ca/tsp/concorde/}
#'
#' David Applegate, Robert Bixby, Vasek Chvatal, William Cook (2001): TSP cuts
#' which do not conform to the template paradigm, Computational Combinatorial
#' Optimization, M. Junger and D. Naddef (editors), Springer-Verlag.
#'
#' David Applegate and William Cook and Andre Rohe (2003): Chained
#' Lin-Kernighan for Large Traveling Salesman Problems, \emph{INFORMS Journal
#' on Computing}, \bold{15}, 82--92.
#' @keywords documentation
#' @examples
#'
#' \dontrun{
#' ## see if Concorde is correctly installed
#' concorde_path()
#'
#'
#' ## set path to the Concorde executible if it is not in the search PATH
#' ## Example:
#' ## concorde_path("~/concorde/")
#'
#' concorde_help()
#'
#' data("USCA312")
#'
#' ## run Concorde in verbose mode (-v) with fast cuts only (-V)
#' ## Note: use the contol parameter verbose = FALSE to supress Concorde's output
#' solve_TSP(USCA312, method = "concorde", control = list(clo = "-v -V"))
#' }
#'
NULL


## prepare distances as integers in the appropriate range [0..MAX]
.prepare_dist_concorde <- function(x, MAX, precision, verbose = FALSE) {
  ## handle inf
  x <- .replaceInf(x)

  ## fix neg. values
  min_x <- min(x)
  if (min_x < 0) {
    if (verbose)
    warning(
      "pTSP contains negative distances (maybe the result of a reformulation from ATSP). Shifting distances by subtracting the minimum.", immediate. = FALSE)
    x <- x - min_x
  }

  ## get max (excluding) to check for possible integer overflows
  max_x <- max(x)
  prec <- floor(log10(MAX / max_x))
  if (any((x %% 1) != 0) || prec < 0) {
    if (prec >= precision) {
      x <- x * 10 ^ precision
    } else {
      warning(
        paste0(
          "Concorde/Linken can only handle distances represented as integers. Converting the provided distances to integers with precison ",
          prec,
          ". This may lead to rounding errors."
        ),
        immediate. = TRUE
      )
      x <- x * 10 ^ prec
    } 
  }

  storage.mode(x) <-
    "integer" ## so write.TSBLIB does not do precision changes

  x
}

## interface to the Concorde algorithm
## (can only handle TSP and no neg. distances!)

tsp_concorde <- function(x, control = NULL) {
  if (!is.null(control$exe))
    warning("exe is deprecated. Use concorde_path() instead!")

  ## get parameters
  control <- .get_parameters(
    control,
    list(
      clo = "",
      exe = .find_exe(control$exe, "concorde"),
      precision = 6,
      verbose = TRUE,
      keep_files = FALSE
    )
  )



  ## get temp files and change working directory
  wd <- tempdir()
  dir <- getwd()
  setwd(wd)
  on.exit(setwd(dir))

  ### fix for Windows by Stephen Eick
  ##temp_file <- tempfile(tmpdir = wd)
  temp_file <- basename(tempfile(tmpdir = wd))

  ## file name needs to be unique
  tmp_file_in  <- paste(temp_file, ".dat", sep = "")
  tmp_file_out <- paste(temp_file, ".sol", sep = "")

  ## check x
  if (inherits(x, "TSP")) {
    #if(n_of_cities(x) < 10) MAX <- 2^15 - 1 else MAX <- 2^31 - 1
    ### MFH: Concorde may overflow with 2^31-1
    if (n_of_cities(x) < 10)
      MAX <- 2 ^ 15 - 1
    else
      MAX <- 2 ^ 28 - 1
    x <- .prepare_dist_concorde(x, MAX, precision = control$precision, verbose = control$verbose)
    ## precision is already handled!
    write_TSPLIB(x, file = tmp_file_in, precision = 0)

  } else if (inherits(x, "ETSP")) {
    ## nothing to do!
    write_TSPLIB(x, file = tmp_file_in, precision = control$precision)
  } else
    stop("Concorde only handles TSP and ETSP.")

  ## change working directory

  ## do the call and read back result
  ## we do not check return values of Concorde since they are not
  ## very consistent
  system2(
    control$exe,
    args =  paste("-x", control$clo, "-o", tmp_file_out, tmp_file_in),
    stdout = if (control$verbose)
      ""
    else
      FALSE,
    stderr = if (control$verbose)
      ""
    else
      FALSE,
  )

  if (!file.access(tmp_file_out) == 0)
    stop(
      "Concorde has not produced a result file.\nIs concorde properly installed? (see ? Concorde)\nDid Concorde finish without an error or being interupted?"
    )
  ##else cat("Concorde done.\n")

  order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
  ## remove number of nodes and add one (result starts with 0)
  order <- order[-1] + 1L

  ## tidy up
  if (!control$keep_files)
    unlink(c(tmp_file_in, tmp_file_out))
  else
    cat("File are in:", wd, "\n\n")

  order
}

## interface to the Concorde's Chained Lin-Kernighan algorithm
## (can only handle TSP, handles neg. distances)

tsp_linkern <- function(x, control = NULL) {
  if (!is.null(control$exe))
    warning("exe is deprecated. Use concorde_path() instead!")

  ## get parameters
  control <- .get_parameters(
    control,
    list(
      exe = .find_exe(control$exe, "linkern"),
      clo = "",
      precision = 6,
      verbose = TRUE,
      keep_files = FALSE
    )
  )

  ## have to set -r for small instances <8
  if (n_of_cities(x) <= 8)
    control$clo <- paste(control$clo, "-k", n_of_cities(x))

  ## check x
  if (inherits(x, "TSP")) {
    #MAX <- 2^31 - 1
    MAX <- 2 ^ 28 - 1
    x <- .prepare_dist_concorde(x, MAX, precision = control$precision, verbose = control$verbose)

  } else if (inherits(x, "ETSP")) {
    ## nothing to do
  } else
    stop("Linkern only works for TSP and ETSP.")

  ## get temp files and change working directory
  wd <- tempdir()
  dir <- getwd()
  setwd(wd)
  on.exit(setwd(dir))

  ### fix for Windows by Stephen Eick
  ##temp_file <- tempfile(tmpdir = wd)
  temp_file <- basename(tempfile(tmpdir = wd))

  ## file name needs to be unique
  tmp_file_in  <- paste(temp_file, ".dat", sep = "")
  tmp_file_out <- paste(temp_file, ".sol", sep = "")

  write_TSPLIB(x, file = tmp_file_in, precision = 0)

  ## do the call and read back result
  ## we do not check return values of Concorde since they are not
  ## very consistent
  system2(
    control$exe,
    args =  paste("-o",
      tmp_file_out, control$clo, tmp_file_in),
    stdout = if (control$verbose)
      ""
    else
      FALSE,
    stderr = if (control$verbose)
      ""
    else
      FALSE
  )

  if (!file.access(tmp_file_out) == 0)
    stop(
      "Linkern has not produced a result file.\nIs linkern properly installed?\nDid linkern finish without an error or being interrupted?"
    )
  ##else cat("Concorde done.\n")

  order <- read.table(tmp_file_out)[, 1]
  ## remove number of nodes and add one (result starts with 0)
  order <- order + as.integer(1)

  ## tidy up
  if (!control$keep_files)
    unlink(c(tmp_file_in, tmp_file_out))
  else
    cat("File are in:", wd, "\n\n")

  order
}

## path
## path
#' @rdname Concorde
#' @export
concorde_path <- local({
  .path <- NULL
  function(path) {
    if (missing(path)) {
      if (!is.null(.path))
        return(.path)
      else {
        ## find concorde and/or linkern
        p <- dirname(Sys.which("concorde"))
        if (p == "")
          p <- dirname(Sys.which("linkern"))
        if (p == "")
          stop(
            "Can not find executables for concorde or linkern. Please install the executables or set path manually."
          )
        return(p)
      }
    } else {
      if (!is.null(path)) {
        path <- normalizePath(path) ### translate all special characters
        ex <- c(
          list.files(path, pattern = "concorde",
            ignore.case = TRUE),
          list.files(path, pattern = "linkern",
            ignore.case = TRUE)
        )
        if (length(ex) < 1)
          stop(paste("no executable (concorde and/or linkern) found in",
            path))
        cat("found:", ex, "\n")
      }
      .path <<- path

      invisible(.path)

    }
  }
})


## get help page
#' @rdname Concorde
#' @export
concorde_help <- function() {
  cat(
    "The following options can be specified in solve_TSP with method \"concorde\" using clo in control:\n\n"
  )
  system2(.find_exe(NULL, "concorde"), args = "")
}

#' @rdname Concorde
#' @export
linkern_help <- function() {
  cat(
    "The following options can be specified in solve_TSP with method \"linkern\" using clo in control:\n\n"
  )
  system2(.find_exe(NULL, "linkern"), args = "")
}



## helper to find the 'concorde' executable
.find_exe <- function(exe = NULL, prog) {
  ## if not specified
  if (is.null(exe)) {
    ## was the path set ?
    if (!is.null(concorde_path()))
      exe <-
        paste(concorde_path(), .Platform$file.sep, prog, sep = "")
    ## no, so it must be in the systems execution path
    else
      exe <- prog
  }
  exe
}
