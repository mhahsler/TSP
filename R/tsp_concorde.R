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




## prepare distances as integers in the appropriate range [0..MAX]
.prepare_dist_concorde <- function(x, MAX, precision) {
  ## handle inf
  x <- .replaceInf(x)

  ## fix neg. values
  min_x <- min(x)
  if(min_x<0) {
    warning("TSP contains negative distances. Shifting distances by subtracting the minimum.",
      immediate. = TRUE)
    x <- x - min_x
  }

  ## get max (excluding) to check for possible integer overflows
  max_x <- max(x)
  prec <- floor(log10(MAX / max_x))
  x <- x * 10^prec

  if(prec < precision && any((x %% 1) != 0))
    warning(paste0("Concorde/Linken can only handle distances represented as integers. Converting the provided distances to integers with precison ", prec, ". This may lead to rounding errors."),
      immediate. = TRUE)

  storage.mode(x) <- "integer" ## so write.TSBLIB does not do precision changes

  x
}

## interface to the Concorde algorithm
## (can only handle TSP and no neg. distances!)

tsp_concorde <- function(x, control = NULL){

  if(!is.null(control$exe)) warning("exe is deprecated. Use concorde_path() instead!")

  ## get parameters
  control <- .get_parameters(control, list(
    clo = "",
    exe = .find_exe(control$exe, "concorde"),
    precision = 6,
    verbose = TRUE,
    keep_files = FALSE
  ))

  ## check x
  if(inherits(x, "TSP")){
    #if(n_of_cities(x) < 10) MAX <- 2^15 - 1 else MAX <- 2^31 - 1
    ### MFH: concorde may overflow with 2^31-1
    if(n_of_cities(x) < 10) MAX <- 2^15 - 1 else MAX <- 2^28 - 1
    x <- .prepare_dist_concorde(x, MAX, control$precision)

  }else if(inherits(x, "ETSP")) {
    ## nothing to do!
  }else stop("Concorde only handles TSP and ETSP.")


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

  ## precision is already handled!
  write_TSPLIB(x, file = tmp_file_in, precision = 0)

  ## change working directory

  ## do the call and read back result
  ## we do not check return values of Concorde since they are not
  ## very consistent
  system2(control$exe,
    args =  paste("-x", control$clo, "-o", tmp_file_out, tmp_file_in),
    stdout = if(control$verbose) "" else FALSE,
    stderr = if(control$verbose) "" else FALSE,
    )


  if(!file.access(tmp_file_out) == 0)
    stop("Concorde has not produced a result file.\nIs concorde properly installed? (see ? Concorde)\nDid Concorde finish without an error or being interupted?")
  ##else cat("Concorde done.\n")

  order <- scan(tmp_file_out, what = integer(0), quiet = TRUE)
  ## remove number of nodes and add one (result starts with 0)
  order <- order[-1] + 1L

  ## tidy up
  if(!control$keep_files) unlink(c(tmp_file_in, tmp_file_out))
  else cat("File are in:", wd, "\n\n")

  order
}

## interface to the Concorde's Chained Lin-Kernighan algorithm
## (can only handle TSP, handles neg. distances)

tsp_linkern <- function(x, control = NULL){

  if(!is.null(control$exe))
    warning("exe is deprecated. Use concorde_path() instead!")

  ## get parameters
  control <- .get_parameters(control, list(
    exe = .find_exe(control$exe, "linkern"),
    clo = "",
    precision = 6,
    verbose = TRUE,
    keep_files = FALSE
  ))

  ## have to set -r for small instances <8
  if(n_of_cities(x) <=8)
    control$clo <- paste(control$clo, "-k", n_of_cities(x))

  ## check x
  if(inherits(x, "TSP")) {

    #MAX <- 2^31 - 1
    MAX <- 2^28 - 1
    x <- .prepare_dist_concorde(x, MAX, control$precision)

  }else if(inherits(x, "ETSP")) {
    ## nothing to do
  } else stop("Linkern only works for TSP and ETSP.")

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
  system2(control$exe, args =  paste("-o",
    tmp_file_out, control$clo, tmp_file_in),
    stdout = if(control$verbose) "" else FALSE,
    stderr = if(control$verbose) "" else FALSE)

  if(!file.access(tmp_file_out) == 0)
    stop("Linkern has not produced a result file.\nIs linkern properly installed?\nDid linkern finish without an error or being interrupted?")
  ##else cat("Concorde done.\n")

  order <- read.table(tmp_file_out)[,1]
  ## remove number of nodes and add one (result starts with 0)
  order <- order + as.integer(1)

  ## tidy up
  if(!control$keep_files) unlink(c(tmp_file_in, tmp_file_out))
  else cat("File are in:", wd, "\n\n")

  order
}


## get help page
concorde_help <- function() {
  cat("The following options can be specified in solve_TSP with method \"concorde\" using clo in control:\n\n")
  system2(.find_exe(NULL, "concorde"), args = "")
}

linkern_help <- function() {
  cat("The following options can be specified in solve_TSP with method \"linkern\" using clo in control:\n\n")
  system2(.find_exe(NULL, "linkern"), args = "")
}

## path
concorde_path <- local({
  .path <- NULL
  function(path){
    if(missing(path)) {
      if(!is.null(.path)) return(.path)
      else {
        ## find concorde and/or linkern
        p <- dirname(Sys.which("concorde"))
        if(p == "") p <- dirname(Sys.which("linkern"))
        if(p == "") stop("Can not find executables for concorde or linkern. Please install the executables or set path manually.")
        return(p)
      }
    } else {
      if(!is.null(path)) {
        path <- normalizePath(path) ### translate all special characters
        ex <- c(list.files(path, pattern = "concorde",
          ignore.case = TRUE),
          list.files(path, pattern = "linkern",
            ignore.case = TRUE))
        if(length(ex) < 1)
          stop(paste("no executable (concorde and/or linkern) found in",
            path))
        cat("found:", ex, "\n")
      }
      .path <<- path

      invisible(.path)

    }
  }
})


## helper to find the concorde executable
.find_exe <- function(exe = NULL, prog) {
  ## if not specified
  if(is.null(exe)) {
    ## was the path set ?
    if(!is.null(concorde_path()))
      exe <- paste(concorde_path(), .Platform$file.sep, prog, sep ="")
    ## no, so it must be in the systems execution path
    else exe <- prog
  }
  exe
}

