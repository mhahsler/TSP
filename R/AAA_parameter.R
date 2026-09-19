#######################################################################
# Code to check parameter/control objects
# Copyright (C) 2011-2016 Michael Hahsler
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


## helper to parse parameter lists with defaults
.nodots <- function(...) {
  l <- list(...)
  if(length(l) > 0L) warning("Unknown arguments: ",
    paste(names(l), "=",l, collapse=", "))
}

.validate_city_index <- function(x, n, name = "start") {
  if (!is.numeric(x) || length(x) != 1L || anyNA(x) ||
      !is.finite(x) || x != floor(x) || x < 1L || x > n)
    stop(name, " must be a single integer between 1 and ", n, ".",
      call. = FALSE)

  as.integer(x)
}

.validate_labels <- function(labels, n) {
  if (!is.null(labels) && length(labels) != n)
    stop("labels must have one value for each city.", call. = FALSE)
  labels
}

.validate_method <- function(method) {
  if (!is.null(method) &&
      (!is.character(method) || length(method) != 1L || is.na(method)))
    stop("method must be a single non-missing value.", call. = FALSE)
  method
}

.validate_scalar <- function(x, name, type = c("numeric", "logical", "character"),
                             integer = FALSE, lower = -Inf, upper = Inf) {
  type <- match.arg(type)
  valid_type <- switch(type,
    numeric = is.numeric(x) && !is.complex(x),
    logical = is.logical(x),
    character = is.character(x)
  )

  if (!valid_type || length(x) != 1L || anyNA(x) ||
      (type != "character" && !is.finite(x)) ||
      (integer && x != floor(x)) ||
      (type != "character" && (x < lower || x > upper)))
    stop(name, " must be a single ",
      if (integer && lower >= 1) "positive integer" else
        if (integer) "non-negative integer" else type, ".", call. = FALSE)

  x
}

.validate_control <- function(control) {
  .validate_scalar(control$verbose, "verbose", "logical")
  .validate_scalar(control$two_opt, "two_opt", "logical")
  .validate_scalar(control$rep, "rep", integer = TRUE, lower = 1)

  if (!is.null(control$two_opt_repetitions))
    .validate_scalar(control$two_opt_repetitions, "two_opt_repetitions",
      integer = TRUE, lower = 1)
  if (!is.null(control$tmax))
    .validate_scalar(control$tmax, "tmax", integer = TRUE, lower = 1)
  if (!is.null(control$maxit))
    .validate_scalar(control$maxit, "maxit", integer = TRUE, lower = 1)
  if (!is.null(control$trace))
    .validate_scalar(control$trace, "trace", integer = TRUE, lower = 0)
  if (!is.null(control$temp))
    .validate_scalar(control$temp, "temp", lower = 0)
  if (!is.null(control$precision))
    .validate_scalar(control$precision, "precision", integer = TRUE, lower = 0)
  if (!is.null(control$keep_files))
    .validate_scalar(control$keep_files, "keep_files", "logical")
  if (!is.null(control$clo))
    .validate_scalar(control$clo, "clo", "character")
  if (!is.null(control$exe))
    .validate_scalar(control$exe, "exe", "character")
  if (!is.null(control$local_move) && !is.function(control$local_move))
    stop("local_move must be a function.", call. = FALSE)

  control
}


.get_parameters <- function(parameter, defaults, method = NA) {
  defaults <- c(as.list(defaults), "two_opt" = FALSE, rep = 1L)
  parameter <- as.list(parameter)

  ## add verbose
  if(is.null(defaults$verbose)) defaults$verbose <- FALSE

  if(length(parameter) != 0) {
    o <- pmatch(names(parameter), names(defaults))

    ## unknown parameter
    if(any(is.na(o))){
      warning(sprintf(ngettext(sum(is.na(o)),
         "Unknown parameter: %s",
         "Unknown parameters: %s"),
         paste(names(parameter)[is.na(o)],
           collapse = ", ")), call. = FALSE, immediate. = FALSE)
      
      #cat("Available parameter (with default values):\n")
       #print(defaults)
      # cat(rbind(names(defaults)," = ", gsub("\n"," ",as.character(defaults))),
      #   sep=c("\t"," ","\n"))
    }

    defaults[o[!is.na(o)]] <- parameter[!is.na(o)]
  }

  defaults <- .validate_control(defaults)

  if(defaults$verbose) {
    cat("Used control parameters by", sQuote(method), "\n")

    #print(defaults)
    cat(rbind(names(defaults)," = ",
      strtrim(gsub("\n"," ",as.character(defaults)), 50)),
      sep=c("\t"," ","\n"))

    cat("\n")
  }

  defaults
}
