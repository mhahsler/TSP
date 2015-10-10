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



## read a simple TSPLIB format file
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
      data <- data[1:(dim^2/2)]
    
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
      
    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_ROW" 
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_COL") {
      ## kill diag
      kill <- cumsum(c(1, rev(2:dim)))
      data <- data[-kill]
      
      class(data) <- "dist"
      attr(data, "Size")  <- dim
      attr(data, "Diag")  <- FALSE
      attr(data, "Upper") <- FALSE
      
    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_COL" 
      || info$EDGE_WEIGHT_FORMAT == "LOWER_ROW") {
      class(data) <- "dist"
      attr(data, "Size")  <- dim
      attr(data, "Diag")  <- FALSE
      attr(data, "Upper") <- TRUE
      
    }else if(info$EDGE_WEIGHT_FORMAT == "UPPER_DIAG_COL" 
      || info$EDGE_WEIGHT_FORMAT == "LOWER_DIAG_ROW") {
      ## kill diag
      kill <- cumsum(1:dim)
      data <- data[-kill]
      
      class(data) <- "dist"
      attr(data, "Size")  <- dim
      attr(data, "Diag")  <- FALSE
      attr(data, "Upper") <- TRUE
      
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

