% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/USCA312.R
\docType{data}
\name{USCA}
\alias{USCA}
\alias{USCA312}
\alias{USCA312_GPS}
\alias{USCA50}
\title{USCA312/USCA50 -- 312/50 cities in the US and Canada}
\format{
\code{USCA312} and \code{USCA50} are objects of class \code{TSP}.
\code{USCA312_GPS} is a data.frame with city name, long and lat.
}
\source{
John Burkardt, CITIES -- City Distance Datasets, Florida State
University, Department of Scientific Computing
}
\description{
The \code{USCA312} dataset contains the distances between 312 cities in the
US and Canada as an object of class \code{TSP}.  \code{USCA50} is a subset
of \code{USCA312} containing only the first 50 cities.
}
\details{
The \code{USCA312_GPS} dataset contains the location (long/lat) of the 312
cities.
}
\examples{
data("USCA312")

## calculate a tour
tour <- solve_TSP(USCA312)
tour

# Visualize the tour if package maps is installed
if(require("maps")) {

  library(maps)
  data("USCA312_GPS")
  head(USCA312_GPS)

  plot((USCA312_GPS[, c("long", "lat")]), cex = .3)
  map("world", col = "gray", add = TRUE)
  polygon(USCA312_GPS[, c("long", "lat")][tour,], border = "red")
}
}
\author{
Michael Hahsler
}
\keyword{datasets}
