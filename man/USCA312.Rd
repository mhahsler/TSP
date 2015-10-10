\name{USCA}
\alias{USCA312}
\alias{USCA312_map}
\alias{USCA312_basemap}
\alias{USCA312_coords}
\alias{USCA50}
\docType{data}
\title{USCA312/USCA50 -- 312/50 cities in the US and Canada}
\description{
The \code{USCA312} dataset contains the distances between 312 cities in the US
and Canada as an object of class \code{TSP}.  \code{USCA50} is a subset of
\code{USCA312} containing only the first 50 cities.

The \code{USCA312_map} dataset contains 
spatial data of the 312 cities.
}
\usage{
data("USCA312")
data("USCA312_map")
data("USCA50")
}
\format{
  \code{USCA312} and \code{USCA50} are objects of class \code{TSP}.
  \code{USCA312_map} contains in \code{USCA312_coords} the spatial coordinates
  of the 312 cities and in \code{USCA312_basemap} a part of the 
  map of North America.
}
\details{
For \code{USCA312_map} several packages for geographic data are needed (see
Examples section).

We want to thank Roger Bivand for his help with plotting the map.
}
\source{
John Burkardt, CITIES -- City Distance Datasets, Florida State University, 
Department of Scientific Computing
}
\author{Michael Hahsler}
\examples{
data("USCA312")
data("USCA312_map")

## calculate a tour
tour <- solve_TSP(USCA312)
tour

## load map tools
library("maps")
library("sp")
library("maptools")

## plot map
plot(as(USCA312_coords, "Spatial"), axes=TRUE)
plot(USCA312_basemap, add=TRUE, col = "gray")


## plot tour and add cities
tour_line <- SpatialLines(list(Lines(list(
    Line(USCA312_coords[c(tour, tour[1]),])), ID="1")))

plot(tour_line, add=TRUE, col = "red")
points(USCA312_coords, pch=3, cex=0.4, col="black")
}
\keyword{datasets}
