\name{USCA}
\alias{USCA312}
\alias{USCA312_GPS}
\alias{USCA50}
\docType{data}
\title{USCA312/USCA50 -- 312/50 cities in the US and Canada}
\description{
The \code{USCA312} dataset contains the distances between 312 cities in the US
and Canada as an object of class \code{TSP}.  \code{USCA50} is a subset of
\code{USCA312} containing only the first 50 cities.

The \code{USCA312_GPS} dataset contains
the location (long/lat) of the 312 cities.
}
\usage{
data("USCA312")
data("USCA312_GPS")
data("USCA50")
}
\format{
  \code{USCA312} and \code{USCA50} are objects of class \code{TSP}.
  \code{USCA312_GPS} is a data.frame with city name, long and lat.
}
\note{
We want to thank Roger Bivand for his help with plotting the map.
}
\source{
John Burkardt, CITIES -- City Distance Datasets, Florida State University,
Department of Scientific Computing
}
\author{Michael Hahsler}
\examples{
data("USCA312")

## calculate a tour
tour <- solve_TSP(USCA312)
tour

data("USCA312_GPS")
head(USCA312_GPS)

# The following examples requite the suggested package sp, maps, and maptools.
# We run the example only if the packages are installed.

if(require(sp) &&
    require(maps) &&
    require(maptools)) {

  library("sp")
  library("maps")
  library("maptools")

  data("USCA312_GPS")

  # create spatial coordinates and a basemap using WGS84 projection.
  USCA312_coords <- SpatialPointsDataFrame(cbind(USCA312_GPS$long, USCA312_GPS$lat),
      proj4string=CRS("+proj=longlat +datum=WGS84"), data = USCA312_GPS)
  USCA312_basemap <- map2SpatialLines(map("world",
          xlim=c(-166,-47), ylim=c(15,83),
          plot=FALSE), proj4string=CRS("+proj=longlat +datum=WGS84"))

  ## plot map
  plot(as(USCA312_coords, "Spatial"), axes=TRUE)
  plot(USCA312_basemap, add=TRUE, col = "gray")

  ## plot tour and add cities
  tour_line <- SpatialLines(list(Lines(list(
  Line(USCA312_coords[c(tour, tour[1]),])), ID="1")))

  plot(tour_line, add=TRUE, col = "red")
  points(USCA312_coords, pch=3, cex=0.4, col="black")
}
}
\keyword{datasets}
