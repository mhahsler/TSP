% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_tour.R
\name{cut_tour}
\alias{cut_tour}
\alias{cut_tour.TOUR}
\title{Cut a tour to form a path}
\usage{
cut_tour(x, cut, exclude_cut = TRUE)

\method{cut_tour}{TOUR}(x, cut, exclude_cut = TRUE)
}
\arguments{
\item{x}{an object of class \link{TOUR}.}

\item{cut}{the index or label of the city/cities to cut the tour.}

\item{exclude_cut}{exclude the city where we cut? If \code{FALSE}, the city
at the cut is included in the path as the first city.}
}
\value{
Returns a named vector with city ids forming the path. If multiple
cuts are used then a list with paths is returned.
}
\description{
Cuts a tour at a specified city to form a path.
}
\examples{
data("USCA50")

## find a path starting at Austin, TX
tour <- solve_TSP(USCA50)
path <- cut_tour(tour, cut = "Austin, TX", exclude_cut = FALSE)
path

## cut the tours at two cities
tour <- solve_TSP(USCA50)
path <- cut_tour(tour, cut = c("Austin, TX", "Cambridge, MA"), exclude_cut = FALSE)
path

## cut a tour at the largest gap using a dummy city
tsp <- insert_dummy(USCA50, label = "cut")
tour <- solve_TSP(tsp)

## cut tour into path at the dummy city
path <- cut_tour(tour, "cut")
path
}
\seealso{
Other TOUR: 
\code{\link{TOUR}()},
\code{\link{solve_TSP}()},
\code{\link{tour_length}()}
}
\author{
Michael Hahsler
}
\concept{TOUR}
\keyword{optimize}
