\name{tour_length}
\alias{tour_length}
\alias{tour_length.TOUR}
\alias{tour_length.TSP}
\alias{tour_length.ATSP}
\alias{tour_length.ETSP}
\title{Calculate the length of a tour}
\description{
Calculate the length of a tour given a TSP and an order.
  }
\usage{
tour_length(x, ...)
\method{tour_length}{TOUR}(x, tsp = NULL, \ldots)
}
\arguments{
  \item{x}{an object of class \code{TOUR}.}
  \item{tsp}{a TSP object of class \code{TSP}, \code{ATSP} or \code{ETSP}.}
  \item{\ldots}{further arguments are currently unused.}
}
\details{
If no \code{tsp} is given, then the tour length stored in \code{x} 
as attribute \code{"tour_length"} is returned. 
If \code{tsp} is given then the tour length is recalculated.  
  
If a distance in the tour is infinite, the result is also infinite. If 
the tour contains positive and negative infinite distances then the method
returns \code{NA}.
}
\seealso{
\code{\link{TOUR}},
\code{\link{TSP}},
\code{\link{ATSP}} and
\code{\link{ETSP}}.
}
\author{Michael Hahsler}
\examples{
data("USCA50")

## original order
tour_length(solve_TSP(USCA50, method="identity"))

## length of a manually created (random) tour
tour <- TOUR(sample(seq(n_of_cities(USCA50))))
tour
tour_length(tour)
tour_length(tour, USCA50)
}
\keyword{optimize}
