\name{TOUR}
\alias{TOUR}
\alias{print.TOUR}
\alias{as.TOUR}
\alias{as.TOUR.integer}
\title{Class TOUR -- Solution to a traveling salesperson problem}
\description{
  Class to store the solution of a TSP. Objects of this class are returned by
  TSP solvers in this package.  Essentially, an object of class \code{TOUR} is
  a permutation vector containing the order of cities to visit.
}
\usage{
## conctructor
TOUR(x, method=NA, tsp=NULL)

## coercion
as.TOUR(object)

## methods
\method{print}{TOUR}(x, \ldots)
}
\arguments{
  \item{x}{ an integer permutation vector or, for the methods 
      an object of class \code{TOUR}}.
  \item{object}{ data (an integer vector) which can be coerced to \code{TOUR}.}.
  \item{method}{ character string; method used to create the tour.}
  \item{tsp}{ \code{TSP} object the tour applies to. If available then the
  tour will include the tour length. Also the labes of the cities will be 
  available in the tour (otherwise the labels of \code{x} are used).}
  \item{\ldots}{ further arguments are passed on.}
}
\details{
Since an object of class \code{TOUR} is an integer vector, it can be subsetted
as an ordinary vector or coerced to an integer vector using
\code{as.integer()}. It also contains the names of the objects as labels.
Additionally, \code{TOUR} has the following attributes: 
\code{"method"}, \code{"tour_length"}.

For most functions, e.g., \code{tour_length} or \code{image}, the
\code{TSP/ATSP} object used to find the tour is still needed, since the tour
does not contain the distance information.
}
\seealso{
\code{\link{TSP}},
\code{\link{ATSP}},
\code{\link{tour_length}},
\code{\link{image}}.
}
\author{Michael Hahsler}
\examples{
TOUR(1:10)

## calculate a tour
data("USCA50")
tour <- solve_TSP(USCA50)
tour

## get permutation vector
as.integer(tour)

## get tour length directly from tour
attr(tour, "tour_length")
  
## show labels
labels(tour)
}
\keyword{classes}% at least one, from doc/KEYWORDS
