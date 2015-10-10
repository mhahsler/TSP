\name{ATSP}
\alias{ATSP}
\alias{as.ATSP}
\alias{as.ATSP.dist}
\alias{as.ATSP.matrix}
\alias{n_of_cities.ATSP}
\alias{print.ATSP}
\alias{labels.ATSP}
\alias{image.ATSP}
\alias{as.matrix}
\alias{as.matrix.ATSP}
\title{Class ATSP -- Asymmetric traveling salesperson problem}
\description{
  Constructor to create an instance of the 
  asymmetric traveling salesperson problem (ATSP) and 
  some auxiliary methods.
}
\usage{
## constructor
ATSP(x, labels = NULL)

## coercion
as.ATSP(x)

## methods
\method{n_of_cities}{ATSP}(x)
\method{image}{ATSP}(x, order, col = gray.colors(64), \ldots)
\method{labels}{ATSP}(object, \ldots)
\method{print}{ATSP}(x, \ldots)
}
\arguments{
  \item{x, object}{ an object (a square matrix)
      to be converted into an \code{ATSP} or, for the methods, 
      an object of class \code{ATSP}.}
  \item{labels}{optional city labels. If not given, labels are taken 
      from \code{x}.}
  \item{col}{ color scheme for image. }
  \item{order}{ order of cities as an integer vector or an object of class
      \code{TOUR}.}
  \item{\ldots}{ further arguments are passed on.}
      }
\details{
Objects of class \code{ATSP} are internally represented by a matrix 
(use \code{as.matrix()} to get just the matrix).

ATSPs can be transformed into (larger) symmetric TSPs using 
\code{reformulate_ATSP_as_TSP()}.
}
\value{
\code{ATSP()} returns \code{x} as an object of class \code{ATSP}.

\code{n_of_cities()} returns the number of cities in \code{x}.

\code{labels()} returns a vector with the names of the cities in \code{x}.
}
\seealso{
\code{\link{TOUR}},
\code{\link{insert_dummy}},
\code{\link{tour_length}},
\code{\link{reformulate_ATSP_as_TSP}},
\code{\link{solve_TSP}}.
}
\author{Michael Hahsler}
\examples{
data <- matrix(runif(10^2), ncol = 10, dimnames = list(1:10, 1:10))

atsp <- ATSP(data)
atsp

## use some methods
n_of_cities(atsp)
labels(atsp)

## calculate a tour
tour <- solve_TSP(atsp, method = "nn")
tour
  
tour_length(tour)

image(atsp, tour)
}
\keyword{classes}% at least one, from doc/KEYWORDS
