\name{TSP}
\alias{TSP}
\alias{as.TSP}
\alias{as.TSP.matrix}
\alias{as.TSP.dist}
\alias{n_of_cities}
\alias{n_of_cities.TSP}
\alias{print.TSP}
\alias{labels.TSP}
\alias{image.TSP}
\title{Class TSP -- Symmetric traveling salesperson problem}
\description{
  Constructor to create an instance of a symmetric traveling salesperson
  problem (TSP) and some auxiliary methods.
}
\usage{
## constructor
TSP(x, labels = NULL)

## coercion
as.TSP(x)

## methods
\method{n_of_cities}{TSP}(x)
\method{image}{TSP}(x, order, col = gray.colors(64), \ldots)
\method{labels}{TSP}(object, \ldots)
\method{print}{TSP}(x, \ldots)
}
\arguments{
  \item{x, object}{ an object (currently \code{dist} or a symmetric
      matrix) to be converted into a \code{TSP} or, for the methods, 
      an object of class \code{TSP}.}
  \item{labels}{optional city labels. If not given, labels are taken 
      from \code{x}.}
  \item{col}{ color scheme for image. }
  \item{order}{ order of cities for the image as
      an integer vector or an object of class \code{TOUR}.}
  \item{\ldots}{ further arguments are passed on.}
      }
\details{
Objects of class \code{TSP} are internally represented as \code{dist} objects
(use \code{as.dist()} to get the \code{dist} object).
}
\value{
\code{TSP()} returns \code{x} as an object of class \code{TSP}.

\code{n_of_cities()} returns the number of cities in \code{x}.

\code{labels()} returns a vector with the names of the cities in \code{x}.
}
\seealso{
\code{\link{TOUR}},
\code{\link{insert_dummy}},
\code{\link{tour_length}},
\code{\link{solve_TSP}}.
}
\author{Michael Hahsler}
\examples{
data("iris")
d <- dist(iris[-5])

## create a TSP
tsp <- TSP(d)
tsp

## use some methods 
n_of_cities(tsp)
labels(tsp)
image(tsp)
}
\keyword{classes}% at least one, from doc/KEYWORDS
