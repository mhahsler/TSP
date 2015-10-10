\name{ETSP}
\alias{ETSP}
\alias{as.ETSP}
\alias{as.ETSP.matrix}
\alias{as.ETSP.data.frame}
\alias{n_of_cities.ETSP}
\alias{print.ETSP}
\alias{labels.ETSP}
\alias{image.ETSP}
\alias{plot.ETSP}
\title{Class ETSP -- Euclidean traveling salesperson problem}
\description{
  Constructor to create an instance of a Euclidean traveling salesperson
  problem (TSP) represented by city coordinates and some auxiliary methods.
}
\usage{
## constructor
ETSP(x, labels = NULL)

## coercion
as.ETSP(x)

## methods
\method{n_of_cities}{ETSP}(x)
\method{image}{ETSP}(x, order, col = gray.colors(64), \ldots)
\method{plot}{ETSP}(x, y = NULL, tour = NULL, tour_lty = 2, tour_col = 1, \ldots)
\method{labels}{ETSP}(object, \ldots)
\method{print}{ETSP}(x, \ldots)
}
\arguments{
  \item{x, object}{ an object (data.frame or matrix) to be converted into a \code{ETSP} or, for the methods, 
      an object of class \code{ETSP}.}
  \item{labels}{optional city labels. If not given, labels are taken 
      from \code{x}.}
  \item{col}{ color scheme for image. }
  \item{order}{ order of cities for the image as
      an integer vector or an object of class \code{TOUR}.}
  \item{tour, y}{ a tour to be visualized.}
  \item{tour_lty, tour_col}{ line type and color for tour. }
  \item{\ldots}{ further arguments are passed on.}
}
\details{
Objects of class \code{ETSP} are internally represented as a 
\code{matrix} objects
(use \code{as.matrix()} to get the \code{matrix} object).
}
\value{
\code{ETSP()} returns \code{x} as an object of class \code{ETSP}.

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
x <- data.frame(x = runif(20), y = runif(20), row.names = LETTERS[1:20])

## create a TSP
etsp <- ETSP(x)
etsp

## use some methods 
n_of_cities(etsp)
labels(etsp)

## plot ETSP and solution
tour <- solve_TSP(etsp)
tour
  
plot(etsp, tour, tour_col = "red")
}
\keyword{classes}% at least one, from doc/KEYWORDS
