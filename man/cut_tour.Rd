\name{cut_tour}
\alias{cut_tour}
\alias{cut_tour.TOUR}
\title{Cut a tour to form a path}
\description{
Cuts a tour at a specified city to form a path.
}
\usage{
cut_tour(x, cut, exclude_cut = TRUE)
}
\arguments{
  \item{x}{an object of class \code{TOUR}.}
  \item{cut}{the index or label of the city to cut the tour.} 
  \item{exclude_cut}{exclude the city where we cut? If \code{FALSE},
    the city at the cut is included in the path as the first city.} 
}
\seealso{
\code{\link{TOUR}}.
}
\author{Michael Hahsler}
\examples{
data("USCA50")

tsp <- insert_dummy(USCA50, label = "cut")
tour <- solve_TSP(tsp)

## cut tour into path at the dummy city
path <- cut_tour(tour, "cut")

labels(path)
}
\keyword{optimize}
