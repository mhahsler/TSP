\name{reformulate_ATSP_as_TSP}
\alias{reformulate_ATSP_as_TSP}
\title{Reformulate a ATSP as a symmetric TSP}
\description{
A ATSP can be formulated as a symmetric TSP by doubling the number of
cities (Jonker and Volgenant 1983). The solution of the TSP 
also represents the solution of the original ATSP.
}
\usage{
reformulate_ATSP_as_TSP(x, infeasible = Inf, cheap = -Inf)
}
\arguments{
  \item{x}{an ATSP.}
  \item{infeasible}{value for infeasible connections.}
  \item{cheap}{value for distance between a city and its corresponding 
      dummy city.}
}
\details{
To reformulate the ATSP as a TSP, for each city a dummy city (e.g, for 'New
York' a dummy city 'New York*') is added. Between each city and its
corresponding dummy city a negative or very small distance with value
\code{cheap} is used.  This makes sure that each cities always occurs in the
solution together with its dummy city.  The original distances are used between
the cities and the dummy cities, where each city is responsible for the
distance going to the city and the dummy city is responsible for the distance
coming from the city. The distances between all cities and the distances
between all dummy cities are set to \code{infeasible}, a very large value which
makes the infeasible.

%The result is a distance matrix for the TSP of the following form:
%
%\eqn{\begin{pmatrix}\infty & D'^T \\ D' & \infty \end{pmatrix},}{}
%
%where \eqn{D'} is the original distance matrix with
%the diagonal replaced by the value for the  \code{cheap_link}.
}
\value{
a TSP object.
}
\references{
Jonker, R. and Volgenant, T. (1983): Transforming asymmetric into symmetric
traveling salesman problems, \emph{Operations Research Letters, 2, 161--163.}
}
\seealso{
\code{\link{ATSP}},
\code{\link{TSP}}.
}
\author{Michael Hahsler}
\examples{
data("USCA50")

## set the distances towards Austin to zero which makes it a ATSP
austin <- which(labels(USCA50) == "Austin, TX")
atsp <- as.ATSP(USCA50)
atsp[, austin] <- 0

## reformulate as a TSP
tsp <- reformulate_ATSP_as_TSP(atsp)
labels(tsp)

## create tour (now you could use Concorde or LK)
tour_atsp <- solve_TSP(tsp, method="nn")
head(labels(tour_atsp), n = 10)
tour_atsp
## Note that the tour has a lenght of -Inf since the reformulation created
## some -Inf distances

## filter out the dummy cities (we specify tsp so the tour lenght is 
## recalculated)
tour <- TOUR(tour_atsp[tour_atsp <= n_of_cities(atsp)], tsp = atsp)
tour
}
\keyword{optimize}
