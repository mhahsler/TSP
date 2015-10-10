\name{insert_dummy}
\alias{insert_dummy}
\title{Insert dummy cities into a distance matrix}
\description{
  Inserts dummy cities into objects of class \code{TSP} or \code{ATSP}.  A
  dummy city has the same, constant distance (0) to all other cities and is
  infinitely far from other dummy cities. A dummy city can be used to transform
  a shortest Hamiltonian path problem (i.e., finding an optimal linear order)
  into a shortest Hamiltonian cycle problem which can be solved by a TSP
  solvers (Garfinkel 1985).
  
  Several dummy cities can be used together with a TSP solvers to perform
  rearrangement clustering (Climer and Zhang 2006).
  }
\usage{
insert_dummy(x, n = 1, const = 0, inf = Inf, label =  "dummy")
}
\arguments{
  \item{x}{an object of class \code{TSP} or \code{ATSP}.}
  \item{n}{number of dummy cities.}
  \item{const}{distance of the dummy cities to all other cities.}
  \item{inf}{distance between dummy cities.}
  \item{label}{ labels for the dummy cities. If only one label is given, it is
        reused for all dummy cities.}
}
\details{
  The dummy cities are inserted after the other cities in \code{x}. 

  A \code{const} of 0 is guaranteed to work if the TSP finds the optimal
  solution. For heuristics returning suboptimal solutions, a higher
  \code{const} (e.g., \code{2 * max\{x\}}) might provide better results.
}
\seealso{
\code{\link{TSP}},
\code{\link{ATSP}}
}
\author{Michael Hahsler}
\references{
Sharlee Climer, Weixiong Zhang (2006): Rearrangement Clustering: Pitfalls,
Remedies, and Applications, \emph{Journal of Machine Learning Research}
\bold{7}(Jun), pp. 919--943.

R.S. Garfinkel (1985): Motivation and modelling (chapter 2). In: E. L. Lawler,
J. K. Lenstra, A.H.G. Rinnooy Kan, D.  B. Shmoys (eds.) The traveling salesman
problem - A guided tour of combinatorial optimization, Wiley \& Sons.

}
\examples{
## make runs comparable
set.seed(4444)

data("iris")
tsp <- TSP(dist(iris[-5]))

## insert 2 dummy cities
tsp_dummy <- insert_dummy(tsp, n = 2, label = "boundary")

## get a solution for the TSP
tour <- solve_TSP(tsp_dummy)

## plot the distance matrix
image(tsp_dummy, tour)

## draw lines where the dummy cities are located
abline(h = which(labels(tour)=="boundary"), col = "red")
abline(v = which(labels(tour)=="boundary"), col = "red")
}
\keyword{manip}
