\name{TSPLIB}
\alias{TSPLIB}
\alias{write_TSPLIB}
\alias{write_TSPLIB.TSP}
\alias{write_TSPLIB.ATSP}
\alias{write_TSPLIB.ETSP}
\alias{read_TSPLIB}
\title{Read and write TSPLIB files}
\description{
Reads and writes TSPLIB format files. TSPLIB files can be used by most
TSP solvers. Sample instances for the TSP in TSPLIB format are available 
on the TSPLIB homepage (see references).
}
\usage{
write_TSPLIB(x, file, precision = 6, inf = NULL, neg_inf = NULL)
read_TSPLIB(file, precision = 0)
}
\arguments{
  \item{x}{ an object of class \code{TSP}, \code{ATSP} or \code{ETSP}. 
    \code{NA}s are not allowed.}
  \item{file}{ file name or a \code{connection}.  }
  \item{precision}{ controls the number of decimal places used to represent
      distances (see details).  If \code{x} already is \code{integer}, this
      argument is ignored and \code{x} is used as is.}
  \item{inf}{ replacement value for \code{Inf} (TSPLIB format cannot handle
      \code{Inf}). If \code{inf} is \code{NULL}, a large value of 
      \eqn{max(x) + 2 range(x)} (ignoring infinite entries) is used. }
  \item{neg_inf}{ replacement value for \code{-Inf}. 
      If no value is specified, a small value of
      \eqn{min(x) - 2 range(x)} (ignoring infinite entries) is used. }
}
\details{
In the TSPLIB format distances are represented by integer values. Therefore, if
\code{x} contains \code{double} values (which is normal in R) the values given
in \code{x} are multiplied by \eqn{10^{precision}} before coercion to
\code{integer}. Note that therefore all results produced by programs using the
TSPLIB file as input need to be divided by \eqn{10^{precision}} (i.e., the
decimal point has to be shifted \code{precision} placed to the left).

Currently only the following \code{EDGE_WEIGHT_TYPE}s are implemented:
    \code{EXPLICIT}, \code{EUC_2D} and \code{EUC_3D}.
}
\value{
\code{read_TSPLIB} returns an object of class \code{TSP} or \code{ATSP}.
}
\author{Michael Hahsler}
\references{TSPLIB home page, 
\url{http://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/}}
\examples{
## Drilling problem from TSP
drill <- read_TSPLIB(system.file("examples/d493.tsp", package = "TSP"))
drill
tour <- solve_TSP(drill, method = "nn", two_opt = TRUE)
tour
plot(drill, tour, cex=.6, col = "red", pch= 3, main = "TSPLIB: d493")

  
## Write and read data in TSPLIB format
x <- data.frame(x=runif(5), y=runif(5))

## create TSP, ATSP and ETSP (2D)
tsp <- TSP(dist(x))
atsp <- ATSP(dist(x))
etsp <- ETSP(x[,1:2])  
  
write_TSPLIB(tsp, file="example.tsp")
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp")  
r  
  
write_TSPLIB(atsp, file="example.tsp")
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp")  
r  
  
write_TSPLIB(etsp, file="example.tsp")
#file.show("example.tsp")
r <- read_TSPLIB("example.tsp")  
r  
  
## clean up
unlink("example.tsp")
}
\keyword{file}% at least one, from doc/KEYWORDS
