\name{solve_TSP}
\alias{solve_TSP}
\alias{solve_TSP.TSP}
\alias{solve_TSP.ATSP}
\title{TSP solver interface}
\description{
  Common interface to all TSP solvers in this package.
}
\usage{
solve_TSP(x, method = NULL, control = NULL, ...)
}
\arguments{
  \item{x}{ the TSP given as an object of class \code{TSP}, \code{ATSP}
    or \code{ETSP}.}
  \item{method}{ method to solve the TSP (default: arbitrary insertion 
    algorithm with two_opt refinement. }
  \item{control}{ a list of arguments passed on to the TSP solver
    selected by \code{method}.}
  \item{...}{ additional arguments are added to \code{control}.}
}
\details{
Treatment of \code{NA}s and infinite values in \code{x}: 
\code{TSP} and \code{ATSP} contain distances and \code{NA}s are not allowed. 
\code{Inf} is allowed and can be used to model the missing edges in 
incomplete graphs 
(i.e., the distance between the two objects is infinite). 
Internally, \code{Inf} is replaced by a large value given by 
\eqn{max(x) + 2 range(x)}.
Note that the solution might still place the two objects next to each other 
(e.g., if \code{x} contains several unconnected subgraphs) 
which results in a path length of \code{Inf}.   
  
All heuristics can be used with the control arguments \code{repetitions} 
(uses the best from that many repetitions with random starts)
and \code{two_opt} (a logical indicating if two_opt refinement should be 
performed). If several repetitions are done (this includes method \code{"repetitive_nn"}) then \pkg{foreach} is used so they
can be performed in parallel on multiple cores/machines. 
To enable parallel execution an appropriate parallel backend needs to be registered (e.g., load \pkg{doParallel} and register it with
\code{registerDoParallel()}).

\code{ETSP} are currently solved by first calculating a dissimilarity matrix (a 
\code{TSP}). Only concorde and linkern can solve the TSP directly on the 
\code{ETSP}. 

Currently the following methods are available: 
\describe{
\item{\code{"identity"}, \code{"random"}}{return a tour representing the order 
    in the data (identity order) or a random order.}
\item{\code{"nearest_insertion"}, 
    \code{"farthest_insertion"},
    \code{"cheapest_insertion"}, 
    \code{"arbitrary_insertion"}
    }{ 
    Nearest,
    farthest, cheapest and arbitrary insertion algorithms for a symmetric and
    asymmetric TSP (Rosenkrantz et al. 1977).  
        
    The distances between cities are stored in a distance matrix \eqn{D} with
    elements \eqn{d(i,j)}.  All insertion algorithms start with a tour
    consisting of an arbitrary city and choose in each step a city \eqn{k} not
    yet on the tour. This city is inserted into the existing tour between two
    consecutive cities \eqn{i} and \eqn{j}, such that \deqn{d(i,k) + d(k,j) -
    d(i,j)} is minimized. The algorithms stops when all cities are on the tour.

    The nearest insertion algorithm chooses city \eqn{k} in each step as the
    city which is \emph{nearest} to a city on the tour.  

    For farthest insertion, the city \eqn{k} is chosen in each step as the city
    which is \emph{farthest} to any city on the tour.  

    Cheapest insertion chooses the city \eqn{k} such that the cost of inserting
    the new city (i.e., the increase in the tour's length) is minimal.  

    Arbitrary insertion chooses the city \eqn{k} randomly from all cities not
    yet on the tour.

    Nearest and cheapest insertion tries to build the tour using cities which
    fit well into the partial tour constructed so far.  The idea behind behind
    farthest insertion is to link cities far away into the tour fist to
    establish an outline of the whole tour early.

    Additional control options: 
    \describe{
        \item{\code{start}}{index of the
	      first city (default: random city).}
    }
}


\item{\code{"nn", "repetitive_nn"}}{ 
    Nearest neighbor and repetitive nearest neighbor algorithms for
    symmetric and asymmetric TSPs (Rosenkrantz et al. 1977).

    The algorithm starts with a tour containing a random city. Then the
    algorithm always adds to the last city on the tour the nearest not yet
    visited city. The algorithm stops when all cities are on the tour. 

    Repetitive nearest neighbor constructs a nearest neighbor tour for each
    city as the starting point and returns the shortest tour found.

    Additional control options: 
\describe{
      \item{\code{start}}{ index of the first city (default: random city). }
}
}

\item{\code{"two_opt"}}{Two edge exchange improvement procedure (Croes 1958).
    
    This is a tour refinement procedure which systematically exchanges two 
    edges in the graph represented
    by the distance matrix till no improvements are possible. Exchanging two
    edges is equal to reversing part of the tour. The resulting tour is called
    \emph{2-optimal.} 

    This method can be applied to tours created by other methods or used as
    its own method. In this case improvement starts with a random tour.
    
    Additional control options: 
    \describe{ 
    \item{\code{tour}}{ an existing tour which should be improved.
    If no tour is given, a random tour is used. }
    \item{\code{two_opt_repetitions}}{ number of times to try 
        two_opt with a different initial
        random tour (default: 1). }
    }
    }

\item{\code{"concorde"}}{Concorde algorithm (Applegate et al. 2001).

    Concorde is an advanced exact TSP solver for \emph{only symmetric} TSPs
    based on branch-and-cut.  The program is not included in this package and
    has to be obtained and installed separately (see
    \code{\link{Concorde}}).

    Additional control options: 
    \describe{
    \item{\code{exe}}{ a character string containing the path to the executable 
    (see \code{Concorde}).}

    \item{\code{clo}}{ a character string containing command line options for
    Concorde, e.g., \code{control = list(clo = "-B -v")}. See
    \code{concorde_help} on how to obtain a complete list of available
    command line options.} 

    \item{\code{precision}}{ an integer which controls the number of
    decimal places used for the internal representation of distances in
    Concorde. The values given in \code{x} are multiplied by
    \eqn{10^{precision}} before being passed on to Concorde. Note that
    therefore the results produced by Concorde (especially lower and upper
    bounds) need to be divided by \eqn{10^{precision}} (i.e., the decimal point
    has to be shifted \code{precision} placed to the left). The
    interface to Concorde uses \code{write_TSPLIB} (see there for more
    information).}
    }
    }

\item{\code{"linkern"}}{Concorde's Chained Lin-Kernighan heuristic 
    (Applegate et al. 2003).

    The Lin-Kernighan (Lin and Kernighan 1973) 
    heuristic uses variable \eqn{k} edge exchanges to
    improve an initial tour.  The program is not included in this package and
    has to be obtained and installed separately (see
    \code{\link{Concorde}}).
    
    Additional control options: see Concorde above. 
  }
  
%\item{\code{"grasp"}}{ 
%    Greedy Randomized Adaptive Search Procedure (L.S. Pitsoulis
%    and M.G.C. Resende 2001).
%
%    GRASP is a metaheuristic for combinatorial optimization. It is
%    implemented as a multistart procedure, in which in each iteration a
%    random route is constructed (construction phase). To find an
%    optimum the route is improved by a local search algorithm
%    (implemented as k-opt neighborhood search). This is
%    repeatedly done until a stopping criterium is reached (i.e., a number of
%    iterations).
%
%    Additional control options:
%    \describe{
%      \item{\code{start}}{index of the first city (default: random
%	city).}
%      \item{\code{iterations}}{an integer containing the number of
%	iterations after the algorithm stops (default: 100 iterations).}
%      \item{\code{max_i}}{an integer representing the max. steps to find
%	a better solution in local search (default: 100 steps).}
%      \item{\code{k}}{an integer needed to change k edges of the tour in
%	local search (default: 3 cities).}
%    }
%  }
  
}
}
\value{
An object of class \code{TOUR}.
}
\seealso{
\code{\link{TOUR}},
\code{\link{TSP}},
\code{\link{ATSP}},
\code{\link{write_TSPLIB}},
\code{\link{Concorde}}.
}
\author{Michael Hahsler}
\references{
Concorde home page, \url{http://www.tsp.gatech.edu/concorde/}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001): TSP cuts
which do not conform to the template paradigm, Computational Combinatorial
Optimization, M. Junger and D. Naddef (editors), Springer.

D. Applegate, W. Cook and A. Rohe (2003): Chained Lin-Kernighan for Large
Traveling Salesman Problems.  \emph{INFORMS Journal on Computing,
15(1):82--92.}

G.A. Croes (1958): A method for solving traveling-salesman problems.
\emph{Operations Research, 6(6):791--812.}

S. Lin and B. Kernighan (1973): An effective heuristic algorithm for the
traveling-salesman problem. \emph{Operations Research, 21(2): 498--516.}

%L.S. Pitsoulis and M.G.C. Resende (2001): Greedy randomized adaptive search
%procedures.  In P.M. Pardalos and M.G.C. Resende, editors, Handbook of Applied
%Optimization, pp. 168--181.

D.J. Rosenkrantz, R. E. Stearns, and Philip M. Lewis II (1977): An analysis of
several heuristics for the traveling salesman problem.  \emph{SIAM Journal on
Computing, 6(3):563--581.}
}
\examples{
## solve a simple Euclidean TSP (using the default method)
etsp <- ETSP(data.frame(x = runif(20), y = runif(20)))
tour <- solve_TSP(etsp)
tour
tour_length(tour)
plot(etsp, tour)
  
  
## compare methods
data("USCA50")
USCA50
methods <- c("identity", "random", "nearest_insertion", 
  "cheapest_insertion", "farthest_insertion", "arbitrary_insertion", 
  "nn", "repetitive_nn", "two_opt")

## calculate tours
tours <- lapply(methods, FUN = function(m) solve_TSP(USCA50, method = m))
names(tours) <- methods

## use the external solver which has to be installed separately
\dontrun{
tours$concorde  <- solve_TSP(USCA50, method = "concorde")
tours$linkern  <- solve_TSP(USCA50, method = "linkern")
}

## register a parallel backend to perform repetitions in parallel 
\dontrun{
library(doParallel)
registerDoParallel()
}
  
## add some tours using repetition and two_opt refinements   
tours$'nn+two_opt' <- solve_TSP(USCA50, method="nn", two_opt=TRUE)  
tours$'nn+rep_10' <- solve_TSP(USCA50, method="nn", rep=10)  
tours$'nn+two_opt+rep_10' <- solve_TSP(USCA50, method="nn", two_opt=TRUE, rep=10)  
tours$'arbitrary_insertion+two_opt' <- solve_TSP(USCA50)  
  
## show first tour
tours[[1]]

## compare tour lengths
opt <- 14497 # obtained by Concorde
tour_lengths <- c(sort(sapply(tours, tour_length), decreasing = TRUE), 
  optimal = opt) 
dotchart(tour_lengths/opt*100-100, xlab = "percent excess over optimum")
}
\keyword{optimize}% at least one, from doc/KEYWORDS
