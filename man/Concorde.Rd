\name{Concorde}
\alias{Concorde}
\alias{concorde}
\alias{concorde_path}
\alias{concorde_help}
\alias{linkern_help}
\title{Using the Concorde TSP Solver}
\description{
  The Concorde TSP Solver package contains several solvers.
  Currently, interfaces to
  the Concorde solver (Applegate et al. 2001), one of the most
  advanced and fastest TSP solvers using branch-and-cut, and
  the Chained Lin-Kernighan (Applegate et al. 2003)
  implementation are provided in \pkg{TSP}.

  The Concorde TSP Solver package is freely available for academic
  research and has to
  be obtained separately from the Concorde web site (see details).
}
\usage{
## set path for executables
concorde_path(path)

## obtain a list of command line options for the solvers
concorde_help()
linkern_help()
}
\arguments{
  \item{path}{ a character string with the path to the directory where
        the executables are installed. }
}
\details{
The code of the Concorde TSP package is not included in this package and has to
be obtained separately from the Concorde web site (see references).  Either
download the precompiled executables and place them in a suitable directory
and make them executable, or you can get the source code and compile it on your
own. \pkg{TSP} needs to know where the executables are. There are two options:
(1) use \code{concorde_path()} to set the path to the directory  containing the
executables for concorde and linkern, or (2) make sure that the executables are
in the search path stored in the \code{PATH} environment variable
(see \code{link{Sys.setenv}}).

To get a list of all available command line options which can be used via the
\code{clo} option for \code{solve_TSP} use \code{concorde_help()} and
\code{linkern_help()}.  Several options (\option{-x}, \option{-o},
\option{-N},  \option{-Q}) are not available via \code{solve_TSP} since they
are used by the interface.
}
\seealso{
\code{\link{solve_TSP}}
}
\author{Michael Hahsler}
\references{
Concorde home page,
\url{http://www.tsp.gatech.edu/concorde/}

Concorde download page,
\url{http://www.tsp.gatech.edu/concorde/downloads/downloads.htm}

David Appletgate, Robert Bixby, Vasek Chvatal, William Cook (2001):
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer.

David Applegate and William Cook and Andre Rohe (2003):
Chained Lin-Kernighan for Large Traveling Salesman Problems,
\emph{INFORMS Journal on Computing}, \bold{15}, 82--92.
}
\examples{
\dontrun{
## see if Concorde is correctly installed
concorde_path()


## set path to the Concorde executible if it is not in the search PATH
## Example:
## concorde_path("~/concorde/")

concorde_help()

data("USCA312")

## run concorde only with fast cuts (-V)
solve_TSP(USCA312, method = "concorde", control = list(clo = "-V"))
}
}
\keyword{documentation}% at least one, from doc/KEYWORDS
