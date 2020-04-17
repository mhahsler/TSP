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
  implementation are provided in \pkg{TSP}. Concorde can solve TSPs and ETSPs directly. ATSPs are
  reformulated as larger TSP's and then solved.

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

\code{\link{solve_TSP}} uses \code{\link{write_TSPLIB}} to write the TSP for Concorde and tries to
find the appropriate \code{precision} value (digits after the decimal point) to convert the provided distances into the needed integer value range. The \code{precision} value can also be specified in \code{control} in \code{\link{solve_TSP}} with method Concorde. Warning messages will alert the user if the conversion to integer values results into rounding errors that are worse then what is specified in the \code{precision} control parameter.

To get a list of all available command line options which can be used via the
\code{clo} option for \code{solve_TSP} use \code{concorde_help()} and
\code{linkern_help()}.  Several options (\option{-x}, \option{-o},
\option{-N},  \option{-Q}) are not available via \code{solve_TSP} since they
are used by the interface.

If Concorde takes too long, then you can kill the concorde process via your operating system and you can continue with R.
}
\seealso{
\code{\link{solve_TSP}},
\code{\link{write_TSPLIB}}
}
\author{Michael Hahsler}
\references{
Concorde home page,
\url{http://www.math.uwaterloo.ca/tsp/concorde/}

Concorde download page,
\url{http://www.math.uwaterloo.ca/tsp/concorde/downloads/downloads.htm}

David Applegate, Robert Bixby, Vasek Chvatal, William Cook (2001):
TSP cuts which do not conform to the template paradigm,
Computational Combinatorial Optimization, M. Junger and D. Naddef (editors),
Springer-Verlag.

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

## run concorde in verbose mode (-v) with fast cuts only (-V)
solve_TSP(USCA312, method = "concorde", control = list(clo = "-v -V"))
}
}
\keyword{documentation}% at least one, from doc/KEYWORDS
