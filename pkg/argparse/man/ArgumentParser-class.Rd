\name{ArgumentParser-class}
\Rdversion{1.1}
\docType{class}
\alias{ArgumentParser-class}

\title{Class "ArgumentParser" }
\description{	A class designed to be used in conjunction with the function \code{parse_args} to parse command line options. }
\section{Slots}{
	 \describe{
    \item{\code{usage}:}{ A character string containing the Rscript program usage. }
    \item{\code{arguments}:}{ A list of \code{ArgumentParserArgument} instances. }
  }
}
\author{ 
    Trevor Davis. 

    The documentation for Python's argparse library, which this package 
    is based on, is Copyright 2006-2009, Stven Bethard.
}

\seealso{
    \code{\link{ArgumentParser}}
}
\examples{
showClass("ArgumentParser")
}
\keyword{classes}
