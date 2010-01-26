\name{OptionParser-class}
\Rdversion{1.1}
\docType{class}
\alias{OptionParser-class}

\title{Class "OptionParser" }
\description{	A class designed to be used in conjunction with the function \code{parse_args} to parse command line options. }
\section{Slots}{
	 \describe{
    \item{\code{usage}:}{ A character string containing the Rscript program usage. }
    \item{\code{options}:}{ A list of \code{OptionParserOption} instances. }
  }
}
\author{ 
    Trevor Davis. 

    The documentation for Python's optparse library, which this package 
    is based on, is Copyright 1990-2009, Python Software Foundation.
}

\seealso{
    \code{\link{OptionParser}}
}
\examples{
showClass("OptionParser")
}
\keyword{classes}
