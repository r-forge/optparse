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
\author{ Trevor Davis}

\seealso{
    \code{\link{OptionParser}}
}
\examples{
showClass("OptionParser")
}
\keyword{classes}
