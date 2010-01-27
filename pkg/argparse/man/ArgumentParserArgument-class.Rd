\name{ArgumentParserArgument-class}
\Rdversion{1.1}
\docType{class}
\alias{ArgumentParserArgument-class}

\title{Class "ArgumentParserArgument"}
\description{A class that defines how \code{argparse} should react to a given option it finds on the command line.}
\section{Slots}{
	 \describe{
    \item{\code{short_flag}:}{A string of the desired short flag comprised of the \dQuote{-} followed by a letter. }
    \item{\code{long_flag}:}{A string of the desired long flag comprised of \dQuote{--} followed by a sequence of letters. }
    \item{\code{action}:}{A character string that describes the action \code{argparse} should take when it encounters an option,
                     either \dQuote{store}, \dQuote{store_true}, or \dQuote{store_false}. }
    \item{\code{type}:}{A character string that describes specifies which data type should be stored, 
                        either \dQuote{logical}, \dQuote{integer}, \dQuote{double}, \dQuote{complex}, or dQuote{character} }
    \item{\code{dest}:}{A character string that specifies which list field \code{argparse} will store option values. }
    \item{\code{default}:}{ The default value \code{argparse} should use if it does not find the option on the command line. }
    \item{\code{help}:}{A character string describing the option. }
    \item{\code{metavar}:}{A character string that stands in for the option argument when printing help text. }
    \item{\code{nargs}:}{An positive integer or either '*', '+', '?' that helps determine how many arguments 
                        gets assigned to each option variable. }
  }
}

\author{ 
    Trevor Davis. 

    The documentation for Python's argparse library, which this package 
    is based on, is Copyright 2006-2009, Stven Bethard.
}

\seealso{
    \code{\linkS4class{ArgumentParser}} relies on this class, 
     use \code{\link{make_argument}} and \code{\link{add_argument}} to create an instance of this class.
}
\examples{
showClass("ArgumentParserArgument")
}
\keyword{classes}
