\name{OptionParserOption-class}
\Rdversion{1.1}
\docType{class}
\alias{OptionParserOption-class}

\title{Class "OptionParserOption"}
\description{A class that defines how \code{optparse} should react to a given option it finds on the command line.}
\section{Slots}{
	 \describe{
    \item{\code{short_flag}:}{A string of the desired short flag comprised of the \dQuote{-} followed by a letter. }
    \item{\code{long_flag}:}{A string of the desired long flag comprised of \dQuote{--} followed by a sequence of letters. }
    \item{\code{action}:}{A character string that describes the action \code{optparse} should take when it encounters an option,
                     either \dQuote{store}, \dQuote{store_true}, or \dQuote{store_false}. }
    \item{\code{type}:}{A character string that describes specifies which data type should be stored, 
                        either \dQuote{logical}, \dQuote{integer}, \dQuote{double}, \dQuote{complex}, or dQuote{character} }
    \item{\code{dest}:}{A character string that specifies which list field \code{optparse} will store option values. }
    \item{\code{default}:}{ The default value \code{optparse} should use if it does not find the option on the command line. }
    \item{\code{help}:}{A character string describing the option. }
    \item{\code{metavar}:}{A character string that stands in for the option argument when printing help text. }
  }
}

\author{ Trevor Davis}

\seealso{
    \code{\linkS4class{OptionParser}} relies on this class, 
     use \code{\link{make_option}} and \code{\link{add_option}} to create an instance of this class.
}
\examples{
showClass("OptionParserOption")
}
\keyword{classes}
