\name{add_option}
\Rdversion{1.1}
\alias{add_option}
\alias{make_option}
\title{
Functions to enable our OptionParser to recognize specific command line options.
}
\description{
\code{add_option} adds a option to a prexisting \code{OptionParser} instance whereas
     \code{make_option} is used to create a list of \code{OptionParserOption} instances 
    that will be used in the \code{option_list} argument of the \code{OptionParser} function
    to create a new \code{OptionParser} instance.
}
\usage{
    make_option(short_flag, long_flag, action = "store", type = NULL, dest = NULL, 
                default = NULL, help = "", metavar = NULL)
    add_option(object, short_flag, long_flag, action="store", type=NULL, dest=NULL,
               default=NULL, help="", metavar=NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An instance of the \code{OptionParser} class}
  \item{short_flag}{A string of the desired short flag comprised of the \dQuote{-} followed by a letter.}
  \item{long_flag}{A string of the desired long flag comprised of \dQuote{--} followed by a sequence of letters. }
  \item{action}{A character string that describes the action \code{optparse} should take when it encounters an option,
      either \dQuote{store}, \dQuote{store_true}, or \dQuote{store_false}.  The default is \dQuote{store} which signifies 
      that \code{optparse} should store the specified following value if the option is found on the command string.
      \dQuote{store_true} stores \code{TRUE} if the option is found and \dQuote{store_false} stores \code{FALSE} if the option is found.
  }
  \item{type}{A character string that describes specifies which data type should be stored, 
      either \dQuote{logical}, \dQuote{integer}, \dQuote{double}, \dQuote{complex}, or \dQuote{character} 
  }
  \item{dest}{A character string that specifies what field in the returned list of \code{parse_args} 
        should \code{optparse} store option values. 
  }
  \item{default}{
      The default value \code{optparse} should use if it does not find the option on the command line. 
  }
  \item{help}{
      A character string describing the option.  \code{\%default} will be substituted by the the argument of \code{default}.
  }
  \item{metavar}{A character string that stands in for the option argument when printing help text. 
  }
}
\value{
    Both \code{make_option} and \code{add_option} return instances of class \code{OptionParserOption}.
}
\references{ Python's \code{optparse} library, which this package is inspired by,
   is described here:  \url{http://docs.python.org/library/optparse.html} }
\author{ Trevor Davis }

\seealso{
    \code{\linkS4class{OptionParser}}
    \code{\linkS4class{OptionParserOption}}
    \code{\link{parse_args}}
    \code{\link{OptionParser}}
}
\examples{
}
