\name{add_argument}
\Rdversion{1.1}
\alias{add_argument}
\alias{make_argument}
\title{
Functions to enable our ArgumentParser to recognize specific command line argument.
}
\description{
\code{add_argument} adds a argument to a prexisting \code{ArgumentParser} instance whereas
     \code{make_argument} is used to create a list of \code{ArgumentParserOption} instances 
    that will be used in the \code{argument_list} argument of the \code{ArgumentParser} function
    to create a new \code{ArgumentParser} instance.
}
\usage{
    make_argument(opt_str, action = "store", type = NULL, dest = NULL, 
                default = NULL, help = "", metavar = NULL)
    add_argument(object, opt_str, action="store", type=NULL, dest=NULL,
               default=NULL, help="", metavar=NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An instance of the \code{ArgumentParser} class}
  \item{opt_str}{A character vector containing the string of the desired long flag comprised of \dQuote{--} followed by a letter and then a sequence of alphanumeric characters and optionally a string of the desired short flag comprised of the \dQuote{-} followed by a letter.}
  \item{action}{A character string that describes the action \code{optparse} should take when it encounters an argument,
      either \dQuote{store}, \dQuote{store_true}, or \dQuote{store_false}.  The default is \dQuote{store} which signifies 
      that \code{optparse} should store the specified following value if the optional argument is found on the command string.
      \dQuote{store_true} stores \code{TRUE} if the optional argument is found and \dQuote{store_false} stores \code{FALSE} if the optional argument is found.
  }
  \item{type}{A character string that describes specifies which data type should be stored, 
      either \dQuote{logical}, \dQuote{integer}, \dQuote{double}, \dQuote{complex}, or \dQuote{character}.
      Default is \dQuote{logical} if \code{action \%in\% c("store_true", store_false)}, 
      \code{typeof(default)} if \code{action == "store"} and default is not \code{NULL} and
      \dQuote{character} if \code{action == "store"} and default is \code{NULL}.
  }
  \item{dest}{A character string that specifies what field in the list returned by \code{parse_args} 
        should \code{optparse} store argument values.  Default is derived from the long flag in \code{opt_str}.
  }
  \item{default}{
      The default value \code{optparse} should use if it does not find the optional argument on the command line.  
      Default is derived from the long flag in \code{opt_str}.
  }
  \item{help}{
      A character string describing the argument to be used by \code{print_help} in generating a usage message.
      \code{\%default} will be substituted by the the value of \code{default}.
  }
  \item{metavar}{A character string that stands in for the argument when printing help text.
     Default is the value of \code{dest}.
  }
}
\value{
    Both \code{make_argument} and \code{add_argument} return instances of class \code{ArgumentParserOption}.
}
\references{ Python's \code{argparse} library, which this package is based on,
    is described here:  \url{http://argparse.googlecode.com/svn/tags/r101/doc/index.html} 
}
\author{ 
    Trevor Davis. 

    The documentation for Python's argparse library, which this package 
    is based on, is Copyright 2006-2009, Stven Bethard.
}

\seealso{
    \code{\linkS4class{ArgumentParser}}
    \code{\linkS4class{ArgumentParserOption}}
    \code{\link{parse_args}}
    \code{\link{ArgumentParser}}
}
\examples{
    make_argument("--longflag")
    make_argument(c("-l", "--longflag"))
    make_argument("--integer", type="integer", default=5)
    make_argument("--integer", default=as.integer(5))  # same as previous

    # examples from package vignette
    make_argument(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]")
    make_argument(c("-c", "--count"), type="integer", default=5, 
        help="Number of random normals to generate [default \%default]",
        metavar="number")
    make_argument("--generator", default="rnorm", 
        help = "Function to generate random deviates [default \"\%default\"]")
    make_argument("--mean", default=0, 
        help="Mean if generator == \"rnorm\" [default \%default]")
    make_argument("--sd", default=1, metavar="standard deviation",
        help="Standard deviation if generator == \"rnorm\" [default \%default]")
\testonly{
    if (require("RUnit")) {
        checkEquals(make_argument("--integer", type="integer", default=5),
                    make_argument("--integer", default=as.integer(5)))
        checkEquals(make_argument("--logical", type="logical", default="TRUE"),
                    make_argument("--logical", default=TRUE))
        checkException(make_argument("badflag"))
    }
}
}
