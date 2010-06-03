\name{parse_args}
\Rdversion{1.1}
\alias{parse_args}
\title{
    Parse command line options.
}
\description{
    \code{parse_args} parses command line options using an \code{OptionParser} instance for guidance.
}
\usage{
parse_args(object, args = commandArgs(TRUE), print_help_and_exit=TRUE, positional_arguments = FALSE)
}
\arguments{
  \item{object}{
    An \code{OptionParser} instance.
}
  \item{args}{
    A character vector containing command line options to be parsed.  
    Default is everything after the Rscript program in the command line. If \code{positional_arguments} is \code{TRUE} then \code{parse_args} will only look for positional arguments at the end of this vector.
}
  \item{print_help_and_exit}{
    Whether \code{parse_args} should call \code{print_help} to print out a usage message and exit the program.  Default is \code{TRUE}.
}
  \item{positional_arguments}{
    Whether \code{parse_args} should look for and return a character vector of \emph{positional} arguments.  Default is \code{FALSE}.
}
}
\value{
    Returns a list containing option values if \code{positional_arguments} is \code{FALSE} (the default).  Otherwise returns a list with field \code{options} containing our option values as well as another field \code{args} which contains a vector of positional arguments.
}
\references{ Python's \code{optparse} library, which this package is based on,
    is described here:  \url{http://docs.python.org/library/optparse.html} 
}
\author{ 
    Trevor Davis. 

    The documentation for Python's optparse library, which this package 
    is based on, is Copyright 1990-2009, Python Software Foundation.
}

\seealso{
    \code{\linkS4class{OptionParser}}
    \code{\link{print_help}}
    \code{\link{OptionParser}}
}
