\name{parse_args}
\Rdversion{1.1}
\alias{parse_args}
\title{
    Parse command line optional and positional arguments.
}
\description{
    \code{parse_args} parses command line optional and positional arguments using an \code{ArgumentParser} instance for guidance.
}
\usage{
parse_args(object, args = commandArgs(TRUE), print_help_and_exit=TRUE)
}
\arguments{
  \item{object}{
    An \code{ArgumentParser} instance.
}
  \item{args}{
    A character string containing command line optional and positional arguments to be parsed.  
    Default is everything after the Rscript program in the command line. 
}
  \item{print_help_and_exit}{
    Whether \code{parse_args} should call \code{print_help} to print out a usage message and exit the program.  Default is \code{TRUE}.
}
}
\value{
    Returns a list containing argument values.
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
    \code{\link{print_help}}
    \code{\link{ArgumentParser}}
}
