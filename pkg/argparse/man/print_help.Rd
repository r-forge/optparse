\name{print_help}
\Rdversion{1.1}
\alias{print_help}
\title{
    Printing an usage message from an ArgumentParser object
}
\description{
    \code{print_help} print an usage message from an ArgumentParser object, 
    usually called by \code{parse_args} when it encounters a help option.
}
\usage{
print_help(object)
}
\arguments{
  \item{object}{
    A \code{ArgumentParser} instance.
}
}
\value{
    \code{print_help} uses the \code{cat} function to print out a usage message.  It returns \code{invisible(NULL)}.
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
    \code{{parse_args}}
    \code{{ArgumentParser}}
}
