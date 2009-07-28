\name{print_help}
\Rdversion{1.1}
\alias{print_help}
\title{
    Printing an usage message from an OptionParser object
}
\description{
    \code{print_help} print an usage message from an OptionParser object, 
    usually called by \code{parse_args} when it encounters a help option.
}
\usage{
print_help(object)
}
\arguments{
  \item{object}{
    A \code{OptionParser} instance.
}
}
\value{
    \code{print_help} uses the \code{cat} function to print out a usage message.  It returns \code{invisible(NULL)}.
}
\references{ Python's \code{optparse} library, which this package is inspired by,
   is described here:  \url{http://docs.python.org/library/optparse.html} }
\author{ Trevor Davis }

\seealso{
    \code{\linkS4class{OptionParser}}
    \code{{parse_args}}
    \code{{OptionParser}}
}
