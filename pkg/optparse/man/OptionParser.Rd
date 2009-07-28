\name{OptionParser}
\Rdversion{1.1}
\alias{OptionParser}
\title{
    A function to create an istance of the class OptionParser.
}
\description{
    This function is used to create an instance of the class \code{OptionParser} which when combined 
    with its \code{parse_args} method 
    is very useful for parsing options from the command line.
}
\usage{
    OptionParser(usage = "usage: \%prog [options]", option_list = list(), add_help_option = TRUE, prog = commandArgs()[1])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{usage}{
        The program usage message that will printed out if \code{parse_args} finds a help option, 
            \code{\%prog} is substituted with the value of the \code{prog} argument.
    }
    \item{option_list}{
        A list of of \code{OptionParserOption} instances that will define how \code{parse_args} reacts to command line options.
            \code{OptionParserOption} instances are usually created by \code{make_option} and can also be added to an existing
            \code{OptionParser} instance via the \code{add_option} function.
    }
    \item{add_help_option}{
        Whether a standard help option should be automatically added to the \code{OptionParser} instance.
    }
    \item{prog}{
        Program name to be substituted for \code{\%prog} in the usage message, the default is to use
            the actual Rscript file name if called by an Rscript file.
    }
}
\value{
    An istance of the \code{OptionParser} class.
}
\references{ Python's \code{optparse} library, which this package is inspired by,
    is described here:  \url{http://docs.python.org/library/optparse.html} }
    \author{ Trevor Davis }

\seealso{
    \code{\link{parse_args}}
    \code{\link{make_option}}
    \code{\link{add_option}}
    \code{\linkS4class{OptionParser}}
}
\examples{
}
