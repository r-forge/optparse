\name{ArgumentParser}
\Rdversion{1.1}
\alias{ArgumentParser}
\title{
    A function to create an istance of the class ArgumentParser.
}
\description{
    This function is used to create an instance of the class \code{ArgumentParser} which when combined 
    with its \code{parse_args} method 
    is very useful for parsing options from the command line.
}
\usage{
    ArgumentParser(usage = "usage: \%(prog) [options]", argument_list = list(), add_help_option = TRUE, prog = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{usage}{
        The program usage message that will printed out if \code{parse_args} finds a help option, 
            \code{\%(prog)} is substituted with the value of the \code{prog} argument.
    }
    \item{argument_list}{
        A list of of \code{ArgumentParserArgument} instances that will define how \code{parse_args} reacts to command line options.
            \code{ArgumentParserArgument} instances are usually created by \code{make_argument} and can also be added to an existing
            \code{ArgumentParser} instance via the \code{add_argument} function.
    }
    \item{add_help_option}{
        Whether a standard help option should be automatically added to the \code{ArgumentParser} instance.
    }
    \item{prog}{
        Program name to be substituted for \code{\%(prog)} in the usage message, the default is to use
            the actual Rscript file name if called by an Rscript file and otherwise keep \code{\%(prog)}.
    }
}
\value{
    An instance of the \code{ArgumentParser} class.
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
    \code{\link{parse_args}}
    \code{\link{make_argument}}
    \code{\link{add_argument}}
    \code{\linkS4class{ArgumentParser}}
}
