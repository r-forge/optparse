\name{argparse-package}
\Rdversion{1.1}
\alias{argparse-package}
\alias{argparse}
\docType{package}
\title{
Command line optional and positional argument parser
}
\description{
Goal is to create an R package of a command line parser inspired by Python's \dQuote{argparse} library.
}
\details{
%\tabular{ll}{
%Package: \tab argparse\cr
%Type: \tab Package\cr
%Version: \tab 0.8\cr
%Date: \tab 2009-10-27\cr
%License: \tab GPL\cr
%LazyLoad: \tab yes\cr
%Depends: \tab R (>= 2.9.0), methods\cr
%}
%%%% Ported from Allen Day's getopt package
\code{argparse} is primarily intended to be used with \dQuote{Rscript}. It facilitates writing \dQuote{#!} shebang scripts
that accept short and long flags/options. It can also be used from \R directly, but is probably less
useful in this context.

See package vignette for a more detailed example.

Notes on naming convention in package:
1. An option is one of the shell-split input strings.
2. A flag is a type of option. a flag can be defined as having no argument (defined below), a required
argument, or an optional argument.
3. An argument is a type of option, and is the value associated with a flag.
4. A long flag is a type of flag, and begins with the string \dQuote{--}. If the long flag has an associated
argument, it may be delimited from the long flag by either a trailing =, or may be the subsequent
option.
5. A short flag is a type of flag, and begins with the string \dQuote{-}. If a short flag has an associated
argument, it is the subsequent option. short flags may be bundled together, sharing a single leading
\dQuote{"-"}, but only the final short flag is able to have a corresponding argument.
%%%
}
\references{ Python's \code{argparse} library, which this package is based on,
    is described here:  \url{http://argparse.googlecode.com/svn/tags/r101/doc/index.html} 
}
\author{ 
    Trevor Davis. 

    Some documentation and unit tests ported from Allen Day's getopt package.

    The documentation for Python's argparse library, which this package 
    is based on, is Copyright 2006-2009, Stven Bethard.
}
\keyword{ package }
\seealso{
 \code{\link[getopt]{getopt}} 
}
\examples{
    \dontrun{
        #!/path/to/Rscript
        # specify our desired optional arguments in a list
        # by default ``ArgumentParser`` will automatically add an help option equivalent to 
        # ``make_argument("-h", "--help", action="store_true", default=FALSE, 
        #               help="Show this help message and exit")``
        argument_list <- list( 
                make_argument("-v", "--verbose", action="store_true", default=TRUE,
                    help="Print lots of output [default]"),
                make_argument("-q", "--quietly", action="store_false", 
                    dest="verbose", help="Print little output"),
                make_argument("-c", "--count", action="store", type="integer", default=10, 
                    help="Number of random normals to generate [default \%default]"),
                make_argument("-m", "--mean", action="store", type="numeric", default=0,
                    help="Mean of random normals [default \%default]"),
                make_argument("-s", "--sd", action="store", type="numeric", default=1,
                    help="Standard deviation of random normals [default \%default]")
                )
                                                
        # get command line options, if not found set to specified defaults, 
        # and if help option encountered print help and exit
        opt <- parse_args(ArgumentParser(argument_list))

        # print some progress messages to stderr if "quietly" wasn't requested
        if ( opt$verbose ) { write("writing...", stderr()) }

        # do some operations based on user input
        cat(paste(rnorm(opt$count, mean=opt$mean, sd=opt$sd), collapse="\n"))
        cat("\n")
    }

    \testonly{
        if (require("RUnit")) {


            parser <- ArgumentParser( argument_list = list( make_argument("-q", "--quietly", action="store_false", dest="verbose"),
                                                make_argument("--verbose", action="store_true", dest="verbose", default=TRUE),
                                                make_argument("-f", "--filename", action="store", metavar="file")))
                                            
            # verbose should be false
            checkEquals(parse_args(parser, "-q"), list(verbose = FALSE, help = FALSE))
            checkException(checkEquals(parse_args(parser, "-q"), list(verbose = TRUE, help = FALSE)))
            print_help(parser)
        }
    }
}
