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
\examples{
# example from vignette
option_list <- list( 
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]"),
    make_option(c("-q", "--quietly"), action="store_false", 
        dest="verbose", help="Print little output"),
    make_option(c("-c", "--count"), type="integer", default=5, 
        help="Number of random normals to generate [default \%default]",
        metavar="number"),
    make_option("--generator", default="rnorm", 
        help = "Function to generate random deviates [default \"\%default\"]"),
    make_option("--mean", default=0, 
        help="Mean if generator == \"rnorm\" [default \%default]"),
    make_option("--sd", default=1, metavar="standard deviation",
        help="Standard deviation if generator == \"rnorm\" [default \%default]")
    )
parse_args(OptionParser(option_list = option_list), args = c("--sd=3", "--quietly"))
# example from vignette using positional arguments
option_list2 <- list( 
    make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
        help="Print line number at the beginning of each line [default]")
    )
parser <- OptionParser(usage = "\%prog [options] file", option_list=option_list2)

parse_args(parser, args = c("--add_numbers", "example.txt"), positional_arguments = TRUE)

parse_args(parser, args = c("-add_numbers", "example.txt"), positional_arguments = TRUE)
\testonly{
    if (require("RUnit")) {
        sort_list <- function(unsorted_list) { 
            for(ii in seq(along=unsorted_list)) {
                if(is.list(unsorted_list[[ii]])) {
                    unsorted_list[[ii]] <- unsorted_list[[ii]][sort(names(unsorted_list[[ii]]))]
                }
            }
            unsorted_list[sort(names(unsorted_list))] 
        }
        checkEquals(sort_list(parse_args(OptionParser(option_list = option_list), 
                                args = c("--sd=3", "--quietly"))),
                    sort_list(list(sd = 3, verbose = FALSE, help = FALSE, 
                        count = 5, mean = 0, generator = "rnorm")))
        checkEquals(sort_list(parse_args(OptionParser(option_list = option_list), 
                            args = "", positional_arguments = TRUE)),
                    sort_list(list(options = list(sd = 1, help = FALSE, verbose = TRUE, 
                                    count = 5, mean = 0, generator = "rnorm"),
                                args = "")))
        checkEquals(sort_list(parse_args(OptionParser(option_list = option_list), 
                                args = c("-c", "10"))),
                    sort_list(list(sd = 1, help = FALSE, verbose = TRUE, 
                                count = 10, mean = 0, generator = "rnorm")))
        checkEquals(sort_list(parse_args(parser, args = c("--add_numbers", "example.txt"), 
                                positional_arguments = TRUE)),
                    sort_list(list(options = list(add_numbers = TRUE, help = FALSE), 
                                 args = c("example.txt"))))
        checkEquals(sort_list(parse_args(parser, args = c("-add_numbers", "example.txt"), 
                                    positional_arguments = TRUE)),
                    sort_list(list(options = list(add_numbers = FALSE, help = FALSE), 
                                 args = c("-add_numbers", "example.txt"))))
#       checkException(parse_args(parser, args = c("--add_numbers", "example.txt")))
        checkException(parse_args(parser, args = c("-add_numbers", "example.txt")))
    }
}
}
