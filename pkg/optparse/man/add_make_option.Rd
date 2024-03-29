\name{make_option}
\alias{add_option}
\alias{make_option}
\title{Functions to enable our OptionParser to recognize specific command line
options.}
\usage{
  make_option(opt_str, action = "store", type = NULL,
    dest = NULL, default = NULL, help = "", metavar = NULL)

  add_option(object, opt_str, action = "store",
    type = NULL, dest = NULL, default = NULL, help = "",
    metavar = NULL)
}
\arguments{
  \item{object}{An instance of the \code{OptionParser}
  class}

  \item{opt_str}{A character vector containing the string
  of the desired long flag comprised of \dQuote{--}
  followed by a letter and then a sequence of alphanumeric
  characters and optionally a string of the desired short
  flag comprised of the \dQuote{-} followed by a letter.}

  \item{action}{A character string that describes the
  action \code{optparse} should take when it encounters an
  option, either \dQuote{store}, \dQuote{store_true}, or
  \dQuote{store_false}.  The default is \dQuote{store}
  which signifies that \code{optparse} should store the
  specified following value if the option is found on the
  command string.  \dQuote{store_true} stores \code{TRUE}
  if the option is found and \dQuote{store_false} stores
  \code{FALSE} if the option is found.}

  \item{type}{A character string that describes specifies
  which data type should be stored, either
  \dQuote{logical}, \dQuote{integer}, \dQuote{double},
  \dQuote{complex}, or \dQuote{character}.  Default is
  \dQuote{logical} if \code{action %in% c("store_true",
  store_false)}, \code{typeof(default)} if \code{action ==
  "store"} and default is not \code{NULL} and
  \dQuote{character} if \code{action == "store"} and
  default is \code{NULL}.}

  \item{dest}{A character string that specifies what field
  in the list returned by \code{parse_args} should
  \code{optparse} store option values.  Default is derived
  from the long flag in \code{opt_str}.}

  \item{default}{The default value \code{optparse} should
  use if it does not find the option on the command line.
  Default is derived from the long flag in \code{opt_str}.}

  \item{help}{A character string describing the option to
  be used by \code{print_help} in generating a usage
  message.  \code{\%default} will be substituted by the
  value of \code{default}.}

  \item{metavar}{A character string that stands in for the
  option argument when printing help text.  Default is the
  value of \code{dest}.}
}
\value{
  Both \code{make_option} and \code{add_option} return
  instances of class \code{OptionParserOption}.
}
\description{
  \code{add_option} adds a option to a prexisting
  \code{OptionParser} instance whereas \code{make_option}
  is used to create a list of \code{OptionParserOption}
  instances that will be used in the \code{option_list}
  argument of the \code{OptionParser} function to create a
  new \code{OptionParser} instance.
}
\examples{
make_option("--longflag")
   make_option(c("-l", "--longflag"))
   make_option("--integer", type="integer", default=5)
   make_option("--integer", default=as.integer(5))  # same as previous

   # examples from package vignette
   make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
       help="Print extra output [default]")
   make_option(c("-c", "--count"), type="integer", default=5,
       help="Number of random normals to generate [default \%default]",
       metavar="number")
   make_option("--generator", default="rnorm",
       help = "Function to generate random deviates [default \\"\%default\\"]")
   make_option("--mean", default=0,
       help="Mean if generator == \\"rnorm\\" [default \%default]")
   make_option("--sd", default=1, metavar="standard deviation",
       help="Standard deviation if generator == \\"rnorm\\" [default \%default]")
}
\author{
  Trevor Davis.
}
\references{
  Python's \code{optparse} library, which inspires this
  package, is described here:
  \url{http://docs.python.org/library/optparse.html}
}
\seealso{
  \code{\link{parse_args}} \code{\link{OptionParser}}
}

