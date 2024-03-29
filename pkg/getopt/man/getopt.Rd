\name{getopt}
\alias{getopt}
\alias{getopt-package}
\title{C-like getopt behavior}
\usage{
  getopt(spec = NULL, opt = commandArgs(TRUE),
    command = get_Rscript_filename(), usage = FALSE,
    debug = FALSE)
}
\arguments{
  \item{spec}{The getopt specification, or spec of what
  options are considered valid.  The specification must be
  either a 4-5 column \link{matrix}, or a \link{character}
  \link{vector} coercible into a 4 column \link{matrix}
  using \link{matrix}(x,ncol=4,byrow=TRUE) command.  The
  \link{matrix}/\link{vector} contains:

  Column 1: the \emph{long flag} name.  A
  multi-\link{character} string.

  Column 2: \emph{short flag} alias of Column 1.  A
  single-\link{character} string.

  Column 3: \emph{Argument} mask of the \emph{flag}.  An
  \link{integer}. Possible values: 0=no argument,
  1=required argument, 2=optional argument.

  Column 4: Data type to which the \emph{flag}'s argument
  shall be cast using \link{storage.mode}.  A
  multi-\link{character} string.  This only considered for
  same-row Column 3 values of 1,2.  Possible values:
  \link{logical}, \link{integer}, \link{double},
  \link{complex}, \link{character}.

  Column 5 (optional): A brief description of the purpose
  of the option.

  The terms \emph{option}, \emph{flag}, \emph{long flag},
  \emph{short flag}, and \emph{argument} have very specific
  meanings in the context of this document.  Read the
  ``Description'' section for definitions.}

  \item{opt}{This defaults to the return value of
  \link{commandArgs}(TRUE).

  If R was invoked directly via the ``R'' command, this
  corresponds to all arguments passed to R after the
  ``--args'' flag.

  If R was invoked via the ``\link{Rscript}'' command, this
  corresponds to all arguments after the name of the R
  script file.

  Read about \link{commandArgs} and \link{Rscript} to learn
  more.}

  \item{command}{The string to use in the usage message as
  the name of the script.  See argument \emph{usage}.}

  \item{usage}{If TRUE, argument \emph{opt} will be ignored
  and a usage statement (character string) will be
  generated and returned from \emph{spec}.}

  \item{debug}{This is used internally to debug the
  getopt() function itself.}
}
\description{
  getopt is primarily intended to be used with
  ``\link{Rscript}''.  It facilitates writing ``\#!''
  shebang scripts that accept short and long flags/options.
  It can also be used from ``R'' directly, but is probably
  less useful in this context.
}
\details{
  getopt() returns a \link{list} data structure containing
  \link{names} of the flags that were present in the
  \link{character} \link{vector} passed in under the
  \emph{opt} argument.  Each value of the \link{list} is
  coerced to the data type specified according to the value
  of the \emph{spec} argument.  See below for details.

  Notes on naming convention:

  1. An \emph{option} is one of the shell-split input
  strings.

  2. A \emph{flag} is a type of \emph{option}.  a
  \emph{flag} can be defined as having no \emph{argument}
  (defined below), a required \emph{argument}, or an
  optional \emph{argument}.

  3. An \emph{argument} is a type of \emph{option}, and is
  the value associated with a flag.

  4. A \emph{long flag} is a type of \emph{flag}, and
  begins with the string ``--''.  If the \emph{long flag}
  has an associated \emph{argument}, it may be delimited
  from the \emph{long flag} by either a trailing \emph{=},
  or may be the subsequent \emph{option}.

  5. A \emph{short flag} is a type of \emph{flag}, and
  begins with the string ``-''.  If a \emph{short flag} has
  an associated \emph{argument}, it is the subsequent
  \emph{option}.  \emph{short flags} may be bundled
  together, sharing a single leading ``-'', but only the
  final \emph{short flag} is able to have a corresponding
  \emph{argument}.

  Many users wonder whether they should use the getopt
  package or optparse package Here is some of the major
  differences:

  Features available in \code{getopt} unavailable in
  \code{optparse}

  1. As well as allowing one to specify options that take
  either no argument or a required argument, \code{getopt}
  also allows one to specify option with an optional
  argument.

  Some features implemented in \code{optparse} package
  unavailable in \code{getopt}

  1. Limited support for capturing positional arguments
  after the optional arguments when
  \code{positional_arguments} set to TRUE in
  \code{parse_args}

  2. Automatic generation of an help option and printing of
  help text when encounters an "-h"

  3. Option to specify default arguments for options as
  well the variable name to store option values

  There is also new package \code{argparse} which contains
  all the features of both getopt and optparse but which
  has a dependency on Python (>= 2.7) and has not been used
  in production for a few years like the getopt and
  optparse packages.

  Some Features unlikely to be implemented in
  \code{getopt}:

  1. Support for multiple, identical flags, e.g. for "-m 3
  -v 5 -v", the trailing "-v" overrides the preceding "-v
  5", result is v=TRUE (or equivalent typecast).

  2. Support for multi-valued flags, e.g.
  "--libpath=/usr/local/lib --libpath=/tmp/foo".

  3. Support for lists, e.g. "--define os=linux --define
  os=redhat" would set result$os$linux=TRUE and
  result$os$redhat=TRUE.

  4. Support for incremental, argument-less flags, e.g.
  "/path/to/script -vvv" should set v=3.

  5. Support partial-but-unique string match on options,
  e.g. "--verb" and "--verbose" both match long flag
  "--verbose".

  6. No support for mixing in positional arguments or extra
  arguments that don't match any options.  For example, you
  can't do "my.R --arg1 1 foo bar baz" and recover "foo",
  "bar", "baz" as a list.  Likewise for "my.R foo --arg1 1
  bar baz".
}
\examples{
#!/path/to/Rscript
library('getopt');
#get options, using the spec as defined by the enclosed list.
#we read the options from the default: commandArgs(TRUE).
spec = matrix(c(
  'verbose', 'v', 2, "integer",
  'help'   , 'h', 0, "logical",
  'count'  , 'c', 1, "integer",
  'mean'   , 'm', 1, "double",
  'sd'     , 's', 1, "double"
), byrow=TRUE, ncol=4);
opt = getopt(spec);

# if help was asked for print a friendly message
# and exit with a non-zero error code
if ( !is.null(opt$help) ) {
  cat(getopt(spec, usage=TRUE));
  q(status=1);
}

#set some reasonable defaults for the options that are needed,
#but were not specified.
if ( is.null(opt$mean    ) ) { opt$mean    = 0     }
if ( is.null(opt$sd      ) ) { opt$sd      = 1     }
if ( is.null(opt$count   ) ) { opt$count   = 10    }
if ( is.null(opt$verbose ) ) { opt$verbose = FALSE }

#print some progress messages to stderr, if requested.
if ( opt$verbose ) { write("writing...",stderr()); }

#do some operation based on user input.
cat(paste(rnorm(opt$count,mean=opt$mean,sd=opt$sd),collapse="\\n"));
cat("\\n");

#signal success and exit.
#q(status=0);
}
\author{
  Allen Day
}
\seealso{
  \code{\link{getopt}}
}
\keyword{data}

