\name{OptionParser}
\Rdversion{1.1}
\alias{OptionParser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OptionParser(usage = "usage: %prog [options]", option_list = list(), add_help_option = TRUE, prog = commandArgs()[1])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usage}{
%%     ~~Describe \code{usage} here~~
}
  \item{option_list}{
%%     ~~Describe \code{option_list} here~~
}
  \item{add_help_option}{
%%     ~~Describe \code{add_help_option} here~~
}
  \item{prog}{
%%     ~~Describe \code{prog} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (usage = "usage: \%prog [options]", option_list = list(), 
    add_help_option = TRUE, prog = commandArgs()[1]) 
{
    if (.Platform$OS.type == "windows") {
        prog <- gsub("\\\\", "\\\\\\\\", prog)
    }
    usage <- sub("\%prog", prog, usage)
    if (add_help_option) {
        option_list[[length(option_list) + 1]] <- make_option("-h", 
            "--help", action = "store_true", dest = "help", default = FALSE, 
            help = "Show this help message and exit")
    }
    return(new("OptionParser", usage = usage, options = option_list))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
