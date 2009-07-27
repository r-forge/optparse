\name{make_option}
\Rdversion{1.1}
\alias{make_option}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
make_option(short_flag, long_flag, action = "store", type = NULL, dest = NULL, default = NULL, help = "", metavar = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{short_flag}{
%%     ~~Describe \code{short_flag} here~~
}
  \item{long_flag}{
%%     ~~Describe \code{long_flag} here~~
}
  \item{action}{
%%     ~~Describe \code{action} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{dest}{
%%     ~~Describe \code{dest} here~~
}
  \item{default}{
%%     ~~Describe \code{default} here~~
}
  \item{help}{
%%     ~~Describe \code{help} here~~
}
  \item{metavar}{
%%     ~~Describe \code{metavar} here~~
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
function (short_flag, long_flag, action = "store", type = NULL, 
    dest = NULL, default = NULL, help = "", metavar = NULL) 
{
    if (grepl("^-", short_flag)) {
    }
    else {
        short_flag <- sub("^", "-", short_flag)
    }
    if (grepl("^--", long_flag)) {
    }
    else {
        long_flag <- sub("^", "--", long_flag)
    }
    if (is.null(type)) {
        if (action \%in\% c("store_true", "store_false")) {
            type <- "logical"
        }
        else {
            if (action \%in\% c("store")) {
                type <- "character"
            }
        }
    }
    if (is.null(dest)) {
        dest <- sub("^--", "", long_flag)
    }
    if (is.null(metavar)) {
        if (action == "store") {
            metavar <- dest
        }
        else {
            metavar <- character(0)
        }
    }
    return(new("OptionParserOption", short_flag = short_flag, 
        long_flag = long_flag, action = action, type = type, 
        dest = dest, default = default, help = help, metavar = metavar))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
