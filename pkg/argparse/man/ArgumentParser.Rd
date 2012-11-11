\name{ArgumentParser}
\alias{ArgumentParser}
\title{Create a command line parser}
\usage{
  ArgumentParser(...,
    python_cmd = getOption("python_cmd", ifelse(.Platform$OS.type == "windows", "python.exe", "python")))
}
\arguments{
  \item{...}{Arguments cleaned and passed to Pythons
  argparse.ArgumentParser()}

  \item{python_cmd}{The python executable for
  \code{argparse} to use.  Must have argparse and json
  modules (i.e. Python (>= 2.7)).  Default is
  \code{python.exe} on Windows else \code{python}}
}
\description{
  \code{ArgumentParser} crates a parser object that acts as
  a wrapper to Python's argparse module
}
\examples{
parser <- ArgumentParser(description='Process some integers')
parser$add_argument('integers', metavar='N', type="integer", nargs='+',
                   help='an integer for the accumulator')
parser$add_argument('--sum', dest='accumulate', action='store_const',
                   const='sum', default='max',
                   help='sum the integers (default: find the max)')
parser$print_help()
# default args for ArgumentParser()$parse_args are commandArgs(TRUE)
# which is what you'd want for an Rscript but not for interactive use
args <- parser$parse_args(c("--sum", "1", "2", "3"))
accumulate_fn <- get(args$accumulate)
print(accumulate_fn(args$integers))
}

