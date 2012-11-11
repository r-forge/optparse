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

