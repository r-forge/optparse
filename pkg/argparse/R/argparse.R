#' Create a command line parser 
#'
#' \code{ArgumentParser} crates a parser object that acts as 
#' a wrapper to Python's argparse module
#' 
#' @param ... Arguments cleaned and passed to Pythons argparse.ArgumentParser()
#' @param python_cmd The python executable for \code{argparse} to use.
#'      Must have argparse and json modules (i.e. Python (>= 2.7)).  
#'      Default is \code{python}
#' @return  \code{ArgumentParser} returns a parser object which contains
#'    an \code{add_argument} function to add arguments to the parser,
#'    a \code{parse_args} function to parse command line arguments into
#'    a list, a \code{print_help} and \code{print_usage} function to print
#'    usage information.  See code examples, package vignette, 
#'    and corresponding python module for more information on how to use it.
#'    
#' @references Python's \code{argparse} library, which this package is based on,
#'  is described here: \url{http://docs.python.org/library/argparse.html}
#'      
#' @import rjson
#' @import proto
#' @export
#' @examples
#'
#' parser <- ArgumentParser(description='Process some integers')
#' parser$add_argument('integers', metavar='N', type="integer", nargs='+',
#'                    help='an integer for the accumulator')
#' parser$add_argument('--sum', dest='accumulate', action='store_const',
#'                    const='sum', default='max',
#'                    help='sum the integers (default: find the max)')
#' parser$print_help()
#' # default args for ArgumentParser()$parse_args are commandArgs(TRUE)
#' # which is what you'd want for an Rscript but not for interactive use
#' args <- parser$parse_args(c("--sum", "1", "2", "3")) 
#' accumulate_fn <- get(args$accumulate)
#' print(accumulate_fn(args$integers))
## ifelse(.Platform$OS.type == "windows", "python.exe", "python")
ArgumentParser <- function(..., python_cmd=getOption("python_cmd", "python")) {
    python_code = c("import argparse, json",
    "",
    sprintf("parser = argparse.ArgumentParser(%s)", 
            convert_..._to_arguments("ArgumentParser", ...)),
    "")
    proto(expr = {
        python_code = python_code
        parse_args = function(., args=commandArgs(TRUE)) {
            python_code <- c(.$python_code, 
                    sprintf("args = parser.parse_args([%s])",
                            paste(sprintf("'%s'", args), collapse=", ")),
                    "print(json.dumps(args.__dict__, sort_keys=True))")
            output <- suppressWarnings(system(paste(python_cmd, "2>&1"),
                        input=python_code, intern=TRUE))
            if(grepl("^usage:", output[1])) {
                cat(output, sep="\n")
                quit(status=1)
            } else {
                return(rjson::fromJSON(output))
            }
        }
        print_help <- function(.) {
            python_code <- c(.$python_code, "parser.print_help()")
            cat(system(python_cmd, input=python_code, intern=TRUE), sep="\n")   
            invisible(NULL)
        }
        print_usage <- function(.) {
            python_code <- c(.$python_code, "parser.print_usage()")
            cat(system(python_cmd, input=python_code, intern=TRUE), sep="\n")   
            invisible(NULL)
        }
        add_argument = function(., ...) {
            .$python_code <- c(.$python_code,
                    sprintf("parser.add_argument(%s)",
                            convert_..._to_arguments("add_argument", ...)))
            return(invisible(NULL))
        }
    })
}

# @param mode Either "add_argument" or "ArgumentParser"
#' @import getopt
convert_..._to_arguments <- function(mode, ...) {

    argument_list <- list(...)
    argument_names <- names(argument_list)
    equals <- ifelse(argument_names == "", "", "=")
    arguments <- shQuote(as.character(argument_list))
    proposed_arguments <- paste(argument_names, equals, arguments, sep="")
    # Make sure types are what Python wants
    if(mode == "add_argument" && any(grepl("type=", proposed_arguments))) {
        ii <- grep("type=", proposed_arguments)
        type <- argument_list[[ii]]
        python_type <- switch(type,
                character = "str",
                double = "float",
                integer = "int",
                logical = "bool",
                stop(paste(sprintf("type %s not supported,", type),
                        "supported types:",
                        "'logical', 'integer', 'double' or 'character'")))
        proposed_arguments[ii] <- sprintf("type=%s", python_type)
                                 
    }
    # make sure nargs are what python wants
    if(mode == "add_argument" && any(grepl("nargs=", proposed_arguments))) {
        ii <- grep("nargs=", proposed_arguments)
        nargs <- argument_list[[ii]]
        if(is.numeric(nargs)) {
            nargs <- as.character(nargs)
        } else {
            nargs <- shQuote(nargs)
        }
        proposed_arguments[ii] <- sprintf("nargs=%s", nargs)
    }
    # Make defaults are what Python wants, if specified
    default_string <- switch(mode,
           add_argument = "default=", 
           ArgumentParser = "argument_default=",
           stop(sprintf("Unknown mode %s", mode)))
    if(any(grepl(default_string, proposed_arguments))) {
        ii <- grep(default_string, proposed_arguments)
        default <- argument_list[[ii]]
        if(is.character(default)) default <- shQuote(default) 
        if(is.logical(default)) default <- ifelse(default, 'True', 'False') 
        proposed_arguments[ii] <- sprintf("%s%s", default_string, default)
    }
    # Set right default prog name if not specified, if possible
    # Do last to not screw up other fixes with prog insertion
    if(mode == "ArgumentParser" && all(!grepl("prog=", proposed_arguments))) {
        prog <- get_Rscript_filename()
        if(is.na(prog)) prog <- "PROGRAM"
        proposed_arguments <- c(sprintf("prog='%s'", prog), proposed_arguments)
    }
    return(paste(proposed_arguments, collapse=", "))
}
