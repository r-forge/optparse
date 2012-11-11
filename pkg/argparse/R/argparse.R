#' Create a command line parser 
#'
#' \code{ArgumentParser} crates a parser object that acts as 
#' a wrapper to Python's argparse module
#' 
#' @param ... Arguments cleaned and passed to Pythons argparse.ArgumentParser()
#' @param python_cmd The python executable for \code{argparse} to use.
#'      Must have argparse and json modules (i.e. Python (>= 2.7)).  
#'      Default is \code{python.exe} on Windows else \code{python}
#'      
#' @import rjson
#' @import proto
#' @export
ArgumentParser <- function(..., 
        python_cmd=getOption("python_cmd", 
                ifelse(.Platform$OS.type == "windows", "python.exe", "python"))) {
    python_code = c("import argparse, json",
    "",
    sprintf("parser = argparse.ArgumentParser(%s)", convert_..._to_arguments(...)),
    "")
    proto(expr = {
        python_code = python_code
        parse_args = function(., args=commandArgs(TRUE)) {
            python_code <- c(.$python_code, 
                    sprintf("args = parser.parse_args([%s])",
                            paste(sprintf("'%s'", args), collapse=", ")),
                    "print(json.dumps(args.__dict__, sort_keys=True))")
            output <- system(python_cmd, input=python_code, intern=TRUE)   
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
                            convert_..._to_arguments(...)))
            return(invisible(NULL))
        }
    })
}

convert_..._to_arguments <- function(...) {
    argument_list <- list(...)
    argument_names <- names(argument_list)
    equals <- ifelse(argument_names == "", "", "=")
    arguments <- shQuote(as.character(argument_list))
    proposed_arguments <- paste(argument_names, equals, arguments, sep="")
    if(any(grepl("type=", proposed_arguments))) {
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
    if(any(grepl("default=", proposed_arguments))) {
        ii <- grep("default=", proposed_arguments)
        default <- argument_list[[ii]]
        if(is.character(default)) default <- shQuote(default) 
        if(is.logical(default)) default <- ifelse(default, 'True', 'False') 
        proposed_arguments[ii] <- sprintf("default=%s", default)
    }
    return(paste(proposed_arguments, collapse=", "))
}
