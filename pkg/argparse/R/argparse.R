#   The documentation for Python's optparse library, which this package 
#   is based on, is Copyright 1990-2009, Python Software Foundation.

setClass("ArgumentParser", representation(usage = "character", arguments = "list"))

ArgumentParser <- function(usage = "usage: %prog [options]", argument_list=list(),
                            add_help_option=TRUE, prog=NULL) {
    
    if(is.null(prog)) {
        args <- commandArgs()
        file_index <- grepl("--file=", args)
        prog <- gsub("--file=", "", args[file_index])
    }
    if( .Platform$OS.type == "windows") {
        prog <- gsub("\\\\", "\\\\\\\\", prog)
    }
    if(length(prog)) {
        usage <- sub("%prog", prog, usage)
    }

    if(add_help_option) {
        argument_list[[length(argument_list) + 1]] <- 
            make_argument("-h", "--help",
                action = "store_true", dest = "help", default = FALSE,
                help = "Show this help message and exit")
    }

    return(new("ArgumentParser", usage=usage, arguments=argument_list))
}

make_argument <- function(..., action="store", type=NULL,
                     dest=NULL, default=NULL, help="", metavar=NULL) {
    opt_str <- c(...)
    short_flag <- opt_str[grepl("^-[[:alpha:]]", opt_str)]
    if(length(short_flag)) {} else { short_flag <- as.character(NA) }
    long_flag <- opt_str[grepl("^--[[:alpha:]]", opt_str)]
    if(length(long_flag)) {} else {stop("We require a long flag option")}

    if(is.null(type)) {
        if( action %in% c("store_true", "store_false") ) {
            type <- "logical"
        }
        else {
            if( action %in% c("store") ) {
                if (is.null(default)) {
                    type <- "character"
                } else {
                    type <- typeof(default)
                }
            }
        }
    }
    if((type != typeof(default)) & !is.null(default)) {
        storage.mode(default) <- type
    }
    if(is.null(dest)) { dest <- sub("^--", "", long_flag) }
    if(is.null(metavar)) {
        if(action == "store") { 
            metavar <- sub("^--", "", long_flag)
        } else {
            metavar <- character(0)
        }
    }
        
    return(new("ArgumentParserOption", short_flag=short_flag, long_flag=long_flag,
                        action=action, type=type, dest=dest, default=default, 
                        help=help, metavar=metavar))
}

setClass("ArgumentParserOption", representation(short_flag="character", 
                                    long_flag="character",
                                    action="character",
                                    type="character",
                                    dest="character",
                                    default="ANY",
                                    help="character",
                                    metavar="character"),)

.convert_to_getopt <- function(object) {
    short_flag <- sub("^-", "", object@short_flag)
    long_flag <- sub("^--", "", object@long_flag)
    if( object@action %in% c("store_true", "store_false") ) {
        argument <- 0
    } else {
        argument <- 1
    }
    return( c( long_flag, short_flag, argument, object@type, object@help) )
}
add_argument <- function(object, ..., action="store", type=NULL, 
                    dest=NULL, default=NULL, help="", metavar=NULL) {
    arguments <- object@arguments
    n_original_arguments <- length(arguments)
    arguments[[n_original_arguments + 1]] <- make_argument(opt_str, ...,
                                           action=action, type=type, dest=dest,
                                           default=default, help=help, metavar=metavar)        
    object@arguments <- arguments
    return(object)
}
print_help <- function(object) {
    cat(object@usage, fill=TRUE)
    cat("\n")
    cat("arguments:", sep="\n")    

    arguments <- object@arguments
    for(ii in seq(along=arguments)) {
        argument <- arguments[[ii]]
        cat("\t")
        if(!is.na(argument@short_flag)) {
            cat(argument@short_flag)
            if( argument@action == "store" ) {
                cat(" ", toupper(argument@metavar), sep="")
            }
            cat(", ")
        }
        if(!is.null(argument@long_flag)) {
            cat(argument@long_flag)
            if( argument@action == "store" ) {
                cat("=", toupper(argument@metavar), sep="")
            }
        }
        cat("\n\t\t")
        default <- as.character(argument@default)
        default_str <- ifelse(length(default), default, "NULL")
        cat(sub("%default", default_str, argument@help))
        cat("\n\n")
    }
    return(invisible(NULL))
}
parse_args <- function(object, args = commandArgs(TRUE), print_help_and_exit = TRUE) {
    n_arguments <- length( object@arguments )
    spec <- matrix(NA, nrow = n_arguments, ncol = 5)
    for (ii in seq(along = object@arguments)) {
        spec[ii, ] <- .convert_to_getopt( object@arguments[[ii]] )
    }
    opt <- .getopt(spec=spec, opt=args)

    arguments <- list()
    for (ii in seq(along = object@arguments)) {
        argument <- object@arguments[[ii]]
        argument_value <- opt[[sub("^--", "", argument@long_flag)]] 
        if( !is.null(argument_value) ) {
            if ( argument@action == "store_false" ) {
                arguments[[argument@dest]] <- FALSE
            } else {    
                arguments[[argument@dest]] <- argument_value
            }
        } else {
            if( !is.null(argument@default) & is.null(arguments[[argument@dest]]) ) {
                arguments[[argument@dest]] <- argument@default  
            }
        }
    }
    if(arguments[["help"]] & print_help_and_exit) {
        print_help(object)
        quit(status=1)
    }
        
    return(arguments)
}

