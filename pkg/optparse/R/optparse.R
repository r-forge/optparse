#   The documentation for Python's optparse library, which this package 
#   is based on, is Copyright 1990-2009, Python Software Foundation.

setClass("OptionParser", representation(usage = "character", options = "list"))

# setMethod("initialize", "OptionParser", function(.Object) {
#     return(add_option(.Object, "-h", "--help", action="store_true", 
#                         dest="help", default=FALSE, 
#                         help="Show this help message and exit"))
# })

OptionParser <- function(usage = "usage: %prog [options]", option_list=list(),
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
        option_list[[length(option_list) + 1]] <- 
            make_option(c("-h", "--help"),
                action="store_true", dest="help", default=FALSE,
                help="Show this help message and exit")
    }

    return(new("OptionParser", usage=usage, options=option_list))
}

make_option <- function(opt_str, action="store", type=NULL,
                     dest=NULL, default=NULL, help="", metavar=NULL) {
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
        
    return(new("OptionParserOption", short_flag=short_flag, long_flag=long_flag,
                        action=action, type=type, dest=dest, default=default, 
                        help=help, metavar=metavar))
}

setClass("OptionParserOption", representation(short_flag="character", 
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
add_option <- function(object, opt_str, action="store", type=NULL, 
                    dest=NULL, default=NULL, help="", metavar=NULL) {
    options_list <- object@options
    n_original_options <- length(options_list)
    options_list[[n_original_options + 1]] <- make_option(opt_str=opt_str,
                                           action=action, type=type, dest=dest,
                                           default=default, help=help, metavar=metavar)        
    object@options <- options_list
    return(object)
}

print_help <- function(object) {
    cat(object@usage, fill=TRUE)
    cat("\n")
    cat("options:", sep="\n")    

    options_list <- object@options
    for(ii in seq(along=options_list)) {
        option <- options_list[[ii]]
        cat("\t")
        if(!is.na(option@short_flag)) {
            cat(option@short_flag)
            if( option@action == "store" ) {
                cat(" ", toupper(option@metavar), sep="")
            }
            cat(", ")
        }
        if(!is.null(option@long_flag)) {
            cat(option@long_flag)
            if( option@action == "store" ) {
                cat("=", toupper(option@metavar), sep="")
            }
        }
        cat("\n\t\t")
        default <- as.character(option@default)
        default_str <- ifelse(length(default), default, "NULL")
        cat(sub("%default", default_str, option@help))
        cat("\n\n")
    }
    return(invisible(NULL))
}

parse_args <- function(object, args = commandArgs(trailingOnly = TRUE), 
                    print_help_and_exit = TRUE, positional_arguments = FALSE) {
    n_options <- length( object@options )
    spec <- matrix(NA, nrow = n_options, ncol = 5)
    for (ii in seq(along = object@options)) {
        spec[ii, ] <- .convert_to_getopt( object@options[[ii]] )
    }

    if(positional_arguments) {
        os_and_n_arg <- .get_option_strings_and_n_arguments(object)
        suppressWarnings(
            last_option_index <- max(which(args %in% os_and_n_arg$option_strings))
        )
        if(last_option_index == -Inf) {
            n_option_arguments <- 0
        } else {
            last_option <- args[last_option_index]
            n_arg <- os_and_n_arg$n_arguments[which(os_and_n_arg$option_strings == last_option)]
            n_option_arguments <- last_option_index + n_arg
        }
        if(length(args) > n_option_arguments) {
            arguments_positional <- args[(n_option_arguments + 1):length(args)]
            if(n_option_arguments) {
                args <- args[1:n_option_arguments]
            } else {
                args <- NULL
            }
        } else {
            arguments_positional <- character(0)
        }
    }
    options_list <- list()
    if(length(args)) {
        opt <- getopt(spec=spec, opt=args)
    } else {
        opt <- list()
    }

    for (ii in seq(along = object@options)) {
        option <- object@options[[ii]]
        option_value <- opt[[sub("^--", "", option@long_flag)]] 
        if( !is.null(option_value) ) {
            if ( option@action == "store_false" ) {
                options_list[[option@dest]] <- FALSE
            } else {    
                options_list[[option@dest]] <- option_value
            }
        } else {
            if( !is.null(option@default) & is.null(options_list[[option@dest]]) ) {
                options_list[[option@dest]] <- option@default  
            }
        }
    }
    if(options_list[["help"]] & print_help_and_exit) {
        print_help(object)
        quit(status=1)
    }
    if(positional_arguments) {
        return(list(options = options_list, args = arguments_positional))
    } else {    
        return(options_list)
    }
}

.get_option_strings_and_n_arguments <- function(object) {
    option_strings <- vector("character")
    n_arguments <- vector("numeric")
    for (ii in seq(along = object@options)) {
        option <- object@options[[ii]]
        option_strings <- c(option_strings, option@short_flag)
        option_strings <- c(option_strings, option@long_flag)
        if (option@action == "store") {
            n_arguments <- c(n_arguments, 1, 1)
        } else {
            n_arguments <- c(n_arguments, 0, 0)
        }
    }
    return(list(option_strings = option_strings, n_arguments = n_arguments))
}
