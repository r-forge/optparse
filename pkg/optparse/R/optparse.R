setClass("OptionParser", representation(usage = "character", options = "list"))

# setMethod("initialize", "OptionParser", function(.Object) {
#     return(add_option(.Object, "-h", "--help", action="store_true", 
#                         dest="help", default=FALSE, 
#                         help="Show this help message and exit"))
# })

OptionParser <- function(usage = "usage: %prog [options]", option_list=list(),
                            add_help_option=TRUE, prog=commandArgs()[1]) {
    if( .Platform$OS.type == "windows") {
        prog <- gsub("\\\\", "\\\\\\\\", prog)
    }
    usage <- sub("%prog", prog, usage)

    if(add_help_option) {
        option_list[[length(option_list) + 1]] <- 
            make_option("-h", "--help",
                action="store_true", dest="help", default=FALSE,
                help="Show this help message and exit")
    }

    return(new("OptionParser", usage=usage, options=option_list))
}

make_option <- function(short_flag, long_flag, action="store", type=NULL,
                     dest=NULL, default=NULL, help="", metavar=NULL) {
    if(grepl("^-", short_flag)) {} else {short_flag <- sub("^", "-", short_flag)}
    if(grepl("^--", long_flag)) {} else {long_flag <- sub("^", "--", long_flag)}
    if(is.null(type)) {
        if( action %in% c("store_true", "store_false") ) {
            type <- "logical"
        }
        else {
            if( action %in% c("store") ) {
                type <- "character"
            }
        }
    }
    if(is.null(dest)) { dest <- sub("^--", "", long_flag) }
    if(is.null(metavar)) {
        if(action == "store") { 
            metavar <- dest
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

# if (!isGeneric(".convert_to_getopt")) {
#     if (is.function(".convert_to_getopt"))
#         fun <- .convert_to_getopt
#     else fun <- function(object) standardGeneric(".convert_to_getopt")
#     setGeneric(".convert_to_getopt", fun)
# }
# 
# 
# setMethod(".convert_to_getopt", "OptionParserOption", function(object) {
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
#)

# if (!isGeneric("add_option")) {
#     if(is.function("add_option"))
#         fun <- add_option
#     else fun <- function(object, ...) standardGeneric("add_option")
#     setGeneric("add_option", fun)
# }
# 
# setMethod("add_option", "OptionParser", 
add_option <- function(object, short_flag, long_flag, action="store", type=NULL, 
                    dest=NULL, default=NULL, help="", metavar=NULL) {
    options <- object@options
    n_original_options <- length(options)
    options[[n_original_options + 1]] <- make_option(short_flag, long_flag, 
                                           action=action, type=type, dest=dest,
                                           default=default, help=help, metavar=metavar)        
    object@options <- options
    return(object)
}
#)

# if (!isGeneric("print_help")) {
#     if(is.function("print_help"))
#         fun <- print_help
#     else fun <- function(object) standardGeneric("print_help")
#     setGeneric("print_help", fun)
# }
# 
# setMethod("print_help", "OptionParser", 
print_help <- function(object) {
    cat(object@usage, fill=TRUE)
    cat("\n")
    cat("options:", sep="\n")    

    options <- object@options
    for(ii in seq(along=options)) {
        option <- options[[ii]]
        cat("\t")
        if(!is.null(option@short_flag)) {
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
#)

# if (!isGeneric("parse_args")) {
#     if (is.function("parse_args")) {
#         fun <- parse_args
#     } else {
#         fun <- function(object, args, ...) standardGeneric("parse_args")
#     }
#     setGeneric("parse_args", fun)
# }
# 
# 
# setMethod("parse_args", "OptionParser", function(object, args = commandArgs(TRUE), print_help_and_exit = TRUE) {
parse_args <- function(object, args = commandArgs(TRUE), print_help_and_exit = TRUE) {
    n_options <- length( object@options )
    spec <- matrix(NA, nrow = n_options, ncol = 5)
    for (ii in seq(along = object@options)) {
        spec[ii, ] <- .convert_to_getopt( object@options[[ii]] )
    }
    opt <- getopt(spec=spec, opt=args)

    options <- list()
    for (ii in seq(along = object@options)) {
        option <- object@options[[ii]]
        option_value <- opt[[sub("^--", "", option@long_flag)]] 
        if( !is.null(option_value) ) {
            if ( option_value & (option@action == "store_false") ) {
                options[[option@dest]] <- FALSE
            } else {    
                options[[option@dest]] <- option_value
            }
        } else {
            if( !is.null(option@default) & is.null(options[[option@dest]]) ) {
                options[[option@dest]] <- option@default  
            }
        }
    }
    if(options[["help"]] & print_help_and_exit) {
        print_help(object)
        quit(status=1)
    }
        
    return(options)
}
#)

