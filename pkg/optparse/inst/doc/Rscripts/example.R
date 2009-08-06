#!/usr/bin/env Rscript
# Note:  This example is a port of an example in the getopt package
#        which is Copyright 2008 Allen Day
suppressPackageStartupMessages(library("optparse"))

# specify our desired options in a list
# by default ``OptionParser`` will automatically add an help option equivalent to 
# ``make_option("-h", "--help", action="store_true", default=FALSE, 
#               help="Show this help message and exit")``
option_list <- list( 
    make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
        help="Print extra output [default]"),
    make_option(c("-q", "--quietly"), action="store_false", 
        dest="verbose", help="Print little output"),
    make_option(c("-c", "--count"), action="store", type="integer", default=5, 
        help="Number of random normals to generate [default %default]",
        metavar="integer"),
    make_option("--mean", action="store", type="numeric", default=0,
        help="Mean of random normals [default %default]"),
    make_option("--sd", action="store", type="numeric", default=1,
        help="Standard deviation of random normals [default %default]")
    )
                                        
# get command line options, if help option encountered print help and exit,
# otherwise if options not found on command line then set to pre-specified defaults, 
opt <- parse_args(OptionParser(option_list=option_list))

# print some progress messages to stderr if "quietly" wasn't requested
if ( opt$verbose ) { 
    write("writing some verbose output to standard error...\n\n", stderr()) 
}

# do some operations based on user input
cat(paste(rnorm(opt$count, mean=opt$mean, sd=opt$sd), collapse="\n"))
cat("\n")
