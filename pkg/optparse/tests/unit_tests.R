library("testthat")
library("optparse")

context("Testing make_option")
test_that("make_option works as expected", {
    expect_equal(make_option("--integer", type="integer", default=5),
                make_option("--integer", default=as.integer(5)))
    expect_equal(make_option("--logical", type="logical", default="TRUE"),
                make_option("--logical", default=TRUE))
    expect_that(make_option("badflag"), throws_error())
})

context("Testing parse_args")
test_that("parse_args works as expected", {
    option_list <- list( 
        make_option(c("-v", "--verbose"), action="store_true", default=TRUE,
            help="Print extra output [default]"),
        make_option(c("-q", "--quietly"), action="store_false", 
            dest="verbose", help="Print little output"),
        make_option(c("-c", "--count"), type="integer", default=5, 
            help="Number of random normals to generate [default \\%default]",
            metavar="number"),
        make_option("--generator", default="rnorm", 
            help = "Function to generate random deviates [default \"\\%default\"]"),
        make_option("--mean", default=0, 
            help="Mean if generator == \"rnorm\" [default \\%default]"),
        make_option("--sd", default=1, metavar="standard deviation",
            help="Standard deviation if generator == \"rnorm\" [default \\%default]")
        )
    option_list2 <- list( 
        make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
            help="Print line number at the beginning of each line [default]")
        )
    parser <- OptionParser(usage = "\\%prog [options] file", option_list=option_list2)
    sort_list <- function(unsorted_list) {
        for(ii in seq(along=unsorted_list)) {
            if(is.list(unsorted_list[[ii]])) {
                unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
            }
        }
        unsorted_list[sort(names(unsorted_list))] 
    }
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                            args = c("--sd=3", "--quietly"))),
                sort_list(list(sd = 3, verbose = FALSE, help = FALSE, 
                    count = 5, mean = 0, generator = "rnorm")))
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                        args = character(0), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = TRUE, 
                                count = 5, mean = 0, generator = "rnorm"),
                            args = character(0))))
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                            args = c("-c", "10"))),
                sort_list(list(sd = 1, help = FALSE, verbose = TRUE, 
                            count = 10, mean = 0, generator = "rnorm")))
    expect_equal(sort_list(parse_args(parser, args = c("--add_numbers", "example.txt"), 
                            positional_arguments = TRUE)),
                sort_list(list(options = list(add_numbers = TRUE, help = FALSE), 
                             args = c("example.txt"))))
    expect_equal(sort_list(parse_args(parser, args = c("--add_numbers"), 
                            positional_arguments = TRUE)),
                sort_list(list(options = list(add_numbers = TRUE, help = FALSE), 
                             args = character(0))))
    expect_equal(sort_list(parse_args(parser, args = c("-add_numbers", "example.txt"), 
                                positional_arguments = TRUE)),
                sort_list(list(options = list(add_numbers = FALSE, help = FALSE), 
                             args = c("-add_numbers", "example.txt"))))
    expect_that(parse_args(parser, args = c("-add_numbers", "example.txt")), throws_error())

    # test bug found by Juan Carlos Borrás
    optlist <- list(
     make_option(c("-s", "--substitutions"), type="character",
    dest="substitutions", default=NULL,
       help='String of the form "KEY1=VALUE1 KEY2=VALUE2 ... KEY=VALUE"
    stating the SQL template substitutions',
       metavar="substitution-list")
    )
    optparser <- OptionParser(option_list=optlist)
    opt <- parse_args(optparser, c("-s", "FOO=bar"))
    opt_alt <- parse_args(optparser, c("--substitutions=FOO=bar"))
    expect_that(opt, equals(opt_alt))
})
