library("testthat")
library("getopt")
library("argparse")

context("Unit tests")

context("make_argument")
test_that("make_argument works as expected", {
    expect_equal(make_argument("--integer", type="integer", default=5),
                make_argument("--integer", default=as.integer(5)))
    expect_equal(make_argument("--logical", type="logical", default="TRUE"),
                make_argument("--logical", default=TRUE))
    expect_that(make_argument("badflag"), throws_error())
})


context("parse_args")
test_that("parse_args works as expected", {
    argument_list <- list( 
        make_argument(c("-v", "--verbose"), action="store_true", default=TRUE,
            help="Print extra output [default]"),
        make_argument(c("-q", "--quietly"), action="store_false", 
            dest="verbose", help="Print little output"),
        make_argument(c("-c", "--count"), type="integer", default=5, 
            help="Number of random normals to generate [default \\%default]",
            metavar="number"),
        make_argument("--generator", default="rnorm", 
            help = "Function to generate random deviates [default \"\\%default\"]"),
        make_argument("--mean", default=0, 
            help="Mean if generator == \"rnorm\" [default \\%default]"),
        make_argument("--sd", default=1, metavar="standard deviation",
            help="Standard deviation if generator == \"rnorm\" [default \\%default]")
        )
    argument_list2 <- list( 
        make_argument(c("-n", "--add_numbers"), action="store_true", default=FALSE,
            help="Print line number at the beginning of each line [default]")
        )
    parser <- ArgumentParser(argument_list=argument_list)
    parser2 <- ArgumentParser(usage = "\\%prog [options] file", argument_list=argument_list2)
    sort_list <- function(unsorted_list) {
        for(ii in seq(along=unsorted_list)) {
            if(is.list(unsorted_list[[ii]])) {
                unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
            }
        }
        unsorted_list[sort(names(unsorted_list))] 
    }
    expect_equal(sort_list(parse_args(parser, args = c("--sd=3", "--quietly"))),
                sort_list(list(sd = 3, verbose = FALSE, help = FALSE, 
                    count = 5, mean = 0, generator = "rnorm")))
    expect_equal(sort_list(parse_args(parser, args = c("-c", "10"))),
                sort_list(list(sd = 1, help = FALSE, verbose = TRUE, 
                            count = 10, mean = 0, generator = "rnorm")))
    expect_that(parse_args(parser2, args = c("-add_numbers", "example.txt")), throws_error())

    # verbose should be false
    expect_that(parse_args(parser, "-q")$verbose, is_false())
    expect_that(print_help(parser), prints_text("usage"))
})
