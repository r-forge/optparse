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
sort_list <- function(unsorted_list) {
    for(ii in seq(along=unsorted_list)) {
        if(is.list(unsorted_list[[ii]])) {
            unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
        }
    }
    unsorted_list[sort(names(unsorted_list))] 
}

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
    # option_list took outside test_that
    option_list2 <- list( 
        make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
            help="Print line number at the beginning of each line [default]")
        )
    parser <- OptionParser(usage = "\\%prog [options] file", option_list=option_list2)
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
})

# Bug found by Juan Carlos Borrás
test_that("test bug of multiple '=' signs", {
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

    # also check when positional_arguments is set to true, like later bug unit test
    opt <- parse_args(optparser, c("-s", "FOO=bar"), positional_arguments=TRUE)
    opt_alt <- parse_args(optparser, c("--substitutions=FOO=bar"), positional_arguments=TRUE)
    expect_that(opt, equals(opt_alt))
})

# Bug found by Jim Nikelski 
test_that("test bug when multiple short flag options '-abc' with positional_arguments = TRUE", {
    sort_list <- function(unsorted_list) {
        for(ii in seq(along=unsorted_list)) {
            if(is.list(unsorted_list[[ii]])) {
                unsorted_list[[ii]] <- sort_list(unsorted_list[[ii]])
            }
        }
        unsorted_list[sort(names(unsorted_list))] 
    }
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                        args = c("-qc", "10"), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = FALSE, 
                                count = 10, mean = 0, generator = "rnorm"),
                            args = character(0))))
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                        args = c("-qcde", "10"), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = TRUE, 
                                count = 5, mean = 0, generator = "rnorm"),
                            args = c("-qcde", "10"))))
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                        args = c("CMD", "-qc", "10", "bumblebee"), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = FALSE, 
                                count = 10, mean = 0, generator = "rnorm"),
                            args = c("CMD", "bumblebee"))))
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                        args = c("CMD", "-qc", "10", "--qcdefg", "--what-what", "bumblebee"), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = FALSE, 
                                count = 10, mean = 0, generator = "rnorm"),
                            args = c("CMD", "--qcdefg", "--what-what", "bumblebee"))))
})

# Bug found by Ino de Brujin and Benjamin Tyner 
test_that("test bug when long flag option with '=' with positional_arguments = TRUE", {
    expect_equal(sort_list(parse_args(OptionParser(option_list = option_list), 
                            args = c("--count=10"), positional_arguments = TRUE)),
                sort_list(list(options = list(sd = 1, help = FALSE, verbose = TRUE, 
                            count = 10, mean = 0, generator = "rnorm"),
                            args=character(0))))
})

# Bug found by Miroslav Posta
optlist = list(make_option(c("--tmin"), type="numeric", help="Startup time [sec]. "))
parser = OptionParser(option_list=optlist, usage="", epilogue="")
test_that("test bug with a NA short flag option with positional_arguments = TRUE", {
    expect_equal(sort_list(parse_args(args=c("-h", "foo"), parser, positional_arguments=TRUE, 
                                      print_help_and_exit=FALSE)),
                sort_list(list(options = list(help=TRUE), args="foo")))
})

context("print_help")
test_that("description and epilogue work as expected", {
    parser <- OptionParser()
    expect_output(print_help(parser), "Usage:")
    expect_output(print_help(parser), "Options:")
    parser2 <- OptionParser(usage="program", description="foo", epilogue="bar")
    expect_output(print_help(parser2), "foo")
    expect_output(print_help(parser2), "bar$")
    expect_output(print_help(parser2), "^Usage: ")
    expect_equal(stringr::str_count(
                capture.output(print_help(OptionParser("usage: foo bar")))[1],
                "[Uu]sage"), 1)

    # bug / feature request by Miroslav Posta
    parser = OptionParser(usage="test %prog test %prog", epilog="epilog test %prog %prog", 
                description="description %prog test %prog", prog="unit_test.r")
    expect_output(print_help(parser), 'Usage:.*unit_test.r.*unit_test.r')
    expect_output(print_help(parser), 'description unit_test.r test unit_test.r')
    expect_output(print_help(parser), 'epilog test unit_test.r unit_test.r')
})
