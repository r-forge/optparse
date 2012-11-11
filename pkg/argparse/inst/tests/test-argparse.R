context("Unit tests")

context("print_help")
test_that("print_help works as expected", {
    parser <- ArgumentParser(description="Process some integers.")
    expect_output(parser$print_help(), "usage:")
    expect_output(parser$print_help(), "optional arguments:")
    expect_output(parser$print_help(), "Process some integers.")
    expect_output(parser$print_usage(), "usage:")
    # expect_output(parser$parse_args("-h"), "usage:")
    # expect_output(parser$parse_args("--help"), "options:")
})
context("convert_..._to_arguments")
test_that("convert_..._to_arguments works as expected", {
    c.2a <- convert_..._to_arguments
    waz <- "wazzup"
    expect_equal(c.2a(foo="bar", hello="world"), "foo='bar', hello='world'")
    expect_equal(c.2a(foo="bar", waz), "foo='bar', 'wazzup'")
    expect_equal(c.2a(type="character"), "type=str")
    expect_equal(c.2a(default=TRUE), "default=True")
    expect_equal(c.2a(default=3.4), "default=3.4")
    expect_equal(c.2a(default="foo"), "default='foo'")
})

context("add_argument")
test_that("add_argument works as expected", {
    parser <- ArgumentParser()
    parser$add_argument('integers', metavar='N', type="integer", nargs='+',
                       help='an integer for the accumulator')
    parser$add_argument('--sum', dest='accumulate', action='store_const',
                       const='sum', default='max',
                       help='sum the integers (default: find the max)')
    expect_output(parser$print_help(), "sum the integers")
    expect_equal(parser$parse_args(c("--sum", "1", "2"))$accumulate, "sum")
    expect_equal(parser$parse_args(c("--sum", "1", "2"))$integers, c(1,2))
    expect_error(parser$add_argument("--foo", type="boolean"))
})
