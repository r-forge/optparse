
python_code = c("import argparse",
"",
"parser = argparse.ArgumentParser(description='Process some integers.')",
"parser.add_argument('integers', metavar='N', type=int, nargs='+',",
"                   help='an integer for the accumulator')",
"parser.add_argument('--sum', dest='accumulate', action='store_const',",
"                   const=sum, default=max,",
"                   help='sum the integers (default: find the max)')",
"",
"args = parser.parse_args(['--sum', '7', '-1', '42'])",
"print(args.__dict__)",
"",
"args = parser.parse_args(['-h'])")

ArgumentParser <- function() {
    require("proto")
    proto(python_code = python_code,
            parse_args = function(.) {
                default_cmd <- ifelse(.Platform$OS.type == "windows", "python.exe", "python")
                python_cmd <- getOption("python_cmd", default_cmd)
                system(python_cmd, input=.$python_code, intern=TRUE)   
            }
        )
}

convert_..._to_arguments <- function(..., should_convert_type=FALSE) {
    argument_list <- list(...)
    argument_names <- names(argument_list)
    equals <- ifelse(argument_names == "", "", "=")
    arguments <- as.character(argument_list)
    # shQuote
    return(paste(argument_names, equals, arguments, sep="", collapse=", "))
}

parser <- ArgumentParser()
print(parser$parse_args())

f <- function(...) { convert_..._to_arguments(...) }
print(f('-h', foo="bar", hello="world"))

g <- function(x, z, y) { cat(x, y, z) }
do.call(g, list(1, y=2, z=3))
