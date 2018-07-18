# Get rid of the parentheses after taxnonomic levels
remove_end_paren <- function(x, pattern = "([:alnum:]|[:punct:])+(?=\\([:alnum:]+\\))") {
  str_extract(x, pattern)
}

