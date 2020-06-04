
#' Groovy String
#' @param x \code{character} to use as the string base
#' @param multiLine allow string to span multiple lines
#' @param interpolation allow groovy string interpolation
#' @seealso \url{https://groovy-lang.org/syntax.html#all-strings}
#' @export
GString <- function(
    x,
    multiLine = grepl("\n", x),
    interpolation = grepl("\\$\\{[^\\{]*\\}", x)) {
  
  structure(
      x,
      multiLine = multiLine,
      interpolation = interpolation,
      class = c("GString", class(x))
  )
  
}

#' Format a groovy string
#' @param gstring the groovy string to format
#' @export
formatGString <- function(gstring) {
  
  sprintf("%1$s%2$s%1$s",
      strrep(
          if (attr(gstring, "interpolation")) "\"" else "'",
          if (attr(gstring, "multiLine")) 3 else 1),
      gstring)
  
}

#' Read a test file
readTestFile <- function(fileName) {
  paste(collapse = "\n",
      readLines(
          system.file(
              "testdata",
              fileName,
              package = "rjenkins",
              mustWork = TRUE)))
}
