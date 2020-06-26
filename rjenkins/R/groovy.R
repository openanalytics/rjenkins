
#' Groovy Closure
#' @param ... statements
#' @param parameters optional parameters: \code{character()}
#' @seealso \url{https://groovy-lang.org/closures.html}
#' @export
GroovyClosure <- function(..., parameters = character()) {
  
  structure(
      dropNull(list(...)),
      parameters = parameters,
      class = c("GroovyClosure", "list")
  )
  
}

#' Format a groovy closure
#' @param closure \link{\code{GroovyClosure}}
#' @return \code{character()}
#' @export
formatGroovyClosure <- function(closure) {
  opening <- if (length(attr(closure, "parameters")) == 0) "{"
      else sprintf("{ %s ->", toString(attr(closure, "parameters")))
  closing <- "}"
  paste(collapse = "",
      endLines(
          c(opening, indentLines(as.vector(closure, "character")), closing)
      )
  )
}

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

#' Format arguments and parameters
#' @param value value to format
#' @return \code{character()}
formatArgument <- function(value) {
  if (is.logical(value)) {
    if (value) "true" else "false"
  } else if (is.numeric(value)) {
    sprintf("%s", value)
  } else if (is(value, "AsIs")) {
    value
  } else if (is(value, "GString")) {
    formatGString(value)
  } else if (is(value, "GroovyClosure")) {
    formatGroovyClosure(value)
  } else {
    formatGString(GString(value))
  }
}
