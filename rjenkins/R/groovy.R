
#' Groovy Method Call
#' @param .name method name
#' @param ... mix of unnamed and named arguments: \code{character()}
#' @seealso \url{http://docs.groovy-lang.org/docs/groovy-latest/html/documentation/#_named_parameters_2}
#' @export
GroovyCall <- function(.name, ...) {
  
  structure(
      list(
          name = .name,
          arguments = list(...)
      ),
      class = c("GrooyCall", "list")
  )
  
}

#' Format Groovy Call
#' @param call \code{\link{GroovyCall}}
#' @param brackets include brackets around arguments
#' @param closureSugar if the last argument is a closure, move it outside the
#' brackets 
#' @export
formatGroovyCall <- function(call, brackets = TRUE, closureSugar = FALSE) {
  
  args <- if (!is.null(names(call$arguments)) || is.null(call$arguments)) {
    call$arguments
  } else {
    setNames(call$arguments, rep("", length(call$arguments)))
  }

  nArgs <- length(args)
  
  if (closureSugar && nArgs >= 1 && is(args[[nArgs]], "GroovyClosure")) {
    sprintf(if (!brackets) "%s %s, %s" else "%s(%s) %s",
        call$name,
        formatGroovyCallArgs(args[-nArgs]),
        formatArgument(args[[nArgs]]))
  } else {
    sprintf(if (!brackets) "%s %s" else "%s(%s)",
        call$name,
        formatGroovyCallArgs(args))
  }
  
}

formatGroovyCallArgs <- function(args) {
  
  args <- sapply(args, formatArgument)
  
  namedArgs <- names(args) != ""
  
  paste(collapse = ", ",
      c(  sprintf("%s: %s",
              names(args[namedArgs]),
              args[namedArgs]),
          args[!namedArgs])
  )
}

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
#' @param closure \code{\link{GroovyClosure}}
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
