
readTestFile <- function(fileName) {
  paste(collapse = "\n",
      readLines(
          system.file(
              "testdata",
              fileName,
              package = "rjenkins",
              mustWork = TRUE)))
}

#' Indent lines
#' @param text \code{character()} vector of text snippets
#' @param indent \code{character()} string to indent with
#' @return \code{character()}
indentLines <- function(
    text,
    indent = strrep(" ", getOption("rjenkins.indent", 4))) {
  
  # if (length(i <- which(!grepl("\n$", text))) > 0)
  #   stop("snippet does not end in newline: ", text[i])
  
  if (length(text) > 0)
    paste0(indent, gsub("\n(.{1})", sprintf("\n%s\\1", indent), text))
  else character()
  
}

#' Add newlines to the end of text snippets
#' @param texts \code{character()} vector of text snippets
#' @return \code{character()}
endLines <- function(texts) {
  if (length(texts) > 0)
    ifelse(!grepl("\n$", texts), paste0(texts, "\n"), texts)
  else character()
}

dropNull <- function(x) {
  Filter(Negate(is.null), x)
}

`%&&%` <- function(a, b) {
  if (is.null(a)) NULL else b
}

#' Right-Variadic operator
#' @param nf number of fixed arguments
#' @param nv minimum number of variable arguments
#' @param parts vector of length \code{n +_1} to interlace with the fixed arguments
#' @param left prefix for the variadic part
#' @param sep separator for the variadic part
#' @param right suffix for the variadic part
#' @return closure
rightVariadicOp <- function(nf, nv, parts, left, sep, right) {
  
  opf <- naryOp(nf, parts)
  opv <- variadicOp(nv, left, sep, right)
  
  function(...) {
    args <- list(...)
    if (length(args) < nf)
      stop(sprintf("Operator takes at least %i fixed arguments", nf))
    
    paste0(
        do.call(opf, args[1:nf]),
        do.call(opv, args[(nf+1):length(args)])
    )
    
  }
  
}

#' N-ary operator
#' @param n exact number of arguments
#' @param parts vector of length \code{n +_1} to interlace with the arguments
#' @return closure
naryOp <- function(n, parts) {
  force(n)
  force(parts)
  
  stopifnot(length(parts) == n + 1)
  
  function(...) {
    args <- list(...)
    
    if (length(args) != n)
      stop(sprintf("Operator %s takes exactly %i arguments", parts[1], n))
    
    paste0(parts, c(args,""), collapse = "")
  }
}

#' Variadic operator
#' @param n minimum number of arguments
#' @param left text to use as a prefix
#' @param right text to use as a suffix
#' @param sep seperator text
#' @return closure
variadicOp <- function(n, left, sep, right) {
  force(n)
  force(left)
  force(right)
  force(sep)
  
  function(...) {
    args <- list(...)
    
    if (length(args) < n)
      stop(sprintf("Function %s takes at least %i arguments", left, n))
    
    paste0(left, paste(args, collapse = sep), right)
  }
}
