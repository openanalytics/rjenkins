
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

#' Indent lines
#' @param text \code{character()} vector of text snippets ending in a newline
#' @param indent \code{character()} string to indent with
#' @return \code{character()}
indentLines <- function(
    text,
    indent = strrep(" ", getOption("rjenkins.indent", 4))) {
  
  if (length(i <- which(!grepl("\n$", text))) > 0)
    stop("snippet does not end in newline: ", text[i])
  
  paste0(indent, gsub("\n(.{1})", sprintf("\n%s\\1", indent), text))
  
}

#' Add newlines to the end of text snippets
#' @param texts \code{character()} vector of text snippets
#' @return \code{character()}
endLines <- function(texts) {
  ifelse(!grepl("\n$", texts), paste0(texts, "\n"), texts)
}
