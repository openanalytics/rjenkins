
#' Generate Declarative Jenkins Pipeline syntax from a pipeline expression
#' @description TODO refer to Jenkins 2 book
#' @rdname pipeline
#' @param pipelineExpr pipeline expression
#' @return Declarative Pipeline syntax as a \code{character()}
#' @export
jenkinsPipeline <- function(pipelineExpr) {
  
  blocks <- list(
      pipeline = blockOp("pipeline"),
      always = blockOp("always"),
      triggers = blockOp("triggers")
  )
  
  sections <- list(
      stages = sectionOp("stages"),
      steps = sectionOp("steps"),
      post = sectionOp("post")
  )
  
  directives <- list(
      agent = agentDirective,
      stage = stageDirective,
      docker = dockerDirective
  )
  
  steps <- list(
      echo = stepOp("echo"),
      step = step
  )
  
  envir <- list2env(c(blocks, sections, directives, steps), parent = emptyenv())
  
  eval(substitute(pipelineExpr), envir)
  
  # TODO escaping
  # structure(eval(pipelineExpr, envir), class = "Jenkinsfile") 
  
}

#' Docker Directive
#' @description DSL helper function
#' @return \code{character()}
dockerDirective <- function(image, args = NULL) {
  b <- blockOp("docker")
  
  if (is.null(args)) 
    b(step("image", image))
  else 
    b(step("image", image), step("args", args))
}

#' Agent Directive
#' @description DSL helper function
#' @return \code{character()}
agentDirective <- function(...) {
  args <- list(...)
  if (length(args) == 1 &&
      is.character(args[[1]]) &&
      any(args[[1]] == c("any", "none"))) {
    paste0("agent ", args[[1]], "\n")
  } else {
    blockOp("agent")(...)
  }
}

#' Format arguments and parameters
#' @description DSL helper function
#' @param value value to format
#' @return \code{character()}
formatParameter <- function(value) {
  if (is.logical(value)) {
    if (value) "true" else "false"
  } else if (is.numeric(value)) {
    sprintf("%s", value)
  } else {
    sprintf("'%s'", value)
  }
}

#' Step
#' @description DSL helper function
#' @param name step name
#' @return \code{character()}
#' @examples \dontrun{
#' step("a", foo = 4, "5", bar = 77, foobar = FALSE)
#' }
step <- function(name, ...) {
  
  args <- list(...)
  
  if (length(args) == 0) {
    paste0(name, "\n")
  } else {
    argStr <- sapply(args, formatParameter)
    argNames <- if (is.null(names(args))) rep("", length(args)) else names(args)
    paste0(
        name,
        " ",
        paste(collapse = ", ",
            ifelse(argNames == "",
                yes = argStr,
                no = sprintf("%s: %s", argNames, argStr))),
        "\n")
  }
}

#' Step
#' @description DSL helper function
#' @param name step name
#' @return closure
stepOp <- function(name) function(...) step(name, ...)


#' Stage Directive
#' @description DSL helper function
#' @param stageName stage name
#' @return \code{character()}
stageDirective <- function(stageName, ...) {
  
  blockOp(sprintf("stage(%s)", formatParameter(stageName)))(...)
  
}

#' Indent lines
#' @description block header
#' @param texts \code{character()} vector of text snippets ending in a newline
#' @return \code{character()}
indentLines <- function(text, indent = "    ") {
  
  if (length(i <- which(!grepl("\n$", text))) > 0)
    stop("snippet does not end in newline: ", text[i])
  
  paste0(indent, gsub("\n(.{1})", sprintf("\n%s\\1", indent), text))
  
}

#' Block
#' @description DSL helper function
#' @param header block header
#' @return closure
blockOp <- function(header) {
  force(header)
  
  function(...) {
    args <- list(...)
    
    paste0(
        header, " {\n",
        if (length(args) > 0)
          paste(indentLines(as.vector(args, "character")), collapse = ""),
        "}\n")
  }
}

#' Section
#' @description DSL helper function
#' @param name section name
#' @return closure
sectionOp <- function(name) blockOp(name)


#' Right-Variadic operator
#' @description DSL helper function
#' @param nf number of fixed arguments
#' @param nv minimum number of variable arguments
#' @param parts vector of length \code{n +_1} to interlace with the fixed arguments
#' @param left prefix for the variadic part
#' @param sep separator for the variadic part
#' @param right suffix for the variadic part
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
#' @description DSL helper function
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
#' @description DSL helper function
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

