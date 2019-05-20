
#' Block
#' @description DSL helper function
#' @param header block header
#' @return closure
blockOp <- function(header) {
  force(header)
  
  function(...) {
    args <- Filter(Negate(is.null), list(...))
    
    paste0(
        header, " {\n",
        if (length(args) > 0)
          paste(indentLines(endLines(as.vector(args, "character"))), collapse = ""),
        "}\n")
  }
}

#' Step
#' @description DSL helper function
#' @param name step name
#' @param ... step arguments
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



#' Pipeline sections
#' @description DSL helper functions
#' @export
pipelineSections <- list(
    stages = blockOp("stages"),
    steps = blockOp("steps"),
    post = blockOp("post")
)

#' Pipeline directives
#' @description DSL helper functions
#' @export
pipelineDirectives <- list(
    
    triggers = function(...) {
      triggers <- list(
          cron = function(x) naryOp(1, c("cron(", ")"))(formatParameter(x)),
          pollSCM = function(x) naryOp(1, c("pollSCM(", ")"))(formatParameter(x))
      )
      do.call(
          blockOp("triggers"),
          eval(substitute(list(...)),
              list2env(triggers, parent = parent.frame())))
    },
    
    options = function(...) {
      options <- list(
          buildDiscarder = naryOp(1, c("buildDiscarder(", ")")),
          logRotator = function(numToKeepStr) {
            sprintf("logRotator(numToKeepStr: %s)", formatParameter(numToKeepStr))
          }
      )
      envir <- list2env(options, parent = parent.frame())
      do.call(
          blockOp("options"),
          eval(substitute(list(...)), envir))
    },
    
    docker = function(image = NULL, args = NULL, label = NULL,
        reuseNode = NULL, customWorkspace = NULL, registryUrl = NULL,
        registryCredentialsId = NULL, alwaysPull = NULL) {
      do.call(blockOp("docker"),
          Filter(Negate(is.null), list(
                  if (!is.null(image)) step("image", image),
                  if (!is.null(args)) step("args", args),
                  if (!is.null(label)) step("label", label),
                  if (!is.null(reuseNode)) step("reuseNode", reuseNode),
                  if (!is.null(customWorkspace)) step("customWorkspace", customWorkspace),
                  if (!is.null(registryCredentialsId)) step("registryCredentialsId", registryCredentialsId),
                  if (!is.null(registryUrl)) step("registryUrl", registryUrl),
                  if (!is.null(alwaysPull)) step("alwaysPull", alwaysPull)
              )))
    },
    
    dockerfile = function(fileName = NULL, reuseNode = NULL) {
      do.call(blockOp("dockerfile"),
          Filter(Negate(is.null), list(
                  if (!is.null(fileName)) step("filename", fileName),
                  if (!is.null(reuseNode)) step("reuseNode", reuseNode)
              )))
    },
    
    agent = function(...) {
      args <- list(...)
      if (length(args) == 1 &&
          is.character(args[[1]]) &&
          any(args[[1]] == c("any", "none"))) {
        paste0("agent ", args[[1]], "\n")
      } else {
        blockOp("agent")(...)
      }
    },
    
    stage = function(stageName, ...) {
      
      blockOp(sprintf("stage(%s)", formatParameter(stageName)))(...)
      
    }
    
)

#' Generate Declarative Jenkins Pipeline syntax from a pipeline expression
#' @description TODO refer to Jenkins 2 book
#' @rdname pipeline
#' @param pipelineExpr pipeline expression
#' @return Declarative Pipeline syntax as a \code{character()}
#' @export
jenkinsPipeline <- function(pipelineExpr) {
  
  others <- list(
      pipeline = blockOp("pipeline"),
      always = blockOp("always"),
      echo = stepOp("echo"),
      step = step
  )
  
  envir <- list2env(c(
          pipelineSections,
          pipelineDirectives,
          others),
      parent = parent.frame())
  
  eval(substitute(pipelineExpr), envir)
  
  # TODO escaping
  # structure(eval(pipelineExpr, envir), class = "Jenkinsfile") 
  
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

#' Indent lines
#' @description DSL helper function
#' @param text \code{character()} vector of text snippets ending in a newline
#' @param indent \code{character()} string to indent with
#' @return \code{character()}
indentLines <- function(text, indent = "    ") {
  
  if (length(i <- which(!grepl("\n$", text))) > 0)
    stop("snippet does not end in newline: ", text[i])
  
  paste0(indent, gsub("\n(.{1})", sprintf("\n%s\\1", indent), text))
  
}

#' Add newlines to the end of text snippets
#' @description DSL helper function
#' @param texts \code{character()} vector of text snippets
#' @return \code{character()}
endLines <- function(texts) {
  ifelse(!grepl("\n$", texts), paste0(texts, "\n"), texts)
}

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

