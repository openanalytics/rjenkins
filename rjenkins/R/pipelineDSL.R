
#' Declarative Jenkins Pipeline
#' @description Generate Declarative Jenkins Pipeline syntax from a pipeline expression.
#' @rdname pipeline
#' @param pipelineExpr pipeline expression
#' @return Declarative Pipeline syntax as a \code{character()}
#' @example inst/example/jenkinsPipeline.R
#' @references \url{https://jenkins.io/doc/book/pipeline/syntax/}
#' @export
jenkinsPipeline <- function(pipelineExpr = pipeline()) {
  
  others <- list(
      pipeline = blockOp("pipeline"),
      always = blockOp("always"),
      changed = blockOp("changed"),
      fixed = blockOp("fixed"),
      regression = blockOp("regression"),
      success = blockOp("success"),
      aborted = blockOp("aborted"),
      step = step
  )
  
  envir <- list2env(c(
          pipelineSections,
          pipelineDirectives,
          pipelineSteps,
          others),
      parent = parent.frame())
  
  eval(substitute(pipelineExpr), envir)
  
}
#' @rdname pipeline
#' @export
pipeline <- function(...) eval(substitute(jenkinsPipeline(pipeline(...))))

#' Block
#' @param header block header
#' @return closure
blockOp <- function(header) {
  force(header)
  
  function(...) {
    sprintf("%s %s", header, formatGroovyClosure(GroovyClosure(...)))
  }
}

#' Step
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
    argStr <- sapply(args, formatArgument)
    argNames <- if (is.null(names(args))) rep("", length(args)) else names(args)
    paste0(
        name,
        " ",
        paste(collapse = ", ",
            ifelse(argNames == "",
                yes = argStr,
                no = sprintf("%s: %s", argNames, argStr))))
  }
}

#' Step
#' @param name step name
#' @return closure
stepOp <- function(name) function(...) step(name, ...)

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

#' Pipeline steps
#' @references \url{https://jenkins.io/doc/pipeline/steps/}
#' @export
pipelineSteps <- list(
    echo = stepOp("echo"),
    sh = function(lines, multiLine = FALSE) {
      if (!multiLine) step("sh", lines)
      else sprintf("sh '''\n%s\n'''", lines)
    },
    R = function(sexpr, options = "") {
      lines <- paste(collapse = "\n", deparse(substitute(sexpr)))
      pipelineSteps$sh(sprintf("R %s -e \\'%s\\'", options, lines))
    },
    container = function(containerName, ...) {
      blockOp(sprintf("container(%s)", formatArgument(containerName)))(...)
    },
    withDockerRegistry = function(..., url = NULL, credentialsId = NULL) {
      registry <- c(url = url, credentialsId = credentialsId)
      blockOp(
          sprintf("withDockerRegistry([%s])",
              paste(collapse = ", ", sprintf('%s: "%s"', names(registry), registry))))(...)
    }
)

#' Pipeline sections
#' @export
pipelineSections <- list(
    stages = blockOp("stages"),
    parallel = blockOp("parallel"),
    steps = blockOp("steps"),
    post = blockOp("post")
)

directive <- function(header, content, maxExpr, ...) {
  expr <- substitute(list(...))
  if (length(expr) - 1 > maxExpr)
    stop(sprintf("%s directive accepts only %i subexpression(s) but %i were provided",
            header, maxExpr, length(match.call()) - 4))
  do.call(
      blockOp(header),
      eval(substitute(list(...)), list2env(content, parent = parent.frame(2))))
}

#' Pipeline directives
#' @export
pipelineDirectives <- list(
    
    when = function(...) {
      conditions <- list(
          branch = stepOp("branch"),
          buildingTag = stepOp("buildingTag"),
          changelog = stepOp("changelog"),
          changeset = stepOp("changeset"),
          changeRequest = stepOp("changeRequest"),
          environment = stepOp("environment"),
          equals = stepOp("equals"),
          expression = function(x) I(x),
          tag = stepOp("tag"),
          triggeredBy = stepOp("triggeredBy"),
          not = blockOp("not"),
          allOf = blockOp("allOf"),
          anyOf = blockOp("anyOf")
      )
      directive("when", conditions, Inf, ...)
    },
    
    environment = function(...) {
      do.call(
          blockOp("environment"),
          Map(
              function(name, value) sprintf("%s = %s", name, formatArgument(value)),
              name = names(c(...)),
              value = list(...)))
    },
    
    triggers = function(...) {
      triggers <- list(
          cron = function(x) naryOp(1, c("cron(", ")"))(formatArgument(x)),
          pollSCM = function(x) naryOp(1, c("pollSCM(", ")"))(formatArgument(x))
      )
      directive("triggers", triggers, Inf, ...)
    },
    
    options = function(...) {
      options <- list(
          buildDiscarder = naryOp(1, c("buildDiscarder(", ")")),
          logRotator = function(numToKeepStr) {
            sprintf("logRotator(numToKeepStr: %s)", formatArgument(numToKeepStr))
          }
      )
      directive("options", options, Inf, ...)
    },
    
    agent = function(...) {
      agents <- list(
          docker = function(image = NULL, args = NULL, label = NULL,
              reuseNode = NULL, customWorkspace = NULL, registryUrl = NULL,
              registryCredentialsId = NULL, alwaysPull = NULL) {
            blockOp("docker")(
                image %&&% step("image", image),
                args %&&% step("args", args),
                label %&&% step("label", label),
                reuseNode %&&% step("reuseNode", reuseNode),
                customWorkspace %&&% step("customWorkspace", customWorkspace),
                registryCredentialsId %&&% step("registryCredentialsId",registryCredentialsId),
                registryUrl %&&% step("registryUrl", registryUrl),
                alwaysPull %&&% step("alwaysPull", alwaysPull)
            )
          },
          
          dockerfile = function(fileName = NULL, reuseNode = NULL) {
            blockOp("dockerfile")(
                filename %&&% step("filename", fileName),
                reuseNode %&&% step("reuseNode", reuseNode)
            )
          },
          
          kubernetes = function(yaml = NULL, defaultContainer = NULL) {
            blockOp("kubernetes")(
                yaml %&&% step("yaml", yaml),
                defaultContainer %&&% step("defaultContainer", defaultContainer)
            )
          },
      
          any = "any",
      
          none = "none"
      
      )
      thisCall <- match.call()
      if (length(thisCall) - 1 > 0 && is(thisCall[[2]], "character")) {
        # TODO: should depecrate this in favor of symbol-based any and none
        # but need to test first if agent { any } is equivalent to agent any
        paste0("agent ", thisCall[[2]], "\n")
      } else {
        directive("agent", agents, 1, ...)
      }
    },
    
    stage = function(stageName, ...) {
      directive(
          sprintf("stage(%s)", formatArgument(stageName)),
          c(  pipelineSections,
              pipelineSteps,
              pipelineDirectives[c("when", "agent", "environment")]),
          Inf,
          ...)
    }

)
