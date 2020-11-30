
#' Declarative Jenkins Pipeline
#' @rdname pipeline
#' @description Generate Declarative Jenkins Pipeline syntax from a pipeline
#' expression.
#' @details The pipeline expression will be evaluated in a new child
#' environment of the caller context. This environment contains R function
#' equivalents of the concepts (\link[=pipelineSections]{Sections},
#' \link[=pipelineDirectives]{Directives} and \link[=pipelineSteps]{Steps})
#' in the Pipeline Syntax.
#' Any object from the caller context can be referenced but name clashes
#' will be resolved in favor of the pipeline syntax, meaning it is not possible
#' to override e.g. \code{pipeline()}
#' @param pipelineExpr pipeline expression
#' @return Declarative Pipeline syntax as a \code{character()}
#' @example inst/example/jenkinsPipeline.R
#' @seealso pipelineSections pipelineDirectives pipelineSteps
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
      failure = blockOp("failure"),
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
#' @details \code{pipeline()} can be used as convenience shorthand for
#' \code{jenkinsPipeline(pipeline())}
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

#' Pipeline Steps
#' @references \url{https://jenkins.io/doc/pipeline/steps/}
#' @export
pipelineSteps <- list(
    echo = stepOp("echo"),
    sh = function(lines) step("sh", lines),
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

#' Pipeline Sections
#' @description Sections in Declarative Pipeline typically contain one or more
#' \code{\link{pipelineDirectives}} or \code{\link{pipelineSteps}}.
#' @export
pipelineSections <- list(
    stages = blockOp("stages"),
    parallel = blockOp("parallel"),
    steps = blockOp("steps"),
    post = blockOp("post")
)

directive <- function(header, allowedContent, ...) {
  content <- dropNull(eval(substitute(list(...)), list2env(allowedContent, parent = parent.frame(2))))
  
  opts <- names(content) != ""
  content[opts] <- Map(step, name = names(content[opts]), content[opts])
  
  do.call(blockOp(header), content)
}

#' Pipeline Directives
#' @references \url{https://www.jenkins.io/doc/book/pipeline/syntax/#declarative-directives}
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
      directive("when", conditions, ...)
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
      directive("triggers", triggers, ...)
    },
    
    options = function(...) {
      options <- list(
          buildDiscarder = naryOp(1, c("buildDiscarder(", ")")),
          logRotator = function(numToKeepStr) {
            sprintf("logRotator(numToKeepStr: %s)", formatArgument(numToKeepStr))
          }
      )
      directive("options", options, ...)
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
      
          any = function() "any",
      
          none = function() "none"
      
      )
      thisCall <- match.call()
      if (length(thisCall) - 1 > 0 && is(thisCall[[2]], "character")) {
        # TODO: should depecrate this in favor of symbol-based any and none
        # but need to test first if agent { any } is equivalent to agent any
        paste0("agent ", thisCall[[2]], "\n")
      } else {
        directive("agent", agents, ...)
      }
    },
    
    stage = function(stageName, ...) {
      directive(
          sprintf("stage(%s)", formatArgument(stageName)),
          c(  pipelineSections,
              pipelineSteps,
              pipelineDirectives[c("when", "agent", "environment")]),
          ...)
    }

)
