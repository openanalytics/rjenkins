
JenkinsJob <- function(conn, name, path, parent = NULL) {
  
  structure(
      list(conn = conn,
          name = name,
          path = path,
          parent = parent),
      class = c("JenkinsJob", "list"))
  
}

#' Possible Jenkins Build Refs
#' @description a \code{character()} vector containing all possible build references
#' @details
#' The following are valid build references: \itemize{
#' \item \code{lastBuild}
#' \item \code{lastCompletedBuild}
#' \item \code{lastFailedBuild}
#' \item \code{lastStableBuild}
#' \item \code{lastSuccessfulBuild}
#' \item \code{lastUnsuccessfulBuild}
#' }
#' @references https://wiki.jenkins.io/display/JENKINS/Terminology
#' @export
JENKINS_BUILD_REFS <- c("lastBuild", "lastCompletedBuild", "lastFailedBuild",
    "lastStableBuild", "lastSuccessfulBuild", "lastUnsuccessfulBuild")


#' Browse Jenkins objects in the web interface
#' @rdname browse
#' @importFrom httr modify_url
#' @export
browse.JenkinsJob <- function(x, ...) {
  
  browseURL(modify_url(x$conn$host, path = c(x$conn$contextPath, x$path)), ...)
  
}

#' Show a Jenkins Job
#' @param x object representing a jenkins job.
#' @param ... further arguments; not used
#' @importFrom httr modify_url
#' @export
print.JenkinsJob <- function(x, ...) {
  
  cat(sprintf("<jenkins job with url: %s>\n",
          modify_url(x$conn$host, path = c(x$conn$contextPath, x$path))))
  
}

#' Summarize Jenkins Job
#' @description Get summary information for the given jenkins job
#' @param object object of class \code{JenkinsJob} e.g. created using \code{\link{getJob}}
#' @param ... further arguments; not used
#' @importFrom methods show
#' @export
summary.JenkinsJob <- function(object, ...) {
  
  info <- getJobInfo(object)
  history <- getBuildHistory(object)
  artifacts <- listArtifacts(object, build = "lastSuccessfulBuild")
  
  cat("Job:\t", info$displayName, "\n")
  cat("Type:\t", info$itemType, "\n")
  
  if (!is.null(object$parent))
    cat("Parent:\t", getJobInfo(object$parent)$displayName,"\n")
  
  if (!is.null(info$description))
    cat("\t\t", info$description, "\n")
  
  cat("\n")
  
  cat("Build History:\n\n")
  
  show(history)
  
  cat("\n")
  
  cat("Last Successful Artifacts:\n\n")
  
  if (length(artifacts) > 0)
    show(artifacts)
  else
    cat("[no artifacts]\n")
  
  cat("\n")
  
}

#' Get General Job Information
#' @description Query some general information about a job such as it's
#' descriptione etc.
#' @template JenkinsJobOp
#' @return list with properties
#' @importFrom xml2 as_list xml_name
#' @export
getJobInfo <- function(job) {
  
  xml <- jenkinsGET(job$conn,
      path = job$path,
      tree = "description,displayName")
  
  result <- unlist(recursive = FALSE, as_list(xml)[[1]])
  result$itemType <- xml_name(xml)
  
  result
  
}

#' Get the Build History
#' @template JenkinsJobOp
#' @param depth maximum number of builds to list
#' @return \code{data.frame} where each row corresponds to one build
#' @importFrom xml2 xml_children xml_text xml_child
#' @export
getBuildHistory <- function(job, depth = 3) {
  
  if (depth < 1)
    stop("depth must be strictly positive")
  
  url <- modify_url(job$conn$host,
      path = c(job$conn$contextPath, job$path, "api", "xml"),
      query = list(
          xpath = "/*/build",
          wrapper = "builds",
          tree = "builds[number,result,timestamp]"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  result <- content(response)
  
  if (length(xml_children(result)) > 0) {
    
    parseChild <- function(x) data.frame(stringsAsFactors = FALSE,
          number = as.numeric(xml_text(xml_child(x, "number"))),
          result = xml_text(xml_child(x, "result")),
          timestamp = as.numeric(xml_text(xml_child(x, "timestamp"))))
    
    history <- do.call(rbind, lapply(xml_children(result), parseChild))
    
    history$success <- history$result == "SUCCESS"
    
    history$timestamp <- as.POSIXct(as.numeric(history$timestamp)/1000,
        origin="1970-01-01")
    
    depth <- min(nrow(history), depth)
    history[order(history$timestamp, decreasing = TRUE)[seq_len(depth)], ]
    
  } else {
    
    data.frame(number = numeric(),
        result = character(),
        timestamp = as.POSIXct(numeric(), origin = "1970-01-01"),
        success = logical())
    
  }
  
}

#' List Build Artifacts 
#' @template JenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @importFrom httr modify_url GET authenticate content http_error
#' @importFrom xml2 as_list
#' @export
listArtifacts <- function(job, build = JENKINS_BUILD_REFS) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  url <- modify_url(job$conn$host,
      path = c(job$conn$contextPath, job$path, build, "api", "xml"),
      query = list(xpath = "/*/artifact/relativePath", wrapper = "artifacts"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  if (http_error(response))
    character()
  else
    unlist(as_list(content(response))) 
  
}

#' Delete a job
#' @template JenkinsJobOp
#' @export
deleteJob <- function(job) {
  
  invisible(jenkinsPOST(job$conn, c(job$path, "doDelete"), xml = NULL))
  
}

#' Schedule a Build
#' @template JenkinsJobOp
#' @param params build parameters; named list
#' @importFrom httr http_status modify_url
#' @export
scheduleBuild <- function(job, params = NULL) {
  
  if (!is.null(params)) {
    
    if (!is.list(params) || length(names(params)) != length(params))
      stop("build parameters should be given as a named list")
    
    url <- modify_url(job$conn$host, path = c(job$conn$contextPath, job$path, "buildWithParameters"))
    body <- params
    
  } else {
    
    url <- modify_url(job$conn$host, path = c(job$conn$contextPath, job$path, "build"))
    body <- NULL
    
  }
  
  response <- POST(url,
      authenticate(job$conn$user, job$conn$token),
      crumbHeader(crumbRequest(job$conn)),
      body = body)
  
  if (status_code(response) %/% 100 == 2) message("Build scheduled.")
  
  if (status_code(response) == 400 && is.null(params)) {
    
    parametersQueryReponse <- GET(modify_url(job$conn$host, path = c(job$conn$contextPath, job$path, "api", "xml")),
        authenticate(job$conn$user, job$conn$token),
        query = list(
            xpath = "/*/property[@_class='hudson.model.ParametersDefinitionProperty']",
            wrapper = "matches"))
    
    stop_for_status(parametersQueryReponse)
    
    if (length(xml_children(content(parametersQueryReponse))) == 1) {
      warning("The job is parametrized but NULL was supplied; retrying with an empty list")
      scheduleBuild(job, list())
    } else {
      http_status(response)
    }
    
  } else {
    
    http_status(response)
    
  }
  
  invisible(job)
  
}

#' Schedule SCM Polling
#' @description TODO
#' @template JenkinsJobOp
#' @export
schedulePoll <- function(job) {
  
  res <- jenkinsPOST(job$conn, path = c(job$path, "polling"))
  
  if (res$status %/% 100 == 2) message("SCM polling scheduled.")
  
  invisible(job)
  
}


#' Retrieve the Build Log
#' @template JenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @param start byte offset
#' @export
getBuildLog <- function(job, build = JENKINS_BUILD_REFS, start = 0) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  jenkinsGET(job$conn,
      path = c(job$path, build, "logText", "progressiveText"),
      query = list(start = start))
  
}

#' @rdname listJobs
#' @export
listJobs.JenkinsJob <- function(x) {
  
  xml <- jenkinsGET(x$conn,
      path = x$path,
      xpath = "/*/job/name",
      wrapper = "jobs")
  
  unlist(as_list(xml), use.names = FALSE)
  
}

#' @rdname getJob
#' @export
getJob.JenkinsJob <- function(x, name) {
  
  stopifnot(hasJob(x, name))
  
  JenkinsJob(
      conn = x$conn,
      name = name,
      path = c(x$path, "job", name),
      parent = x)
  
}

#' @rdname hasJob
#' @importFrom httr HEAD modify_url authenticate status_code
#' @export
hasJob.JenkinsJob <- function(x, name) {
  
  jenkinsHEAD(x$conn, c(x$path, "job", name))$status %/% 100 == 2
  
}
