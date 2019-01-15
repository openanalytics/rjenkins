
#' Jenkins Job
#' @description Create an object representing a jenkins job.
#' @template jenkinsOp
#' @param name job name
#' @param parent (optional) parent job
#' @export
jenkinsJob <- function(conn, name, parent = NULL) {
  
  structure(
      list(conn = conn,
          name = name,
          parent = parent),
      class = c("jenkinsJob", "list"))
  
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


#' Browse a Jenkins Job in the Jenkins web interface
#' @rdname browse
#' @importFrom httr modify_url
#' @export
browse.jenkinsJob <- function(x, ...) {
  
  browseURL(modify_url(x$conn$host, path = x$name), ...)
  
}

#' Show a Jenkins Job
#' @param x object representing a jenkins job.
#' @param ... further arguments; not used
#' @seealso \code{\link{getJob}}
#' @importFrom httr modify_url
#' @export
print.jenkinsJob <- function(x, ...) {
  
  cat(sprintf("<jenkins job with url: %s>\n",
          modify_url(x$conn$host, path = x$name)))
  
}

#' Summarize Jenkins Job
#' @description Get summary information for the given jenkins job
#' @param object object of class \code{jenkinsJob} e.g. created using \code{\link{getJob}}
#' @param ... further arguments; not used
#' @importFrom methods show
#' @export
summary.jenkinsJob <- function(object, ...) {
  
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
#' @template jenkinsJobOp
#' @return list with properties
#' @importFrom xml2 as_list xml_name
#' @export
getJobInfo <- function(job) {
  
  url <- modify_url(job$conn$host,
      path = c(job$name, "api", "xml"),
      query = list(
          tree = "description,displayName"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  result <- unlist(recursive = FALSE, as_list(content(response))[[1]])
  result$itemType <- xml_name(content(response))
  
  result
  
}

#' Get the Build History
#' @template jenkinsJobOp
#' @param depth maximum number of builds to list
#' @return \code{data.frame} where each row corresponds to one build
#' @importFrom xml2 xml_children xml_text xml_child
#' @export
getBuildHistory <- function(job, depth = 3) {
  
  if (depth < 1)
    stop("depth must be strictly positive")
  
  url <- modify_url(job$conn$host,
      path = c(job$name, "api", "xml"),
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
#' @template jenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @importFrom httr modify_url GET authenticate content http_error
#' @importFrom xml2 as_list
#' @export
listArtifacts <- function(job, build = JENKINS_BUILD_REFS) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  url <- modify_url(job$conn$host,
      path = c(job$name, build, "api", "xml"),
      query = list(xpath = "/*/artifact/relativePath", wrapper = "artifacts"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  if (http_error(response))
    character()
  else
    unlist(as_list(content(response))) 
  
}


#' Schedule a Build
#' @template jenkinsJobOp
#' @param params build parameters; named list
#' @importFrom httr http_status
#' @export
scheduleBuild <- function(job, params = NULL) {
  
  if (!is.null(params)) {
    
    if (!is.list(params) || is.null(names(params)))
      stop("build paramaters should be given as a named list")
    
    url <- modify_url(job$conn$host, path = c(job$name, "buildWithParameters"))
    body <- params
    
  } else {
    
    url <- modify_url(job$conn$host, path = c(job$name, "build"))
    body <- NULL
    
  }
  
  response <- POST(url,
      authenticate(job$conn$user, job$conn$token),
      crumbHeader(crumbRequest(job$conn)),
      body = body)
  
  http_status(response)
  
}

#' Schedule SCM Polling
#' @description TODO
#' @template jenkinsJobOp
#' @export
schedulePoll <- function(job) {
  
  # FIXME
  
  stop("not yet implemented")
  
}


#' Retrieve the Build Log
#' @template jenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @param start byte offset
#' @importFrom httr modify_url GET authenticate content stop_for_status
#' @export
getBuildLog <- function(job, build = JENKINS_BUILD_REFS, start = 0) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  url <- modify_url(job$conn$host,
      path = c(job$name, build, "logText", "progressiveText"),
      query = list(start = start))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  stop_for_status(response)
  
  content(response)
  
}

#' @rdname listJobs
#' @export
listJobs.jenkinsJob <- function(x) {
  
  url <- modify_url(x$conn$host,
      path = c(x$name, "api", "xml"),
      query = list(xpath = "/*/job/name", wrapper = "jobs"))
  
  response <- GET(url, authenticate(x$conn$user, x$conn$token))
  
  stop_for_status(response)
  
  unlist(as_list(content(response)), use.names = FALSE)
  
}

#' @rdname getJob
#' @export
getJob.jenkinsJob <- function(x, name) {
  
  jenkinsJob(
      conn = x$conn,
      name = sprintf("%s/job/%s", x$name, escapeJenkinsItemName(name)),
      parent = x)
  
}

#' @rdname hasJob
#' @export
hasJob.jenkinsJob <- function(x, name) {
  
  url <- modify_url(x$conn$host, path = c(x$name, "job", name))
  
  response <- HEAD(url, authenticate(x$conn$user, x$conn$token))
  
  status_code(response) == 200
  
}
