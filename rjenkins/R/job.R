# Jenkins job-level operations
# 
# Author: Daan Seynaeve
###############################################################################

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
#' @seealso \link{https://wiki.jenkins.io/display/JENKINS/Terminology}
#' @export
JENKINS_BUILD_REFS <- c("lastBuild", "lastCompletedBuild", "lastFailedBuild",
    "lastStableBuild", "lastSuccessfulBuild", "lastUnsuccessfulBuild")

#' Get a jenkins job
#' @description Create an object representing a jenkins job.
#' @template jenkinsOp
#' @param name job name
#' @export
jenkinsJob <- function(conn, name) {
  
  structure(
      list(conn = conn,
          name = name),
      class = c("jenkinsJob", "list"))
  
}

#' Summarize Jenkins Job
#' @description Get summary information for the given jenkins job
#' @template jenkinsJobOp
#' @export
summary.jenkinsJob <- function(job) {
  
  info <- getJobInfo(job)
  history <- getBuildHistory(job)
  artifacts <- listArtifacts(job, build = "lastSuccessfulBuild")
  
  cat("Job:\t", info$displayName, "\n")
  cat("\t\t", info$description, "\n")
  
  cat("\n")
  
  cat("Build History:\n\n")
  
  show(history)
  
  cat("\n")
  
  cat("Last Successful Artifacts:\n\n")
  
  show(artifacts)
  
  cat("\n")
  
}

#' Get General Job Information
#' @description Query some general information about a job such as it's
#' descriptione etc.
#' @template jenkinsJobOp
#' @return list with properties
#' @export
getJobInfo <- function(job) {
  
  url <- modify_url(job$conn$host,
      path = c("job", job$name, "api", "xml"),
      query = list(
          tree = "description,displayName"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  result <- unlist(recursive = FALSE, as_list(content(response))[[1]])
  
}

#' Get the Build History
#' @template jenkinsJobOp
#' @param depth maximum number of builds to list
#' @return \code{data.frame} where each row corresponds to one build
#' @importFrom xml2 xml_children
#' @export
getBuildHistory <- function(job, depth = 3) {
  
  if (depth < 1) {
    stop("depth must be strictly positive")
  }
  
  url <- modify_url(job$conn$host,
      path = c("job", job$name, "api", "xml"),
      query = list(
          xpath = "/*/build",
          wrapper = "builds",
          tree = "builds[number,result,timestamp]"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  result <- content(response)
  
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
  
}

#' List Build Artifacts 
#' @template jenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @importFrom httr modify_url GET authenticate content
#' @importFrom xml2 as_list
#' @export
listArtifacts <- function(job, build = JENKINS_BUILD_REFS) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  url <- modify_url(job$conn$host,
      path = c("job", job$name, build, "api", "xml"),
      query = list(xpath = "/*/artifact/relativePath", wrapper = "artifacts"))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  unlist(as_list(content(response))) 
}


#' Schedule a Build
#' @template jenkinsJobOp
#' @param params build parameters; currently not supported yet.
#' @export
scheduleBuild <- function(job, params = NULL) {
  
  if (!is.null(params)) stop("build parameters are not supported yet") # FIXME
  
  url <- modify_url(job$conn$host,
      path = c("job", job$name, "build"))
  
  response <- POST(url,
      authenticate(job$conn$user, job$conn$token),
      crumbHeader(crumbRequest(conn)))
  
  http_status(response)
  
}

#' Schedule SCM Polling
#' @description TODO
#' @template jenkinsJobOp
#' @export
schedulePoll <- function(job) {
  
  # FIXME
  
}


#' Retrieve the Build Log
#' @template jenkinsJobOp
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @param start byte offset
#' @importFrom httr modify_url GET authenticate content
#' @export
getBuildLog <- function(job, build = JENKINS_BUILD_REFS, start = 0) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  url <- modify_url(job$conn$host,
      path = c("job", job$name, build, "logText", "progressiveText"),
      query = list(start = start))
  
  response <- GET(url, authenticate(job$conn$user, job$conn$token))
  
  content(response)
  
}