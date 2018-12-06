
#' Load resource into an HTML Browser
#' @rdname browse
#' @description S3 generic wrapper replacement of \code{\link[utils]{browseURL}}
#' @param x resource to browse
#' @param ... further arguments to \code{\link[utils]{browseURL}}
#' @export 
browse <- function(x, ...) {
  
  UseMethod("browse")
  
}
#' @rdname browse
#' @importFrom utils browseURL
#' @export
browse.character <- function(x, ...) browseURL(x, ...)


#' List job names
#' @rdname listJobs
#' @description S3 generic
#' @param x object to list jobs for
#' @export
listJobs <- function(x) {
  
  UseMethod("listJobs")
  
}

#' Find a job
#' @rdname getJob
#' @description S3 generic
#' @param x job parent
#' @param name job name
#' @return object of class \code{jenkinsJob}
#' @export
getJob <- function(x, name) {
  
  if (!hasJob(x, name)) {
    stop("Job with given name does not exist on the jenkins server.")
  }
  
  UseMethod("getJob")
  
}

#' Check if a job with given name exists
#' @param x object to check jobs for
#' @param name job name
#' @export
hasJob <- function(x, name) {
  
  any(listJobs(x) == escapeJenkinsItemName(name))
  
}
