
#' List job names
#' @rdname listJobs
#' @description S3 generic
#' @param x object to list jobs for
#' @export
listJobs <- function(x, ...) {
  
  UseMethod("listJobs")
  
}

#' Find a job
#' @rdname getJob
#' @description S3 generic
#' @param x job parent
#' @param name job name
#' @return object of class \code{jenkinsJob}
#' @export
getJob <- function(x, name, ...) {
  
  if (!hasJob(x, name)) {
    stop("Job with given name does not exist on the jenkins server.")
  }
  
  UseMethod("getJob")
  
}

#' Check if a job with given name exists
#' @param name job name
#' @export
hasJob <- function(x, name) {
  
  any(listJobs(x) == name)
  
}
