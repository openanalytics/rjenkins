
#' Load a given resource into an HTML browser.
#' @description Load a resource into an HTML browser.
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
listJobs <- function(x, ...) {
  
  UseMethod("listJobs")
  
}

#' Find a job
#' @rdname getJob
#' @description S3 generic
#' @param x object to get job for
#' @export
getJob <- function(x, name, ...) {
  
  stopifnot(hasJob(x, name))
  
  UseMethod("getJob")
  
}

#' Check if a job exists
#' @rdname hasJob
#' @description S3 generic
#' @param x object to check jobs for
#' @return \code{logical}
#' @export
hasJob <- function(x, ...) {
  
  UseMethod("hasJob")
  
}
