
JenkinsView <- function(jenkins, path) {
  
  structure(
      list(conn = jenkins,
          path = path),
      class = c("JenkinsView", "list"))
  
}

#' @rdname browse
#' @importFrom httr modify_url
#' @importFrom curl curl_escape
#' @export
browse.JenkinsView <- function(x, ...) {
  
  browseURL(modify_url(x$conn$host, path = curl_escape(c(x$conn$contextPath, x$path))), ...)
  
}

#' Show a Jenkins View
#' @param x object representing a jenkins view.
#' @param ... further arguments; not used
#' @importFrom httr modify_url
#' @importFrom curl curl_escape
#' @export
print.JenkinsView <- function(x, ...) {
  
  cat(sprintf("<jenkins view with url: %s>\n",
          modify_url(x$conn$host, path = curl_escape(c(x$conn$contextPath, x$path)))))
  
}

#' Summarize a Jenkins View
#' @importFrom xml2 as_list
#' @export
summary.JenkinsView <- function(x, ...) {
  
  info <- as_list(jenkinsGET(x$conn, path = x$path))
  
  cat("View:\t", info$listView$name, "\n")
  
  invisible(info)
  
}

#' @rdname listJobs
#' @importFrom xml2 as_list
#' @export
listJobs.JenkinsView <- function(x, ...) {
  
  xml <- jenkinsGET(x$conn, path = x$path, xpath = "/*/job/name",
      wrapper = "jobs", ...)
  
  unlist(as_list(xml), use.names = FALSE)
  
}

#' @rdname getJob
#' @export
getJob.JenkinsView <- function(x, name) {
  stopifnot(hasJob(x, name[1]))
  JenkinsJob(x$conn, name[1], path = c(x$path, "job", name[1]))
}

#' @rdname hasJob
#' @export
hasJob.JenkinsView <- function(x, name) {
  jenkinsHEAD(x$conn, c(x$path, "job", name))$status %/% 100 == 2
}
