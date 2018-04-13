
#' Connect to a Jenkins Server
#' @description Create a connection object for the Jenkins API. Credentials
#' must be specified either directly or through a configuration object.
#' @param url jenkins endpoint
#' @param user user to connect with
#' @param token token to use for authentication
#' @param auth authentication credentials obtained via \code{\link{jenkinsAuth}}
#' @importFrom httr parse_url
#' @return object of class \code{jenkinsConnection}
#' @export
jenkinsConnection <- function(url = "http://localhost", user = NULL,
    token = NULL, auth = NULL) {
  
  if (!any(parse_url(url)$scheme == c("https", "http"))) {
    warning("Unsupported scheme")
  }
  
  if (!is.null(auth)) {
    user <- auth$user
    token <- auth$token
  }
  
  if (is.null(user) || is.null(token)) stop("missing credentials")
  
  structure(
      list(host = url,
          user = user,
          token = token),
      class = c("jenkinsConnection", "list"))
  
}

#' Show a Jenkins Object
#' @description Display information about a jenkins object.
#' @param x connection to a jenkins instance. Result of \code{\link{jenkinsConnection}} 
#' @param ... further arguments; not used
#' @export
print.jenkinsConnection <- function(x, ...) {
  
  cat(sprintf("<jenkins server with url: %s>", x$host))
  
}

#' Print summary information about the Jenkins Server
#' @param object connection to a jenkins instance. Result of \code{\link{jenkinsConnection}}
#' @param ... further arguments; not used
#' @export
summary.jenkinsConnection <- function(object, ...) {

	# TODO: what to do here?
  
  stop("not yet implemented")

}

#' List all jobs names
#' @template jenkinsOp
#' @importFrom httr modify_url GET authenticate content
#' @export
listJobs <- function(conn) {
  
  url <- modify_url(conn$host,
      path  = c("api", "xml"),
      query = list(xpath = "/*/job/name", wrapper = "jobs"))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  unlist(as_list(content(response)), use.names = FALSE)
  
}

#' Find a job on jenkins
#' @template jenkinsOp
#' @param name job name
#' @return object of class \code{jenkinsJob}
#' @export
getJob <- function(conn, name) {
  
  if (!hasJob(conn, name)) {
    stop("Job with given name does not exist on the jenkins server.")
  }
  
  jenkinsJob(conn, name)
   
}

#' Check if a job with given name exists
#' @template jenkinsOp
#' @param jobName job name
#' @export
hasJob <- function(conn, jobName) {
  
  any(listJobs(conn) == jobName)
  
}


#' Create a crumb header
#' @param crumb result of \link{crumbRequest}
#' @return request header, argument to \code{\link[httr]{POST}}
#' @importFrom stats setNames
#' @importFrom httr add_headers
#' @export
crumbHeader <- function(crumb) {
  
  crumb <- strsplit(crumb, ":")[[1]]
  
  add_headers(setNames(crumb[2], crumb[1]))
  
}

#' Request a crumb
#' @description Request a crumb to deal with CSRF Protection
#' @template jenkinsOp
#' @references https://wiki.jenkins.io/display/JENKINS/Remote+access+API#RemoteaccessAPI-CSRFProtection
#' @return crumb as \code{character()}
#' @importFrom httr modify_url authenticate GET content stop_for_status
#' @export
crumbRequest <- function(conn) {
  
  url <- modify_url(conn$host,
      path = c("crumbIssuer", "api", "xml"),
      query = list(xpath='concat(//crumbRequestField,":",//crumb)'))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  stop_for_status(response)
  
  content(response)
  
}

#' Create a new job
#' @description Creates a new job with given name and xml config.
#' @template jenkinsOp
#' @param name job name
#' @param config job xml specification given either as a file path or an object
#' of class \code{xml_document} from the \code{xml2} package
#' @seealso \link{crumbRequest}
#' @seealso \code{\link[xml2]{read_xml}}
#' @importFrom httr modify_url authenticate POST content_type_xml
#' @importFrom xml2 read_xml
#' @return jenkins job
#' @export
createJob <- function(conn, name, config) {
  
  if (!inherits(config, "xml_document")) {
    stopifnot(file.exists(config))
    config <- read_xml(config)
  }
  
  url <- modify_url(conn$host,
      path = "createItem",
      query = list(name = name))
  
  response <- POST(url, authenticate(conn$user, conn$token),
      content_type_xml(),
      crumbHeader(crumbRequest(conn)),
      body = as.character(config))
  
  stop_for_status(response)
  
  jenkinsJob(conn, name)
  
}



