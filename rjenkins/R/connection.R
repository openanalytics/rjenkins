
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
#' @importFrom xml2 as_list
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

#' Get the Build Queue
#' @description Get a summary of queued job builds
#' @template jenkinsOp
#' @return Queued job builds summary as a \code{data.frame} or \code{NULL} if
#' the queue is empty.
#' @importFrom httr GET authenticate content
#' @importFrom xml2 xml_text xml_children xml_find_first
#' @export
getBuildQueue <- function(conn) {
  
  # TODO: trim unneeded info with xpath
  
  url <- modify_url(conn$host,
      path = c("queue", "api", "xml"))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  result <- content(response)
  
  if (length(xml_children(result) == 0)) {
    
    return(NULL)
    
  } else {
    
    parseChild <- function(x) data.frame(stringsAsFactors = FALSE,
          jobName = xml_text(xml_find_first(x, "task/name")),
          inQueueSince = as.numeric(xml_text(xml_find_first(x, "inQueueSince"))),
          shortDescription = xml_text(xml_find_first(x, "shortDescription")))
    
    queue <- do.call(rbind, lapply(xml_children(result), parseChild))
    
    queue$inQueueSince <- as.POSIXct(as.numeric(queue$inQueueSince)/1000,
        origin="1970-01-01")
    
    return(queue)
    
  }
  
}


