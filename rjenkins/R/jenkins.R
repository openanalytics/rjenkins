
#' Connect to jenkins
#' @description Create a connection object for the Jenkins API.
#' @param host jenkins endpoint
#' @param user user to connect with
#' @param token token to use for authentication
#' @return object of class \code{jenkinsConnection}
#' @export
jenkinsConnection <- function(host, user, token) {
  
  structure(
      list(host = host,
          user = user,
          token = token),
      class = c("jenkinsConnection", "list"))
  
}

#' Create a crumb header
#' @param crumb result of \link{crumbRequest}
#' @return request header, argument to \link{httr::POST}
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
#' @references \link{https://wiki.jenkins.io/display/JENKINS/Remote+access+API#RemoteaccessAPI-CSRFProtection}
#' @return crumb as \link{character()}
#' @importFrom httr modify_url authenticate GET content
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
#' @param job job name
#' @param config job xml specification given either as a file path or an object
#' of class \code{xml_document} from the \code{xml2} package
#' @seealso \link{crumbRequest}
#' @seealso \link{xml2::read_xml}
#' @importFrom httr modify_url authenticate POST content_type_xml
#' @importFrom xml2 read_xml
#' @return status code
#' @export
createJob <- function(conn, job, config) {
  
  if (!inherits(config, "xml_document")) {
    stopifnot(file.exists(config))
    config <- read_xml(config)
  }
  
  url <- modify_url(conn$host,
      path = "createItem",
      query = list(name = job))
  
  response <- POST(url, authenticate(conn$user, conn$token),
      content_type_xml(),
      crumbHeader(crumbRequest(conn)),
      body = as.character(config))
  
  stop_for_status(response)
  
  status_code(response)
  
}

#' Check if a job with given name exists
#' @template jenkinsOp
#' @param job job name
#' @export
hasJob <- function(conn, job) {
  
  any(listJobs(conn) == job)
  
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


#' List artifacts 
#' @param conn result of \link{jenkinsConnection}
#' @param job job name
#' @param build build number of identifier
#' @importFrom httr modify_url GET authenticate content
#' @importFrom xml2 as_list
#' @export
listJobArtifacts <- function(conn, job, build = "lastStableBuild") {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, "lastStableBuild")
  }
  
  url <- modify_url(conn$host,
      path = c("job", job, build, "api", "xml"),
      query = list(xpath = "/*/artifact/relativePath", wrapper = "artifacts"))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  unlist(as_list(content(response))) 
}

#' Retrieve the build log
#' @param conn result of \link{jenkinsConnection}
#' @param job job name
#' @param build build number of identifier
#' @param start byte offset
#' @importFrom httr modify_url GET authenticate content
#' @export
getBuildLog <- function(conn, job, build = "lastBuild", start = 0) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, c("lastStableBuild", "lastBuild"))
  }
  
  url <- modify_url(conn$host,
      path = c("job", job, build, "logText", "progressiveText"),
      query = list(start = start))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  content(response)
  
}


#' Install an exported artifact as package
#' @param conn result of \link{jenkinsConnection}
#' @param job job name
#' @param pkg name of the artifact that corresponds to the R package archive
#' to install
#' @param tmpDir temporary directory to store downloaded package archive
#' @param build build number of identifier
#' @importFrom httr modify_url GET authenticate write_disk
#' @importFrom utils install.packages
#' @export
install_jenkins <- function(conn, job, pkg, build = "lastStableBuild",
    tmpDir = normalizePath(file.path("~", "Downloads"))) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, "lastStableBuild")
  }
  
  url <- modify_url(conn$host,
      path = c("job", job, build, "artifact", pkg))
  
  response <- GET(url, write_disk(file.path(tmpDir, pkg), overwrite = TRUE),
      authenticate(conn$user, conn$token))
  
  install.packages(pkgs = file.path(tmpDir, pkg), repos = NULL, type = "source")
  
}
