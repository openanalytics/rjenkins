
#' Connect to a Jenkins Server
#' @description Create a connection object for the Jenkins API. Credentials
#' must be specified either directly or through a configuration object.
#' @param url jenkins endpoint
#' @param user user to connect with
#' @param token token to use for authentication
#' @param auth authentication credentials obtained via \code{\link{jenkinsAuth}}
#' @importFrom httr parse_url
#' @return object of class \code{Jenkins}
#' @export
jenkins <- function(url = "http://localhost", user = NULL,
    token = NULL, auth = NULL, contextPath = c()) {
  
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
          contextPath = contextPath,
          user = user,
          token = token),
      class = c("Jenkins", "list"))
  
}

#' Show a Jenkins Object
#' @description Display information about a jenkins object.
#' @param x connection to a jenkins instance. Result of \code{\link{jenkins}} 
#' @param ... further arguments; not used
#' @export
print.Jenkins <- function(x, ...) {
  
  cat(sprintf("<jenkins server with url: %s>\n", x$host))
  
}

#' Print summary information about the Jenkins Server
#' @param object connection to a jenkins instance. Result of \code{\link{jenkins}}
#' @param ... further arguments; not used
#' @export
summary.Jenkins <- function(object, ...) {

	print(object, ...)
  
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
#' @template JenkinsOp
#' @references https://wiki.jenkins.io/display/JENKINS/Remote+access+API#RemoteaccessAPI-CSRFProtection
#' @return crumb as \code{character()}
#' @importFrom httr modify_url authenticate GET content stop_for_status
#' @export
crumbRequest <- function(conn) {
  
  url <- modify_url(conn$host,
      path = c(conn$contextPath, "crumbIssuer", "api", "xml"),
      query = list(xpath='concat(//crumbRequestField,":",//crumb)'))
  
  response <- GET(url, authenticate(conn$user, conn$token))
  
  stop_for_status(response)
  
  content(response)
  
}

#' Get the Build Queue
#' @description Get a summary of queued job builds
#' @template JenkinsOp
#' @return Queued job builds summary as a \code{data.frame} or \code{NULL} if
#' the queue is empty.
#' @importFrom httr GET authenticate content
#' @importFrom xml2 xml_text xml_children xml_find_first
#' @export
getBuildQueue <- function(conn) {
  
  # TODO: trim unneeded info with xpath
  
  url <- modify_url(conn$host,
      path = c(conn$contextPath, "queue", "api", "xml"))
  
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

#' @rdname getJob
#' @param name job name
#' @export
getJob.Jenkins <- function(x, name, ...) {
  
  job <- JenkinsJob(
      x,
      name[1],
      path = c("job", name[1]))
  
  getJobRecursive(job, ...)
  
}

#' @rdname browse
#' @export
browse.Jenkins <- function(x, ...) {
  
  browseURL(modify_url(x$host, path = x$contextPath), ...)
  
}

#' @rdname listJobs
#' @importFrom httr modify_url GET authenticate content
#' @importFrom xml2 as_list
#' @export
listJobs.Jenkins <- function(x, ...) {
  
  xml <- jenkinsGET(x, xpath = "/*/job/name", wrapper = "jobs", ...)
  
  unlist(as_list(xml), use.names = FALSE)
  
}

#' @rdname hasJob
#' @export
hasJob.Jenkins <- function(x, name) {
  
  jenkinsHEAD(x, c("job", name))$status %/% 100 == 2
  
}

#' Execute a HEAD API request for Jenkins
#' @param jenkins See \code{\link{jenkins}}
#' @param path subpath to api
#' @param ... other arguments to \code{\link[httr]{HEAD}}
#' @importFrom httr modify_url HEAD warn_for_status headers status_code
#' @importFrom curl curl_escape
#' @export
jenkinsHEAD <- function(
    jenkins,
    path,
    ...
) {
  
  url <- modify_url(
      jenkins$host,
      path = c(jenkins$contextPath, curl_escape(path), "api", "xml"))
  
  response <- HEAD(url, authenticate(jenkins$user, jenkins$token))
  
  warn_for_status(response)
  
  list(
      headers = headers(response),
      status = status_code(response))
  
}

#' Execute a POST API request for Jenkins
#' @param jenkins See \code{\link{jenkins}}
#' @param path subpath
#' @param ... other arguments to \code{\link[httr]{POST}}
#' @importFrom httr POST content_type_xml headers status_code
#' @importFrom curl curl_escape
#' @export
jenkinsPOST <- function(
    jenkins,
    path,
    xml = NULL,
    ...) {
  
  url <-  modify_url(
      jenkins$host,
      path = c(jenkins$contextPath, curl_escape(path)))
  
  response <- POST(url,
      body = xml,
      authenticate(jenkins$user, jenkins$token),
      crumbHeader(crumbRequest(jenkins)),
      content_type_xml(),
      ...)
  
  stop_for_status(response)
  
  list(
      headers = headers(response),
      status = status_code(response))
  
}

#' Execute a GET API request for Jenkins
#' @param jenkins See \code{\link{jenkins}}
#' @param path subpath to api
#' @param xpath xpath expression
#' @param wrapper xml group name for the returned objects
#' @param ... other arguments to \code{\link[httr]{GET}}
#' @importFrom httr GET accept_xml content modify_url GET
#' @importFrom curl curl_escape
#' @export
jenkinsGET <- function(
    jenkins,
    path = c(),
    tree = NULL,
    xpath = NULL,
    wrapper = "items",
    returnContent = TRUE,
    query = NULL,
    ...) {
  
  url <- modify_url(
      jenkins$host,
      path = c(jenkins$contextPath, curl_escape(path), "api", "xml"),
      query = c(
          if (!is.null(xpath)) list(xpath = xpath, wrapper = wrapper),
          if (!is.null(tree)) list(tree = tree),
          if (!is.null(query)) query
      )
  )
  
  response <- GET(url,
      authenticate(jenkins$user, jenkins$token),
      accept_xml(),
      ...)
  
  stop_for_status(response)
  
  if (returnContent) content(response) else response
  
}

#' List views
#' @description List available views in the jenkins server
#' @template JenkinsOp
#' @param ... further arguments to \code{\link{jenkinsGET}}
#' @return vector of view names
#' @rdname listViews
#' @importFrom xml2 as_list
#' @export 
listViews <- function(jenkins, ...) {
  
  response <- jenkinsGET(jenkins, path = c(), xpath = "/*/view/name",
      wrapper = "views", ...)
  
  unlist(as_list(response), use.names = FALSE)
  
}

#' Check if a view exists
#' @param name name of the view to check for
#' @param ... further arguments to \code{\link{jenkinsHEAD}}
#' @export 
hasView <- function(jenkins, name, ...) {
  
  jenkinsHEAD(jenkins, path = c("view", name), ...)$status %/% 100 == 2
  
}

#' Get Jenkins View
#' @description Obtain an object representing a Jenkins View.
#' @param name name of the view
#' @export
getView <- function(jenkins, name, ...) {
  
  stopifnot(hasView(jenkins, name))
  
  JenkinsView(jenkins, path = c("view", name))
  
}

#' Create a new job
#' @description Creates a new job with given name and XML config.
#' @template JenkinsOp
#' @param name job name
#' @param config job XML specification: \itemize{
#' \item given as a file path
#' \item or an object returned by \code{\link{pipelineConfig}} or \code{\link{multibranchPipelineConfig}}
#' \item or any object of class \code{xml_document} from the \code{xml2} package
#' }
#' @seealso \link[xml2]{read_xml} \link{crumbRequest}
#' @seealso \link{createMultiBranchPipeline} \link{createPipeline}
#' @importFrom httr modify_url authenticate POST content_type_xml
#' @importFrom xml2 read_xml
#' @return jenkins job
#' @export
createJob <- function(jenkins, name, config) {
  
  if (!inherits(config, "xml_document")) {
    stopifnot(file.exists(config))
    config <- read_xml(config)
  }
  
  url <- modify_url(jenkins$host,
      path = c(jenkins$contextPath, "createItem"),
      query = list(name = name))
  
  response <- POST(url, authenticate(jenkins$user, jenkins$token),
      content_type_xml(),
      crumbHeader(crumbRequest(jenkins)),
      body = as.character(config))
  
  stop_for_status(response)
  
  JenkinsJob(jenkins, name, path = c("job", name))
  
}
