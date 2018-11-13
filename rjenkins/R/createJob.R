
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
