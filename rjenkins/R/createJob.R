
#' Create a new MultiBranch Pipeline
#' @description Creates a new multibranch pipeline with given name and
#' git remote details.
#' @param name multibranch pipeline name
#' @param remote the git remote repository to check out
#' @param credentialsId credentials used to scan branches and check out sources
#' @return jenkins job
#' @importFrom xml2 xml_child xml_text read_xml xml_text<-
#' @export
createMultiBranchPipeline <- function(
    conn,
    name,
    remote,
    credentialsId) {
  
  config <- read_xml(
      system.file("extdata", "template", "MultiBranchProject.xml", package = "rjenkins"))
  
  urlNode <- xml_child(config, "sources/data/*/source/remote")
  xml_text(urlNode) <- remote
  
  credIdNode <- xml_child(config, "sources/data/*/source/credentialsId")
  xml_text(credIdNode) <- credentialsId
  
  createJob(conn, name, config)
  
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
