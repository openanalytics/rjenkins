
#' Create a new Pipeline
#' @description Creates a new pipeline job with given name and git remote
#' details.
#' @inheritParams createJob
#' @param remote the git remote repository to check out
#' @param credentialsId credentials used to scan branches and check out sources
#' @return jenkins job
#' @seealso \link{createMultiBranchPipeline} \link{createJob}
#' @importFrom xml2 xml_child xml_text read_xml xml_text<-
#' @export
createPipeline <- function(
    conn,
    name,
    remote,
    credentialsId) {
  
  config <- read_xml(
      system.file("extdata", "template", "PipelineProject.xml",
          package = "rjenkins"))
  
  urlNode <- xml_child(config, "definition/scm/userRemoteConfigs/*/url")
  xml_text(urlNode) <- remote
  
  credIdNode <- xml_child(config, "definition/scm/userRemoteConfigs/*/credentialsId")
  xml_text(credIdNode) <- credentialsId
  
  createJob(conn, name, config)
  
}

#' Create a new MultiBranch Pipeline
#' @rdname createMultiBranchPipeline
#' @description Creates a new multibranch pipeline with given name and
#' git branch source. More info can be found \href{https://wiki.jenkins.io/display/JENKINS/Pipeline+Multibranch+Plugin}{here}.
#' @inheritParams createPipeline
#' @return jenkins job
#' @example inst/example/createMultiBranchPipeline.R
#' @seealso \link{createPipeline} \link{createJob}
#' @importFrom xml2 xml_child xml_text read_xml xml_text<- xml_root xml_replace
#' @export
createMultiBranchPipeline <- function(
    conn,
    name,
    branchSource) {
  
  config <- read_xml(
      system.file("extdata", "template", "MultiBranchProject.xml",
          package = "rjenkins"))
  
  branchSourceNode <- xml_child(config, "sources/data/*/source")
  xml_replace(branchSourceNode, xml_root(branchSource$config))
  
  createJob(conn, name, config)
  
}
#' @rdname createMultiBranchPipeline
#' @importFrom openssl md5
#' @export
gitBranchSource <- function(remote, credentialsId) {
  
  config <- read_xml(
      system.file("extdata", "template", "GitSCMSource.xml",
          package = "rjenkins"))
  
  idNode <- xml_child(config, "/id")
  xml_text(idNode) <- paste0("rjenkins-",md5(remote))
  
  urlNode <- xml_child(config, "/remote")
  xml_text(urlNode) <- remote
  
  credIdNode <- xml_child(config, "/credentialsId")
  xml_text(credIdNode) <- credentialsId
  
  structure(list(config = config), class = "BranchSource")
  
}
#' @rdname createMultiBranchPipeline
#' @param api GitHub api URI
#' @param repository GitHub repository name
#' @param owner GitHub repository owner
#' @param sshCheckoutCredentialsId (optional) if not \code{NULL}, the id of the
#' ssh credentials to use for the git checkout.
#' @importFrom openssl md5
#' @export
gitHubBranchSource <- function(
    api,
    owner,
    repository,
    credentialsId,
    sshCredentialsId = NULL
    ) {
  
  config <- read_xml(
      system.file("extdata", "template", "GitHubSCMSource.xml",
          package = "rjenkins"))
  
  x <- xml_child(config, "/id")
  xml_text(x) <- paste0("rjenkins-",md5(paste0(api, owner, repository)))
  
  x <- xml_child(config, "/credentialsId")
  xml_text(x) <- credentialsId
  
  x <- xml_child(config, "/repoOwner")
  xml_text(x) <- owner
  
  x <- xml_child(config, "/repository")
  xml_text(x) <- repository
  
  x <- xml_child(config, "/apiUri")
  xml_text(x) <- api
  
  sshTraitNode <- xml_child(config, "traits/org.jenkinsci.plugins.github__branch__source.SSHCheckoutTrait")
  if (!is.null(sshCredentialsId)) {
    xml_text(sshTraitNode) <- sshCredentialsId
  } else {
    xml_remove(sshTraitNode)
  }
  
  structure(list(config = config), class = "BranchSource")
  
}


#' Create a new job
#' @description Creates a new job with given name and xml config. Refer to
#' \link{createMultiBranchPipeline} and \link{createPipeline} for more
#' user-friendly wrappers.
#' @template jenkinsOp
#' @param name job name
#' @param config job xml specification given either as a file path or an object
#' of class \code{xml_document} from the \code{xml2} package
#' @seealso \link[xml2]{read_xml} \link{crumbRequest}
#' @seealso \link{createMultiBranchPipeline} \link{createPipeline}
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
