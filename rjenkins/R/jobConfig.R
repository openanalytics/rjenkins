
tag <- function(name, ...) {
  args <- list(...)
  setNames(
      list(if (is.null(names(args))) {
                structure(list(...))
              } else {
                named <- names(args) != ""
                do.call(structure, c(list(do.call(list, args[!named])), args[named]))
              }),
      name)
}

#' Create a new Pipeline
#' @description Creates configuration for a pipeline job.
#' @param pipeline inline pipeline; see \code{\link{jenkinsPipeline}}
#' @param remote the git remote repository to check out
#' @param credentialsId credentials used to scan branches and check out sources
#' @details If an inline \code{pipeline} is supplied, the \code{remote}
#' argument will be ignored/
#' @return XML document
#' @seealso \link{createMultiBranchPipeline} \link{createJob}
#' @importFrom xml2 xml_child xml_text read_xml xml_text<- as_xml_document
#' @export
pipelineConfig <- function(
    remote,
    credentialsId = NULL,
    pipeline = NULL) {
  
  if (!is.null(pipeline)) {
    
    config <- as_xml_document(
        tag("flow-definition", plugin = "workflow-job@2.32",
            tag("actions"), 
            tag("description"),
            tag("keepDependencies", "false"),
            tag("definition", .class="org.jenkinsci.plugins.workflow.cps.CpsFlowDefinition", plugin="workflow-cps@2.70",
                tag("script", pipeline)))
    )
    
  } else {
    
    if (is.null(credentialsId))
      stop("Please specify credentialsId")
    
    config <- read_xml(
        system.file("extdata", "template", "PipelineProject.xml",
            package = "rjenkins"))
    
    urlNode <- xml_child(config, "definition/scm/userRemoteConfigs/*/url")
    xml_text(urlNode) <- remote
    
    credIdNode <- xml_child(config, "definition/scm/userRemoteConfigs/*/credentialsId")
    xml_text(credIdNode) <- credentialsId
    
  }
  
  config
  
}

#' Create a new MultiBranch Pipeline
#' @rdname createMultiBranchPipeline
#' @description Creates a new multibranch pipeline with given name and
#' git branch source. More info can be found \href{https://wiki.jenkins.io/display/JENKINS/Pipeline+Multibranch+Plugin}{here}.
#' @param credentialsId credentials used to scan branches and check out sources
#' @return XML document
#' @example inst/example/createMultiBranchPipeline.R
#' @seealso \link{createPipeline} \link{createJob}
#' @importFrom xml2 xml_child xml_text read_xml xml_text<- xml_root xml_replace
#' @export
multibranchPipelineConfig <- function(branchSource) {
  
  config <- read_xml(
      system.file("extdata", "template", "MultiBranchProject.xml",
          package = "rjenkins"))
  
  branchSourceNode <- xml_child(config, "sources/data/*/source")
  xml_replace(branchSourceNode, xml_root(branchSource$config))
  
  config
  
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
