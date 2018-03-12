# Creating packamon jobs on the OA jenkins
# TODO: should probably move this to a seperate packae in order to turn rjenkins
# into a more generally applicable package similiar to jenkinsapi for python
# but could be even more general -> 'oaOps?'
#
# Author: Daan Seynaeve
###############################################################################

#' Create a Packamon job for the given git repository
#' @template jenkinsOp
#' @param remote either a remote name or a repository url. If a remote name
#' is supplied, the repository url will be looked up from the git config.
#' @param branch branch name
#' @param description job description
#' @return job name
#' @importFrom xml2 xml_child xml_text read_xml
#' @export
createPackamonJob <- function(conn, remote = "origin", branch = "master",
    description = "Automatically created. Managed by packamon.") {
  
  # construct job name
  
  if (grepl("(https?)|(ssh)|(git)://.*", remote)) {
    url <- remote
  } else {
    url <- system2("git", c("remote", "get-url", "origin"), stdout = TRUE)
  }
  
  job <- sprintf("%s-%s", gsub(".git$", "", basename(url)), branch)
  
  if (hasJob(conn, job)) {
    stop("job already exists: ", job)
  }
  
  # fill out template

  config <- read_xml(system.file("extdata", "packamon-job-config.xml",
      package = "rjenkins"))

  ciGitUsername <- "jenkins"
  ciGitCredentialsId <- "eae5688d-c858-4757-a17f-65b68ca771da"

  descriptionNode <- xml_child(config, "description")
  xml_text(descriptionNode) <- description
  
  urlNode <- xml_child(config, "scm/userRemoteConfigs/*/url")
  xml_text(urlNode) <- modify_url(url, username = ciGitUsername)
  
  credIdNode <- xml_child(config, "scm/userRemoteConfigs/*/credentialsId")
  xml_text(credIdNode) <- ciGitCredentialsId
  
  branchNode <- xml_child(config, "scm/branches/*/name")
  xml_text(branchNode) <- sprintf("*/%s", branch)
  
  # post job
  
  createJob(conn, job, config)
  
  return(job)
  
}