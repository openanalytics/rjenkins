# All things authentication
# 
# Author: Daan Seynaeve
###############################################################################

#' Jenkins Authentication Credentials
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuth} provide credentials directly as
#' function arguments.
#' @param user user to connect with
#' @param token token to use for authentication
#' @return credentials; object of class \code{jenkinsCredentials}
#' @export
jenkinsAuth <- function(user, token) {
  
  structure(list(user = user, token = token), class = "jenkinsCredentials")
  
}
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuthEnv} source credentials from environment
#' variables
#' @param userVar environment variable with user
#' @param tokenVar environment variable with token
#' @export
jenkinsAuthEnv <- function(userVar = "JENKINS_USER",
    tokenVar = "JENKINS_TOKEN") {
  
  jenkinsAuth(
      user = Sys.getenv("JENKINS_USER"),
      token = Sys.getenv("JENKINS_TOKEN"))
  
}
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuthKeyringr} use the \code{keyringr} package to lookup
#' an user password.
#' @param host jenkins host identifier
#' @export
jenkinsAuthKeyringr <- function(user, host = "localhost") {
  
  jenkinsAuth(
      user = user,
      token = keyringr::decrypt_gk_pw(sprintf("jenkins %s user %s", host, user)))
  
}
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuthFile} source credentials from file
#' @param credentialsFile connection or filename
#' @export
jenkinsAuthFile <- function(credentialsFile = ".jenkins-credentials") {
  
  # TODO: check if credentials file has proper permissions
  
  if (!inherits(file, "connection")) {
    if (!file.exists(credentialsFile)) {
      stop("credentials file not found")
    }
    fc <- file(credentialsFile) 
  }
  
  res <- sapply(readLines(fc), strsplit, split = "=")
  
  jenkinsAuth(
      user = res[[1]][2],
      token = res[[2]][2])
  
}