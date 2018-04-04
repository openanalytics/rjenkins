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
jenkinsAuthEnv <- function(userVar = "JENKINS_USER",
    tokenVar = "JENKINS_TOKEN") {
  
  jenkinsAuth(
      user = Sys.getenv("JENKINS_USER"),
      token = Sys.getenv("JENKINS_TOKEN"))
  
}
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuthKeyringr} use the keyringr package
#' @importFrom keyringr decrypt_gk_pw
jenkinsAuthKeyringr <- function() {
  
  jenkinsAuth(
      user = decrypt_gk_pw("jenkins ci user default"),
      token = decrypt_gk_pw(sprintf("jenkins ci token %s", user)))
  
}
#' @rdname jenkinsCredentials
#' @description \code{jenkinsAuthFile} source credentials from file
#' @param file connection or filename
jenkinsAuthFile <- function(file) {
  
  stop("not yet implemented")
  
}