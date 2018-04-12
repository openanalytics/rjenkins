
#' Jenkins API client for R
#' 
#' @description \code{rjenkins} provides an R wrapper around the jenkins API.
#'
#' @details
#' \enumerate{
#'   \item use \code{\link{jenkinsConnection}} to create a connection
#'   \item use \code{\link{getJob}} to access a specific job
#' }
#'  
#' @docType package
#' @name rjenkins

NULL

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("rjenkins, version ", packageVersion("rjenkins"), ", run ?rjenkins to get started")
  }
}

