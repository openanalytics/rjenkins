
#' Jenkins API client for R
#' 
#' @description \code{rjenkins} provides an R wrapper around the jenkins API.
#'
#' @details
#' \enumerate{
#'   \item use \code{\link{jenkinsConnection}} to create a connection
#'   \item use \code{\link{getJob}} to access a specific job
#'   \item use \code{\link{installPackageArtifacts}} to download and install R
#' packages directly from job artifacts
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

