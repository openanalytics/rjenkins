# Jenkins R package installation tools
# 
# Author: Daan Seynaeve
###############################################################################


#' Extract Package Archives Names from Build Artifacts
#' @description Filter a given vector of artifact names to only retain the
#' desired package archive names.
#' @param artifacts \code{character()} vector of artifact name(s)
#' @param latestOnly retain only the latest version of multiple versions of the
#' same package exist.
#' @param archivePattern the regex used for filtering and extracting package
#' information
#' @return \code{data.frame} with valid package archive names and their
#' corresponding package name and version number.
#' @examples
#' artifacts <- c("eightFive_3.4-3.tar.gz", "xgsldgks03", "test_0.3-2.tar.gz",
#'     "test_1.3-2.tar.gz", "eightFive_3.4-2.tar.gz")
#' show(extractPackageArchives(artifacts))
#' ##                  archive      name version
#' ## 1 eightFive_3.4-3.tar.gz eightFive   3.4-3
#' ## 3      test_1.3-2.tar.gz      test   1.3-2
#' @seealso \link{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Creating-R-packages}
#' @export
extractPackageArchives <- function(artifacts, latestOnly = TRUE,
    archivePattern = "^([a-zA-Z0-9.]{2,})_([0-9]+\\.[0-9]+[.-][0-9]+)\\.tar\\.gz$") {
  
  # should contain only (ASCII) letters, numbers and dot, have at least two
  # characters and start with a letter and not end in a dot
  # https://www.regexpal.com/?fam=102797
  
  pkgs <- data.frame(stringsAsFactors = FALSE,
      archive = artifacts[grepl(archivePattern, artifacts)])
  
  pkgs$name <- gsub(archivePattern, "\\1", pkgs$archive)
  pkgs$version <- gsub(archivePattern, "\\2", pkgs$archive)
  
  if (latestOnly) {
    for (name in unique(pkgs$name)) {
      version <- max(package_version(pkgs[pkgs$name == name, "version"]))
      pkgs <- pkgs[pkgs$name != name | pkgs$version == version,]
    }
  }
  
  pkgs
  
}

#' Install a Package from Jenkins
#' @description Install build artifacts exported from a jenkins job as R packages.
#' @template jenkinsJobOp
#' @param artifacts \code{character()} vector or artifact name(s) that should be
#' installed as an R package. By default this is the complete list of artifacts
#' exported from the latest succesful build of the corresponding job.
#' @param build build number or a build ref. See \code{\link{JENKINS_BUILD_REFS}}
#' @param latestOnly only install the latest version when multiple versions
#' of the same package are given in \code{pkg}; \code{TRUE} by default
#' @param tmpDir temporary directory to store downloaded package archive
#' @details only artifacts that have a name that is interpretable under the
#' default package archive naming convention will be installed.
#' @return nothing
#' @seealso \code{\link{extractPackageArchives}} for filtering package names
#' from a list of build artifacts
#' @importFrom httr modify_url GET authenticate write_disk
#' @importFrom utils install.packages
#' @export
installPackageArtifact <- function(
    job,
    artifacts = listArtifacts(job, "lastSuccessfulBuild"),
    build = JENKINS_BUILD_REFS,
    latestOnly = TRUE,
    tmpDir = normalizePath(file.path("~", "Downloads"))) {
  
  if (!is.numeric(build)) {
    build <- match.arg(build, JENKINS_BUILD_REFS)
  }
  
  for (archive in extractPackageArchives(artifacts, latestOnly = latestOnly)$archive) {
    
    url <- modify_url(job$conn$host,
        path = c("job", job$name, build, "artifact", archive))
    
    response <- GET(url, write_disk(file.path(tmpDir, archive), overwrite = TRUE),
        authenticate(job$conn$user, job$conn$token))
    
    install.packages(pkgs = file.path(tmpDir, archive), repos = NULL, type = "source")
    
  }
  
  invisible()
  
}