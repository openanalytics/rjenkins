% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jenkins.R
\name{jenkinsGET}
\alias{jenkinsGET}
\title{Execute a GET API request for Jenkins}
\usage{
jenkinsGET(
  jenkins,
  path = c(),
  tree = NULL,
  xpath = NULL,
  wrapper = "items",
  returnContent = TRUE,
  query = NULL,
  ...
)
}
\arguments{
\item{jenkins}{See \code{\link{jenkins}}}

\item{path}{subpath to api}

\item{xpath}{xpath expression}

\item{wrapper}{xml group name for the returned objects}

\item{...}{other arguments to \code{\link[httr]{GET}}}
}
\description{
Execute a GET API request for Jenkins
}
