% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connection.R
\name{getBuildQueue}
\alias{getBuildQueue}
\title{Get the Build Queue}
\usage{
getBuildQueue(conn)
}
\arguments{
\item{conn}{connection to a jenkins instance. Result of \code{\link{jenkinsConnection}}}
}
\value{
Queued job builds summary as a \code{data.frame} or \code{NULL} if
the queue is empty.
}
\description{
Get a summary of queued job builds
}