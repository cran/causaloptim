% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph-utilities.R
\name{check_parents}
\alias{check_parents}
\title{Check for paths from from to to}
\usage{
check_parents(parent_lookup, from, to, prev = NULL)
}
\arguments{
\item{parent_lookup}{A list of vectors}

\item{from}{character}

\item{to}{character}

\item{prev}{Should always be null when first called}
}
\value{
A list of paths or null if no path is found
}
\description{
Check for paths from from to to
}
\examples{
parent_lookup <- list(M = "Am", Y = c("M", "Ay"), A = NULL, Am = "A", Ay = "A")
check_parents(parent_lookup, "A", "Y")
}
