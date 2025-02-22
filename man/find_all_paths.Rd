% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_all_paths}
\alias{find_all_paths}
\title{Find all paths in a causal model}
\usage{
find_all_paths(respvars, from, to)
}
\arguments{
\item{respvars}{A set of response functions as created by \link{create_response_function}}

\item{from}{A character string indicating the start of the path}

\item{to}{A character string indicating the end of the path}
}
\value{
A list with all the paths or a list with NULL if there are none
}
\description{
Given a set of response functions, find all directed paths from from to to
}
\examples{
 b <- initialize_graph(igraph::graph_from_literal(X -+ Z, Z -+ Y, X -+ Y, Ur -+ Z, Ur -+ Y))
 medmod <- create_response_function(b)
 find_all_paths(medmod, "X", "Y")
 igraph::all_simple_paths(b, "X", "Y", mode = "out")

}
