% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translation-modules.R
\name{create_response_function}
\alias{create_response_function}
\title{Translate regular DAG to response functions}
\usage{
create_response_function(graph, right.vars, cond.vars)
}
\arguments{
\item{graph}{An \link[igraph]{aaa-igraph-package} object that represents a
directed acyclic graph must contain edge attributes named "leftside" and
"lrconnect" that takes values 0 and 1. Only one edge may have a value 1 for
lrconnect. The shiny app returns a graph in this format.}

\item{right.vars}{Vertices of graph on the right side}

\item{cond.vars}{Vertices of graph on the left side}
}
\value{
A list of functions representing the response functions
}
\description{
Translate regular DAG to response functions
}
