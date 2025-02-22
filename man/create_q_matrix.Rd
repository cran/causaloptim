% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/translation-modules.R
\name{create_q_matrix}
\alias{create_q_matrix}
\title{Translate response functions into matrix of counterfactuals}
\usage{
create_q_matrix(respvars, right.vars, cond.vars, constraints)
}
\arguments{
\item{respvars}{A list of functions as returned by \link{create_response_function}}

\item{right.vars}{Vertices of graph on the right side}

\item{cond.vars}{Vertices of graph on the left side}

\item{constraints}{A vector of character strings that represent the constraints}
}
\value{
A list of 3 data frames of counterfactuals and their associated labels
}
\description{
Translate response functions into matrix of counterfactuals
}
\examples{
graphres <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y))
constraints <- "X(Z = 1) >= X(Z = 0)"
cond.vars <- V(graphres)[V(graphres)$leftside == 1 & names(V(graphres)) != "Ul"]
right.vars <- V(graphres)[V(graphres)$leftside == 0 & names(V(graphres)) != "Ur"] 
respvars <- create_response_function(graphres)
create_q_matrix(respvars, right.vars, cond.vars, constraints)
}
