% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process-optimizer.R
\name{optimize_effect_2}
\alias{optimize_effect_2}
\alias{optimize_effect}
\title{Run the optimizer to obtain symbolic bounds}
\usage{
optimize_effect_2(obj)
}
\arguments{
\item{obj}{Object as returned by \link{analyze_graph} or \link{create_linearcausalproblem}}
}
\value{
An object of class "balkebound" that is a list that contains the bounds and logs as character strings, and a function to compute the bounds
}
\description{
Given an object with the linear programming problem set up, compute the bounds
using rcdd. Bounds are returned as text but can
be converted to R functions using \link{interpret_bounds}, or latex code using
\link{latex_bounds}.
}
\examples{
b <- initialize_graph(graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
optimize_effect_2(obj)
}
