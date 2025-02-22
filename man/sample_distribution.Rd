% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/causal-model.R
\name{sample_distribution}
\alias{sample_distribution}
\title{Sample a distribution of observable probabilities that satisfy the causal model}
\usage{
sample_distribution(
  obj,
  simplex_sampler = function(k) {
     rdirichlet(k, alpha = 1)
 }
)
}
\arguments{
\item{obj}{An object of class "causalmodel"}

\item{simplex_sampler}{A function to generate a random sample from the simplex in k dimensions, where k is the number of variables (q parameters, obj$data$variables). By default this is uniform (symmetric dirichlet with parameter 1).}
}
\value{
A vector of observable probabilities that satisfy the causal model
}
\description{
Sample a distribution of observable probabilities that satisfy the causal model
}
\examples{

graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
prob.form <- list(out = c("X", "Y"), cond = "Z")

iv_model <- create_causalmodel(graph, prob.form = prob.form)
sample_distribution(iv_model)

}
