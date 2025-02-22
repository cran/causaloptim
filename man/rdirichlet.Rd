% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{rdirichlet}
\alias{rdirichlet}
\title{Sample from a Dirichlet distribution}
\usage{
rdirichlet(k, alpha = 1)
}
\arguments{
\item{k}{Length of the vector}

\item{alpha}{Concentration parameters}
}
\value{
a numeric vector
}
\description{
Generate a random vector from the k-dimensional symmetric Dirichlet distribution with concentration parameter alpha
}
\examples{
qvals <- rdirichlet(16, 1)
sum(qvals)
}
