% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/renyi_entropy.R
\name{renyi_entropy}
\alias{renyi_entropy}
\title{Computes Renyi entropy}
\usage{
renyi_entropy(pDist, alpha, log_type = "nats")
}
\arguments{
\item{pDist}{the probability distribution in question}

\item{alpha}{the parameter that controls the function, support = [0, Inf (alpha!=0)]}

\item{log_type}{defaults to 'nats'}
}
\description{
This function computes the renyi entropy of a distribution with a particular alpha function
}
\examples{
renyi_entropy(prior('random',g),2)
}
\keyword{entropy}
