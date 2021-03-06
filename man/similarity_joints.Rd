% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarity_joints.R
\name{similarity_joints}
\alias{similarity_joints}
\title{How similar are two joint probability distributions}
\usage{
similarity_joints(joint, joint2, measure = "symmetrised_KL")
}
\arguments{
\item{joint}{The first probability distribution}

\item{joint2}{The second probability distribution}

\item{measure}{'symmetrised_KL' or 'squared_error', defaults to 'symmetrised_KL'}
}
\description{
How similar are two joint probability distributions.
}
\examples{
similarity_joints(c(.2,.8), c(.4,.6))
}
\keyword{similarity}
