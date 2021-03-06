% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/choose_int.R
\name{choose_int}
\alias{choose_int}
\title{Get intervention values}
\usage{
choose_int(g, pG, compute_value = 1)
}
\arguments{
\item{g}{matrix of hypothesis graphs, one line is one graph to be written by row to a matrix.}

\item{compute_value}{defaults to 0=no.  If 1 a value matrix C must be provided with a value for each judgement i given each true network j
Additional arguments can then be passed to this objective function}

\item{pDist}{is the prior distribution over these graphs.}
}
\description{
Computes the most useful intervention according to information, probability and utility gain.
Requires a dataframe called li in the global workspace that encodes the likelihoods of any outcome of any intervention created with likelihood()
Requires a dataframe called o which incodes the outcome space (see example)
Requires a matrix of interventions called ints, 1 is on, 0 is free, -1 is off.
}
\examples{
ints<-as.matrix(expand.grid(rep(list(0:2), 3)))
ints[ints==2]<--1
o<-expand.grid(rep(list(0:1 ), sqrt(dim(g)[2])))
dist<-prior('flat',g)
li<-likelihood(g,.1,.8)
out<-choose_int(g, dist, compute_value=0)
}
\keyword{expected}
\keyword{value}
