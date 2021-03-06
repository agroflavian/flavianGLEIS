% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myFUN_smoother.R
\name{myFUN_smoother}
\alias{myFUN_smoother}
\title{filter nfunction --> GLEIS}
\usage{
myFUN_smoother(geno, smoothFactor, column)
}
\arguments{
\item{geno}{genotype}

\item{smoothFactor}{number -> higher equals smoother}

\item{column}{which column should be smoothed --> e.g. "ler"}
}
\value{
returns a dataframe with the smoothed values
}
\description{
filtering data to drought treatment relevant
}
\examples{
myFUN_smoother()
}
\keyword{smoother}
