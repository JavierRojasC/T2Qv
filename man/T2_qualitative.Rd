% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/T2_qualitative.R
\name{T2_qualitative}
\alias{T2_qualitative}
\title{Multivariate control chart for qualitative variables}
\usage{
T2_qualitative(base, IndK, dim, interactive = FALSE, alpha = 0.0027)
}
\arguments{
\item{base}{Data set}

\item{IndK}{Character with the name of the column that specifies the partition of the data set in k tables.}

\item{dim}{Dimension taken for reduction. Initial dimension - 1 is recommended.}

\item{interactive}{If it is TRUE, the graph will be shown interactively. If FALSE, the graph is displayed flat. FALSE is the default.}

\item{alpha}{Type I error, it is recommended to reach this value by using the ARL.}
}
\value{
A control chart made with the T2 hotelling statistic, applied to detect anomalies in any of the K tables obtained with the specification of \code{IndK}. The control limit of the graph is obtained from the number of dimensions \code{dim} and the type I error \code{alpha}.
}
\description{
Multivariate control chart T2 Hotelling applicable for qualitative variables.
}
\examples{
data(Datak10Contaminated)
T2_qualitative(Datak10Contaminated,"GroupLetter",9, FALSE,0.0027)
}
