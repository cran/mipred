% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glm.mipred.R
\name{.impute}
\alias{.impute}
\title{General imputation routine for mipred}
\usage{
.impute(combdat, miop, nimp, seed)
}
\arguments{
\item{combdat}{Dataset to be imputed}

\item{miop}{Mice options list}

\item{nimp}{Number of imputations}

\item{seed}{Single numerical seed value}
}
\value{
A 'mice' object containing imputations
}
\description{
General imputation routine for mipred
}
\note{
This is an internal 'mipred' function and not intended to be called directly
}
