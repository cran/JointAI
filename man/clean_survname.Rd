% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpfunctions.R
\name{clean_survname}
\alias{clean_survname}
\title{Convert a survival outcome to a model name}
\usage{
clean_survname(x)
}
\arguments{
\item{x}{a character string or vector of character strings}
}
\description{
A helper function that converts the "name of a survival model"
(the \code{"Surv(time, status)"} specification) into a valid variable name
so that it can be used in the JAGS model syntax.
}
\examples{
clean_survname("Surv(eventtime, event != 'censored')")

}
