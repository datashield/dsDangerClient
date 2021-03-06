% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.DANGERlistcode.R
\name{ds.DANGERlistcode}
\alias{ds.DANGERlistcode}
\title{list the code for a function}
\usage{
ds.DANGERlistcode(package.name, function.name, datasources = NULL)
}
\arguments{
\item{package.name}{a vector of character containing a package name}

\item{function.name}{a vector of character containing a function.name}

\item{datasources}{specifies the particular 'connection object(s)' to use.}
}
\value{
a vector of character strings giving the code listing from studies.
}
\description{
list the code for a function deployed on the serverside.
}
\details{
list the code for a function deployed on the serverside.
The function is specified by package name and functions name.
}
\author{
Stuart Wheater, DataSHIELD Team, 2020
}
