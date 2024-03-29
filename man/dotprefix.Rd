% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pack.R
\name{dotprefix}
\alias{dotprefix}
\title{Extract the column name prefixes from a dataset}
\usage{
dotprefix(names, sep = ".", subset = FALSE)
}
\arguments{
\item{sep}{The separator between prefix and suffix.}

\item{subset}{Exclude column names that do not have both prefix and suffix.}

\item{data}{A data frame or tibble.}
}
\value{
A character vector of column name prefixes.
}
\description{
Extract the column name prefixes from a dataset with a mix of prefix and prefix.suffix column names.
}
