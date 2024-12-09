% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_tirage_strat_m2.R
\name{tirage_strat_m2}
\alias{tirage_strat_m2}
\title{tirage_strat_m2}
\usage{
tirage_strat_m2(input_data, taille_strat, var_strat)
}
\arguments{
\item{input_data}{un dataframe}

\item{taille_strat}{un nombre de strates}

\item{var_strat}{une variable de stratification}
}
\value{
a dataframe, result of a stratified sampling without replacement
}
\description{
A fct function that does a stratified random sampling without replacement
}
\examples{
tirage_strat_m2(grandile, 400, "DIPL")
}
