% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_cvs_reg_desais.R
\name{cvs_reg_desais}
\alias{cvs_reg_desais}
\title{cvs_reg_desais}
\usage{
cvs_reg_desais(x)
}
\arguments{
\item{x}{une série temporelle}
}
\value{
une série temporelle
}
\description{
Fonction qui désaisonnalise par regression linéaire
}
\examples{
x <- airpass
y <- log(airpass)  
ycvs <- cvs_reg_desais(y)
exp(ycvs)
}
