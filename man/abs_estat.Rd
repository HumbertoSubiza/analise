% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avalia_estat_abs.R
\name{abs_estat}
\alias{abs_estat}
\title{Funcao avalia_estat_abs -  avalia_estat_abs function}
\usage{
abs_estat(x)
}
\arguments{
\item{x}{entrada de vetor numerico de diferencas altimetricas}
}
\value{
tabela com as estasticas calculadas
}
\description{
Calcula estatisticas basicas de media, desvio padrao, minimo, maximo,
EQM e LE90 para dados altimetricos.
A entrada e um vetor numerico de discrepancias altimetricas e
}
\examples{
set.seed(123)
x <- rnorm(30,1.5,0.5)
y <- rnorm(30,2,1)
avalia_ce90(x,y)

}
