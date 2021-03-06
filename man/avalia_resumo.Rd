% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/avalia_resumo.R
\name{avalia_resumo}
\alias{avalia_resumo}
\title{Funcao avalia_resumo - avalia_resumo function}
\usage{
avalia_resumo(x)
}
\arguments{
\item{x}{entrada de vetor numerico, pode ter NAs.}
}
\value{
Imprime resumo com as estatisticas analisadas
}
\description{
Calcula e imprime numero de observacoes, media, desvpad, mediana, min, max, amplitude e NAs.
}
\examples{
set.seed(123)
x <- c(1.21,0.95,1.55,2.11,0.66, NA, 1.74,0.51,NA,1.85)
avalia_resumo(x)

}
