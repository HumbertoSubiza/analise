#' Funcao avalia_le90 - avalia_le90 function
#'
#' Calcula 90 porcento do erro linear, le90 em altimetria para verificacao do PEC
#' @param x entrada de vetor numerico de diferencas de altitudes.
#' @return  imprime valor de le90 da amostra analisada
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' avalia_le90(x)
#'

avalia_le90    <- function(x) {
  le.90 <- round(( avalia_eqm(x) * 1.6449), 2)
  return (le.90)
}

#' Modificacoes:
#' em 14 de marco 2018.  adaptacao das funcoes
