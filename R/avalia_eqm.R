#' Funcao avalia_eqm - avalia_eqm function
#'
#' calcula Erro Quadratico Medio para posterior verificacao do PEC
#' @param x entrada de vetor numerico pode ter NAs.
#' @param y entrada de vetor numerico pode ter NAs. Valor padrão = 0
#' @return  imprime valor de EQM da amostra analisada
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' y <- rnorm(30,2,1)
#' avalia_eqm(x)
#' avalia_eqm(x,y)

avalia_eqm <- function(x, y=0){
  x    <- x[complete.cases(x)]
  y    <- y[complete.cases(y)]
  remq <- sqrt(sum(x^2 + y^2) / (length(x) - 1))
  return(remq)
}
