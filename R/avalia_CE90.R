#' Funcao avalia_ce90 -  avalia_ce90 function
#'
#' Calcula  90 porcento do erro circular CE90 (planimetria) para verificacao do PEC
#' @param x entrada de vetor numerico de diferencas planimetricas leste.
#' @param y entrada de vetor numerico de diferencas planimetricas norte.
#' @return  imprime valor de CE 90 porcento da amostra analisada
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' y <- rnorm(30,2,1)
#' avalia_ce90(x,y)
#'

avalia_ce90 <- function(x, y) {
  ce.90 <- round(( avalia_eqm(x,y) * 2.15), 2)
  return (ce.90)
}
