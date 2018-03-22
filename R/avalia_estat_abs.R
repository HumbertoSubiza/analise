#' Funcao avalia_estat_abs -  avalia_estat_abs function
#'
#' Calcula estatisticas basicas de media, desvio padrao, minimo, maximo,
#' EQM e LE90 para dados altimetricos.
#' A entrada e um vetor numerico de discrepancias altimetricas e
#'
#' @param x entrada de vetor numerico de diferencas altimetricas
#' @return tabela com as estasticas calculadas
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' y <- rnorm(30,2,1)
#' avalia_ce90(x,y)
#'

abs_estat <- function(x) {
  x <- abs(x)
  x.med  <- mean(x, na.rm = T)
  x.sde  <- sd  (x, na.rm = T)
  x.min  <- min (x, na.rm = T)
  x.max  <- max (x, na.rm = T)
  x.EQM  <- avalia_eqm(x)
  x.LE90 <- avalia_le90(x)

  #gravar arquivo
  x_estat <- rbind(x.med,
                    x.sde,
                    x.min,
                    x.max,
                    x.EQM,
                    x.LE90)
  return(x_estat)
}
