#' Funcao avalia_resumo - avalia_resumo function
#'
#' Calcula e imprime numero de observacoes, media, desvpad, mediana, min, max, amplitude e NAs.
#' @param x entrada de vetor numerico, pode ter NAs.
#' @return  Imprime resumo com as estatisticas analisadas
#' @export
#' @examples set.seed(123)
#' x <- c(1.21,0.95,1.55,2.11,0.66, NA, 1.74,0.51,NA,1.85)
#' avalia_resumo(x)
#'

avalia_resumo <- function(x) {
  sum.na <- sum(is.na(x))
  print(c("Num."        = length(x) - sum.na ,
          "Media"       = mean(x,   na.rm = T),
          "Desvio pad." = sd(x,     na.rm = T),
          "Mediana"     = median(x, na.rm = T),
          "Minimo"      = min(x,    na.rm = T),
          "Maximo"      = max(x,    na.rm = T),
          "Amplitude"   = (abs(min(x, na.rm = T)) +
                           abs(max(x, na.rm=T))),
          "NAs"         = sum.na),
        digits = 2)
}

#' Funcao avalia_ce90 -  avalia_ce90 function
#'
#' Calcula  90 porcento do erro circular CE90 em planimetria para verificacao do PEC
#' @param x entrada de vetor numerico de diferencas planimetricas leste.
#' @param y entrada de vetor numerico de diferencas planimetricas norte.
#' @return  imprime valor de CE 90 porcento da amostra analisada
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' y <- rnorm(30,2,1)
#' avalia_ce90(x,y)
#'

avalia_ce90    <- function(x, y) {
  rsme <- sum(sqrt((x^2 + y^2) / (length(x) - 1)))
               ce.90 <- round((rsme * 2.15), 2)
               return (ce.90)
}

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
  rsme <- sqrt((x^2) / (length(x) - 1))
               le.90 <- round((rsme * 1.6449), 2)
               return (le.90)
}

#' Funcao avalia_eqm - avalia_eqm function
#'
#' calcula Erro Quadratico Medio para verificacao do PEC
#' @param x entrada de vetor numerico pode ter NAs.
#' @return  imprime valor de EQM da amostra analisada
#' @export
#' @examples set.seed(123)
#' x <- rnorm(30,1.5,0.5)
#' avalia_eqm(x)

avalia_eqm <- function (x){
  ord.x <- sort(x)
  x.2   <- (ord.x^2)
  n     <- (length(x.2) - 1)
  eqm   <- round(sqrt((sum(x.2) / n)), 2)
  return (eqm)
}

#' Modificacoes:


