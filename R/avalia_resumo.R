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
