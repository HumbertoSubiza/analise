#' Pacote que carrega varias funcoes com calculos
#' personalizadas como "resumo", "CE90" e "EQM90" para verificacao do PEC
#' O pacote pode ser instalado no começo do analise com o comando:
#' devtools::install("analise")
#'
#' WHSP -  mar2018

#' Bibliotecas
library(tidyverse)
library(psych)
library(plotrix)
library(lattice)
library(grid)
library(gridExtra)

#' Funcao resumo - resumo function
#'
#' Calcula e imprime numero observ., media, desvpad, mediana, min, max, amplitude e NAs.
#' @param x entrada de vetor numerico, pode ter NAs.
#' @return  Imprime resumo com as estatesticas analisadas
#' @export
#' @examples resumo (DeltaN)
#' resumo(DeltaE)


resumo <- function(x) {
  print(c("Num."        = length(x) - sum(is.na(x)) ,
          "Media"       = mean(x,   na.rm = T),
          "Desvio pad." = sd(x,     na.rm = T),
          "Mediana"     = median(x, na.rm = T),
          "Minimo"      = min(x,    na.rm = T),
          "Maximo"      = max(x,    na.rm = T),
          "Amplitude"   = (abs(min(x, na.rm = T)) +
                           abs(max(x, na.rm=T))),
          "NA"          = sum(is.na(x))),
        digits = 2)
}
#-------------------------------------------------------------------------------

#' Funcao ce90 -  ce90 function
#'
#' cálculo del 90 porcento do erro circular CE90 para verificacao do PEC
#' @param x entrada de vetor numérico de diferencas planimetricas.
#' @param y entrada de vetor numérico de diferencas planimetricas.
#' @return  imprime valor de CE 90 porcento da amostra analisada
#' @export
#' @examples ce90(DeltaE,DeltaN)

# calculo del 90 porcento do erro circular CE90 para verificacao do PEC
ce90    <- function(x,y) {
  rsme <- sqrt((x^2+y^2)/(length(x)-1))
               ce.90 <- round((rsme*2.15), 2)
               return (ce.90)
}

#' Funcao le90 - le90 function
#'
#' calculo del 90 porcento do erro linear,  LE90 para verificacao do PEC
#' @param x entrada de vetor numerico de diferencas de altitudes.
#' @return  imprime valor de LE90 da amostra analisada
#' @export
#' @examples le90(DeltaH)


#'calculo del 90 porcento do erro linear 90 para verificacao do PEC
le90    <- function(x) {
  rsme <- sqrt((x^2)/(length(x)-1))
               le.90 <- round((rsme*1.6449), 2)
               return (le.90)
}

#' Funcao eqm - eqm function
#'
#' calculo del Erro Quadratico Medio para verificacao do PEC
#' @param x entrada de vetor numerico pode ter NAs.
#' @return  imprime valor de EQM da amostra analisada
#' @export
#' @examples eqm(DeltaH)
#' eqm()

# Calculo do EQM para verificacao do PEC
eqm <- function (x){
  ord.x <- sort(x)
  x.2   <- (ord.x^2)
  n     <- (length(x.2) - 1)
  eqm   <- round(sqrt((sum(x.2) / n)), 2)
  return (eqm)
  }
#-------------------------------------------------------------------------------
# Modificações:


