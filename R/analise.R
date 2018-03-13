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
#' @param x entrada de vetor numérico pode ter NAs.
#' @return  imprime valor de CE 90 porcento da amostra analisada
#' @export
#' @examples ce90(DeltaN)
#' ce90(DeltaH)

# calculo del 90 porcento do erro circular CE90 para verificacao do PEC
ce90    <- function(x) {
  ord.x <- sort(x)
  n     <- ceiling((length(na.omit(ord.x)))*0.9)
  ce.90 <- round(ord.x[n], 2)
  return (ce.90)
  }

#' Funcao le90 - le90 function
#'
#' calculo del 90\%  do erro,  LE90 para verificacao do PEC
#' @param x entrada de vetor numerico pode ter NAs.
#' @return  imprime valor de LE90 da amostra analisada
#' @export
#' @examples le90(DeltaN)
#' le90()

# LE90
le90  <- function(x){
  ord.r <- sort(x)
  n <- ceiling((length(na.omit(ord.r)))*0.9)
  le.90 <- round(ord.r[n],2)
  return (le.90)}

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


