# Função avalia_cria
#
#' Funcao avalia_cria -  avalia_cria
#'
#' Calcula novas variaveis de discrepancias planimetricas e altimetricas
#' A entrada e um dataframe que deve conter coordenadas UTM de imagem e de campo
#' (E_img, N_img, H_MDT, E, N e H). As discrepancias planimetricas sao DeltaE,
#'  DeltaN, DeltaP e DeltaP_log. A discrepancia altimetrica e DeltaH.
#'  A entrada e o dataframe "dados.orig"
#'
#' @param x entrada de vetor numerico de diferencas planimetricas leste.
#' @return  A saida e um dataframe com as novas variaveis criadas
#' @export
#' @examples
#' if(requireNamespace("dplyr"))
#' if(requireNamespace("magrittr"))
#' E <- c(62304.57,58154.59,63680.38,59388.15,64564.44)
#' N <- c(44204.79,51896.75,57967.56,36321.42,50307.35)
#' H <- c(3.54,1.69,2.05,19.35,11.27)
#'
#'E_img <- c(62304.29,58154.59,63680.46,59387.97,64564.31)
#'N_img <- c(44204.79,51896.75,57967.65,36321.48,50307.28)
#'H_MDT <- c(4.0,2.0,2.0,20.0,12.0)
#' x <- as.data.frame(cbind(E, N, H, E_img, N_img, H_MDT))
#' avalia_cria(x)
#'

avalia_cria <- function(x) {
    library("dplyr")
    library("magrittr")
    attach(x)
    dados <- x    %>%
    mutate(DeltaE = round((E_img - E), 2),
           DeltaN = round((N_img - N), 2),
                  DeltaP = round(sqrt(DeltaE^2 + DeltaN^2),2),
                  DeltaP_log = round(log(DeltaP),2),
                  DeltaH     = round(H_MDT-H,2))
    detach(x)
  return(dados)
}

