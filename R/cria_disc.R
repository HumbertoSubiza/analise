# Função cria_disc
# 


cria_disc <- function(dados.orig) {
     library(magrittr)
     attach(dados.orig)
    dados <- dados.orig  %>% 
    dplyr::arrange(Camp) %>% 
    dplyr::mutate(DeltaE = round((E_img - E), 2), 
                  DeltaN = round((N_img - N), 2), 
                  DeltaP = round(sqrt(DeltaE^2 + DeltaN^2),2),
                  DeltaP_log = round(log(DeltaP),2),
                 DeltaH     = round(H_MDT-H,2))
    detach(dados.orig)
  return(dados)
}

cria_disc(dados.orig)
