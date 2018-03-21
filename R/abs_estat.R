# funcao 

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
