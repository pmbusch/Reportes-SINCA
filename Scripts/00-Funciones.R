## Proyecto SINCA
## Funciones
## PBH Octubre 2018

cuartilHora <- function(x){
  retVal = 'NA'
  if (x<7){
    retVal='Hora: 00-06'
  }
  else if (x<13){
    retVal='Hora: 06-12'
  }
  else if (x<19){
    retVal='Hora: 12-18'
  }
  else{retVal='Hora: 18-24'}
  return(retVal)
}

# Funcion para aplicar split a un string y obtener el valor n del arreglo (comando recurrente PBH)
f_split_n <- function(X,Sep,N){
  X %>% str_split(Sep) %>% sapply(function(x) x[N])
}

# https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 

f_remover_acentos <- function(x){
  x %>% 
    str_replace_all("á","a") %>% 
    str_replace_all("é","e") %>% 
    str_replace_all("í","i") %>% 
    str_replace_all("ó","o") %>% 
    str_replace_all("ú","u") %>% 
    str_replace_all("ñ","n") %>% 
    str_replace_all("Ñ","N")
}
