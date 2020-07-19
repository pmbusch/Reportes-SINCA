### Proyecto SINCA
## Lee mediante una funcion recursiva todos los archivos de concentracion de una carpeta
## Deja una dataframe para su posterior analisis
## PBH Jul 2020
## Ultima atualizacion: PBH Jul 2020

# Navegacion directorio ---------
library(tools)

## Funcion recursiva para cargar todos los datos
f_loadAllData <- function(archivo, pollutant, df){
  if (file.exists(archivo)){
    extension <- file_ext(archivo)
    # Es carpeta
    if(extension==""){
      for (archs in list.files(archivo)){
        # cat("recursion ", archs, " \n")
        archivo_new <- paste(archivo, archs, sep="/")
        df <- f_loadAllData(archivo_new, pollutant, df)
      }
      return(df)
    }
    # Archivo concentraciones
    else if(extension=="csv"){
      # Pollutant de interes
      if (str_detect(archivo,pollutant)){
        # Añado al dataframe
        cat("Lectura ", archivo," \n")
        return(rbind(df, f_readFileConc(archivo)))
      }
      else{
        return(rbind(df, data.frame()))
      }
    }
    else {
      return(rbind(df, data.frame()))
    }
  }
  cat("Archivo no existe ", archivo, " \n")
}

## EoF