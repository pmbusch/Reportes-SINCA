## Proyecto SINCA
## Descarga de datos de la web del SINCA
## Funcion para descargar datos del SINCA
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020


# Funcion para descargar y trabajar datos de concentracion
# Recibe el URL de la descarga y nombre del archivo
f_scrap_sinca <- function(url, file_name = "Descarga.csv", remover_file=T){
  
  # Descarga y Lectura de archivo  --------------
  destino <- paste('Data',"Temp",file_name,sep='/')
  download.file(url,destfile = destino)
  
  # csv2 uses sep=; and decimal mark=,
  df_conc <- read_delim(destino, delim=";", na = c("NA"), col_types="ccdddd",
                        locale = locale(decimal_mark = ","))
  spec(df_conc)
  df_conc$X6 <- NULL # columna adicional
  df_conc$...6 <- NULL
  
  # If para asegurar que datos meteorologicos pasen igual como validados todos
  if (ncol(df_conc)<5){
    df_conc$X5 <- NA %>% as.numeric()
  }
  
  colnames(df_conc) <- c('fecha','hora','validados','preliminares','noValidados')
  
  if (remover_file){
    file.remove(destino)
  }
  
  ## Aplanar Datos validados, preliminares y no validados -------------
  # Jerarquia de datos: validados > preliminares > noValidados
  df_conc <- df_conc %>% mutate(tipo_dato = case_when(
    !is.na(validados) ~ "validados",
    !is.na(preliminares) ~ "preliminares",
    !is.na(noValidados) ~ "noValidados",
    TRUE ~ "NA"))
  
  # df_conc %>% group_by(tipo_dato) %>% summarise(count=n())
  
  
  # remuevo filas duplicadas (por estado de Dato)
  # df <- df %>% arrange(desc(tipo_dato)) %>% distinct(site,date,.keep_all = T)
  
  # remove valores NA 
  df_conc <- df_conc %>% filter(tipo_dato!="NA") 
  
  # Asigno el valor y borro columnas
  df_conc <- df_conc %>% 
    mutate(valor = case_when(
      tipo_dato=="validados" ~ validados,
      tipo_dato=="preliminares" ~ preliminares,
      tipo_dato=="noValidados" ~ noValidados),
    validados = NULL,
    preliminares = NULL,
    noValidados = NULL)
  
  # Crear date ------------
  df_conc<- df_conc %>% 
    mutate(date=paste(str_sub(fecha,5,6),
                      str_sub(fecha,3,4),
                      str_sub(fecha,1,2),sep='-'),
           horario=paste(str_sub(hora,1,2),'00',sep=':'),
           date=paste(date,horario,sep=' '))
  
  # Formato a date
  df_conc <- df_conc %>% 
    mutate(date = date %>% strptime(format='%d-%m-%y %H:%M', tz="GMT") %>% as_datetime())
  
  # Borrar variables innecesarias
  df_conc <- df_conc %>% mutate(fecha=NULL,
                                hora=NULL,
                                horario=NULL)
  
  #  Util tener estos dates
  df_conc <- df_conc %>% mutate(year=date %>% year(),
                                month=date %>% month(),
                                day=date %>% day())
  
  # Nombre del Contaminante en la columna ---------
  # key: date, site, tipo_dato
  # df_conc <- df_conc %>% mutate(pollutant=pollutant_cod) %>% 
  #   spread(key=pollutant,value =  valor)
  
  return(df_conc)
} ## EoF
