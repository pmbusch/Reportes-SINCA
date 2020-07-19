### Proyecto SINCA
## Funcion para lectura individual de archivo de concentraciones
## PBH Jul 2020
## Ultima atualizacion: PBH Jul 2020

options(dplyr.summarise.inform=FALSE)
# archivo <- "Data/Provincias/II/El Loa/ColegioPedroVergaraKeller_mp2.5.csv"
## Funcion para leer y agregar datos de concentracion de un archivo ---------
f_readFileConc <- function(archivo){
  df_conc <-read_delim(archivo,
                       delim=";", na = c("NA"), col_types="ccdddd",
                       locale = locale(decimal_mark = ","))
  
  df_conc$X6 <- NULL # columna adicional
  colnames(df_conc) <- c('fecha','hora','validados','preliminares','noValidados')
  
  # Jerarquia de datos: validados > preliminares > noValidados
  df_conc <- df_conc %>% mutate(tipo_dato = case_when(
    !is.na(validados) ~ "validados",
    !is.na(preliminares) ~ "preliminares",
    !is.na(noValidados) ~ "noValidados",
    TRUE ~ "NA"))
  
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
  
  # DATE
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
                                horario=NULL,
                                year=date %>% year(),
                                month=date %>% month(),
                                day=date %>% day())
  
  
  df_conc <- df_conc %>% mutate(archivo=archivo)
  

  df_conc_dia <- df_conc %>% group_by(archivo, year, month, day) %>% 
    summarise(valor=mean(valor,na.rm=T),count=n()) %>% 
    filter(count>=18)
  
  df_conc_anual <- df_conc_dia %>% 
    group_by(archivo,year) %>% 
    summarise(valor=mean(valor,na.rm=T), 
              disponibilidad=n()/365) %>% 
    ungroup()
  
  return(df_conc_anual)
}
