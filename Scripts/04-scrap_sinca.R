## Proyecto SINCA
## Descarga de datos de la web del SINCA
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020


## Carga Datos de las estaciones -------------
# Scripts necesarios
source('Scripts/00-Funciones.R')
source('Scripts/03_f_scrap_sinca.R')

# Tipo de las variables: c character, d double, D date
cols_type <- "ccclccccdddccccDDccDccccclcccccc"
df_estaciones <- read_delim("Data/DatosEstacioneSINCA.csv", 
                            delim = ";", skip = 1, na = c("NA"),
                            col_types = cols_type,
                            locale = locale(encoding = "windows-1252"))
rm(cols_type)
spec(df_estaciones)


## FILTROS DESCARGA ---------------
# para descargar los datos, permite hacer filtro para no descargar todo
df_estaciones %>% names
df_estaciones$region %>% unique()
df_estaciones$pollutant %>% unique()


# REGION, ESTACIONES, CONTAMINANTES Y METRICA
df_descarga <- df_estaciones %>%  
  filter(region==reg & 
           estacion_enlinea %in% enLinea &
           pollutant==cont & 
           metrica==metric &
           (f_remover_acentos(provincia) %in% prov|prov=="all") &
           (comuna %in% comunas|comunas=="all") &
           (estacion %in% estaciones|estaciones=="all") )


# Si estaba al dia al momento de recolectar la info, actualizamos la fecha fin
df_descarga <- df_descarga %>% 
  mutate(contaminante_fechaFin=if_else(fecha_fin_actual, Sys.Date(), contaminante_fechaFin))


# Validez Fechas ingresadas
df_descarga<- df_descarga %>%  
  mutate(contaminante_fechaInicio = contaminante_fechaInicio %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         contaminante_fechaFin = contaminante_fechaFin %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         from = fecha_inicio %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         to = fecha_fin %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         inicio_valido = from>contaminante_fechaInicio,
         fin_valido = to<contaminante_fechaFin)

# Dejar solamente fechas validas, si no esta dentro del rango se deja el valor limite
df_descarga <- df_descarga %>% 
  mutate(from = if_else(inicio_valido, from, contaminante_fechaInicio)%>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
         to = if_else(fin_valido, to, contaminante_fechaFin)%>% 
           strptime(format='%Y-%m-%d') %>% as_date())

# Fechas en formato descarga (ej: pasar de 2015-12-31 a 151231)
df_descarga <- df_descarga %>% 
  mutate(from=paste(f_split_n(from,'-',1) %>% str_sub(3,4),
                                   f_split_n(from,'-',2),
                                   f_split_n(from,'-',3),sep=''),
         to=paste(f_split_n(to,'-',1) %>% str_sub(3,4),
                                 f_split_n(to,'-',2),
                                 f_split_n(to,'-',3),sep='')) 


# DESCARGA DE DATOS DE CONCENTRACION -----------------
# Reemplazable macro, date_from, date_to
url <-  'https://sinca.mma.gob.cl/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=%s&from=%s&to=%s&path=/usr/airviro/data/CONAMA/&lang=esp'

# Crear descarga con parametros establecidos
df_descarga <- df_descarga %>% mutate(url_descarga=sprintf(url,macro,from,to))


# Crear DF para almacenar informacion
df <- data.frame(tipo_dato=as.character(),
                 site=factor(),
                 region=factor(),
                 provincia=factor(),
                 comuna=factor(),
                 coord_utm=as.character(),
                 huso=as.character(),
                 longitud=as.numeric(),
                 latitud=as.numeric(),
                 tecnica=factor(),
                 unidad=factor(),
                 date=as.character(),
                 year=as.numeric(),
                 month=as.numeric(),
                 day=as.numeric(),
                 pollutant=as.character(),
                 valor=as.numeric())

# Recorro los datos a descargar, almaceno la informacion en mi dataframe
for (d in 1:length(df_descarga$region)){
  # Descarga concentraciones
  
  df_conc <- f_scrap_sinca(df_descarga$url_descarga[d],
                           file_name = paste(df_descarga$estacion[d], df_descarga$contaminante_cod[d],sep="_"),
                           remover_file = T)
  
  # Agregar info adicional
  df_conc <- df_conc %>% 
    mutate(site=df_descarga$estacion[d],
           region=df_descarga$region[d],
           provincia=df_descarga$provincia[d],
           comuna=df_descarga$comuna[d],
           coord_utm=df_descarga$coord_utm[d],
           huso=df_descarga$huso[d],
           longitud=df_descarga$longitud[d],
           latitud=df_descarga$latitud[d],
           tecnica=df_descarga$contaminante_tecnica[d],
           unidad=df_descarga$unidad[d],
           pollutant=df_descarga$pollutant[d])
  
  # Agregar a DF
  df <- rbind(df,df_conc)
  rm(df_conc)
  }

# Guardar
cat('sep=; \n',file = "Data/Datos_Concentraciones.csv")
write.table(df,'Data/Datos_Concentraciones.csv',sep=';',row.names = F, append = T)

## EoF