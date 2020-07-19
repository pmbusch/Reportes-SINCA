### Proyecto SINCA
## Descarga Masiva Datos SINCA. Descarga unicamente los CSV, sin modificarlos
## PBH Jul 2020
## Ultima atualizacion: PBH Jul 2020

## Carga Datos de las estaciones -------------
# Scripts necesarios
source('Scripts/00-Funciones.R')
source('Scripts/03_f_scrap_sinca.R')
library(tidyverse)
library(lubridate)
library(ggmap)
library(ggrepel)

# Tipo de las variables: c character, d double, D date
cols_type <- "ccclccccdddccccDDccDccccclcccccc"
df_estaciones <- read_delim("Data/DatosEstacioneSINCA.csv", 
                            delim = ";", skip = 1, na = c("NA"),
                            col_types = cols_type,
                            locale = locale(encoding = "windows-1252"))
rm(cols_type)
spec(df_estaciones)

df_estaciones %>% names
df_estaciones$region %>% unique()
df_estaciones$pollutant %>% unique()

# Limites temporales descarga
# Si estaba al dia al momento de recolectar la info, actualizamos la fecha fin
df_estaciones <- df_estaciones %>% 
  mutate(
    contaminante_fechaFin = if_else(fecha_fin_actual, Sys.Date(), contaminante_fechaFin),
    from = contaminante_fechaInicio %>% 
           strptime(format='%Y-%m-%d') %>% as_date(),
    to = contaminante_fechaFin %>% 
           strptime(format='%Y-%m-%d') %>% as_date())

# Fechas en formato descarga (ej: pasar de 2015-12-31 a 151231)
df_estaciones <- df_estaciones %>% 
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
df_estaciones <- df_estaciones %>% mutate(url_descarga=sprintf(url,macro,from,to))


# Loop para descargar datos
# jerarquia: region-provincia- file: estacion_cont
regiones <- df_estaciones$region %>% unique()
for (r in regiones){
  cat("Descargando Region ",r,"\n", sep = "")
  dir.create(paste("Data/Provincias/",r,sep=""), showWarnings = F)
  
  provincias <- df_estaciones %>% filter(region==r) %>% pull(provincia) %>% unique()
  for (p in provincias){
    cat("Descargando Provincia de ",p,"\n", sep = "")
    dir.create(paste("Data/Provincias/",r,"/",p,sep=""), showWarnings = F)
    
    ## Mapa estaciones por provincia -----------
    estaciones_mapa <- df_estaciones %>% filter(provincia==p) %>% 
      group_by(estacion) %>% 
      summarise(longitud=first(longitud), latitud=first(latitud))%>% 
      ungroup()
    
    location <- make_bbox(estaciones_mapa$longitud,estaciones_mapa$latitud, f=0.35)
    if (nrow(estaciones_mapa==1)){ # Caso que solo sea 1 estacion
      location <- location+0.01*c(-1, -1, 1, 1)
    } 
    map <- get_map(location=location, source = "stame", 
                   maptype = "terrain", crop=T)
    
    m <- ggmap(map, extent = "device")+
      geom_point(data=estaciones_mapa, aes(x=longitud, y=latitud, col=estacion), size=3)+
      scale_color_viridis_d()+
      theme(legend.position = "none",
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())+xlab('')+ylab('')+
      geom_label_repel(data=estaciones_mapa, aes(x=longitud, y=latitud, label=estacion))
    
    ggsave(filename = paste("Data/Provincias/",r,"/",p,"/1Mapa_",p,".png",sep=""),
           plot=m, dpi = 300)
    rm(location, estaciones_mapa, map, m)
    
    ## Descarga datos de concentracion ----------
    df_descarga <- df_estaciones %>% filter(provincia==p)
    
    cat(nrow(df_descarga)," archivos a descargar","\n", sep = "")
    
    for (d in 1:nrow(df_descarga)){
      sitio <- df_descarga$estacion[d] %>% str_remove_all(" |'")
      destino <- paste("Data Scrap/Provincias/",r,"/",p,"/",
                       sitio,"_",
                       df_descarga$pollutant[d],
                       ".csv",sep="")
      
      tryCatch(
        {
          download.file(df_descarga$url_descarga[d],destfile = destino)
        }, 
        error = function(cond) return(NULL))
    }
    rm(df_descarga)
  }
}

# Crear ReadME ---------
cat("Este directorio contiene los datos descargados del SINCA ",
    "para todas las estaciones con información disponible. \n\n",
    "Fecha de descarga de los datos: ",format(Sys.time(),'%d-%m-%Y'),"\n\n",
    "Los datos descargados son a nivel HORARIO, por estacion y contaminante \n\n",
    "Cada carpeta a nivel de provincia incluye un mapa con la ubicación ",
    "de las estaciones de monitoreo \n",
    file = "Data/Provincias/ReadMe.txt", sep="", append = F)

## EoF