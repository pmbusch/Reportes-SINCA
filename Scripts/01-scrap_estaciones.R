## Proyecto SINCA
## Scrap de lo datos de estaciones del SINCA
## Permite generar un archivo csv con datos de las estaciones, monitoreo, y 
## links para su descarga
## El codigo de scrap utiliza esta fuente de datos para realizar la descarga 
# mas simple y efectiva
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020


## CARGA LIBRERIAS Y DATOS -------------
library(rvest)
library(tidyverse)
library(readr)
library(lubridate)
# Scripts necesarios
source('Scripts/00-Funciones.R')

# Url de la pagina de descarga principal
path_pagina='https://sinca.mma.gob.cl'

# Estructufa del DF para almacenar los datos a descargar
df <- data.frame(region=as.character(),
                 url_region=as.character(),
                 estacion=as.character(),
                 estacion_enlinea=as.logical(),
                 url_estacion=as.character(),
                 provincia=as.character(),
                 comuna=as.character(),
                 coord_utm=as.character(),
                 huso=as.character(),
                 longitud=as.numeric(),
                 latitud=as.numeric(),
                 contaminante=as.character(),
                 contaminante_desc=as.character(),
                 contaminante_cod=as.character(),
                 contaminante_tecnica=as.character(),
                 contaminante_fechaInicio=as.character(),
                 contaminante_fechaFin=as.character(),
                 contaminante_altura=as.character(),
                 url_contaminante=as.character(),
                 fecha_consulta=as.character(),
                 stringsAsFactors=FALSE)


# Idea general: Ir entrando de mas general a mas especifico, y 
# guardar en una sola linea (Tabla Redundante) toda la info
# El key de la base de datos es un archivo de texto a descargar, el cual 
# tiene una estacion, parametro y metrica (hora, dia, mes) de descarga


## PAGINA PRINCIPAL  -------------

# Regiones
web <- path_pagina %>% read_html() # Descarga de pagina web

# Descarga de datos de regiones a almacenar en la dataframe
regiones_url <- html_nodes(web,'#menu > ul > li.last > ul > li > a') %>% html_attr('href')
regiones <- regiones_url %>% str_split('id/') %>% sapply(., function(x) x[2])


## PAGINA REGION ---------------
# Recorro las estaciones dentro de cada region
for (r in 1:length(regiones)){
  cat('Descargando region ',regiones[r],': ')
  web <- paste(path_pagina,regiones_url[r],sep='') %>% read_html() # Descarga de pagina web
  
  # Descarga de datos de estaciones a almacenar en la dataframe
  estaciones_url <- html_nodes(web,'#tablaRegional > tbody > tr > th > a') %>% html_attr('href')
  estaciones <- html_nodes(web,'#tablaRegional > tbody > tr > th > a') %>% html_text()
  est_enlinea <- html_nodes(web,'#tablaRegional > tbody > tr > th') %>% grepl('en línea',.) #Bool de estaciones en linea
  
  cat(length(estaciones)," estaciones","\n", sep="")
  
  ## PAGINA ESTACION --------------
  # Contaminantes dentro de cada estacion
  for (e in 1:length(estaciones)){
    # Manejo de error HTTP error 500
    tryCatch(
      {
        web <- paste(path_pagina,estaciones_url[e],sep='') %>% read_html()
        
        # Datos estacion
        e_provincia <- html_node(web,'#tablaGeneral > tbody > tr:nth-child(4) > td') %>% html_text()
        e_comuna <- html_node(web,'#tablaGeneral > tbody > tr:nth-child(5) > td') %>% html_text()
        e_coord_utm <- html_node(web,'#tablaGeneral > tbody > tr:nth-child(6) > td') %>% html_text() %>% 
          str_remove('\n') %>% str_trim('both')
        e_huso <- html_node(web,'#tablaGeneral > tbody > tr:nth-child(7) > td') %>% html_text() %>% 
          str_trim('both')
        
        latLong <- web %>% html_text() %>% str_extract("LatLng\\(-\\d*.\\d*,.*\\d*.\\d*")
        lat <- latLong %>% str_remove("LatLng") %>% str_remove_all("\\(") %>% 
          f_split_n(",",1) %>% as.numeric()
        long <- latLong %>% str_remove("LatLng") %>% str_remove_all("\\)") %>% 
          str_remove_all(";") %>% f_split_n(",",2) %>% as.numeric()
        rm(latLong)
        
        web <- paste(path_pagina,estaciones_url[e],sep='') %>% read_html(options = 'HUGE')
      
        # Contaminantes
        cont <- html_nodes(web,'#medicion > tbody > tr > th > span > a') %>% html_text()
        
        # Solo agrego datos de contaminantes si existen
        if(length(cont)!=0) { 
          cont_desc <- html_nodes(web,'#medicion > tbody > tr > th') %>% html_text() %>% 
            str_split('\n') %>% sapply(., function(x) x[2]) %>% trimws('both')
          # Codigo contaminates  
          cod_cont <- html_nodes(web,'#medicion > tbody > tr > th > span > a') %>% 
            html_attr('name')
          # Tecnica medicion
          tecn <- html_nodes(web,'#medicion > tbody > tr > td.helpTecnica.center') %>% 
            html_text() %>% str_remove_all(.,'\\n') %>% str_trim(.,side='both')
          # Fecha registros
          
          #medicion > tbody > tr:nth-child(1) > td:nth-child(2)
          cont_ini <- html_nodes(web,'#medicion > tbody > tr > td:nth-child(2)') %>% 
            html_text() %>% strptime(format='%d-%m-%Y') %>% as_date()
          # Fechas vienen al reves a veces (x eso el ifelse)
          cont_fin <- html_nodes(web,'#medicion > tbody > tr > td:nth-child(3)') %>% 
            html_text()
          
          # manera muy larga de corregirlo, pero sirve
          cont_fines <- tibble(c=cont_fin) %>% mutate(dia_orig=f_split_n(c, "-", 1),
                                                      mes=f_split_n(c, "-", 2),
                                                      ano_orig=f_split_n(c, "-", 3))
          cont_fines <- cont_fines %>% 
            mutate(dia=if_else(str_sub(dia_orig,1,3)=="202",ano_orig, dia_orig),
                   ano=if_else(str_sub(dia_orig,1,3)=="202",dia_orig, ano_orig),
                   fecha=paste(dia,mes,ano,sep="-"))
          cont_fin <- cont_fines %>% pull(fecha) %>% 
            strptime(format='%d-%m-%Y') %>% as_date()
          rm(cont_fines)
          
          # old (no sirve mas)
          # cont_fechas <- html_nodes(web,'#medicion > tbody > tr > td > a') %>% html_text()
          # cont_fechas <- cont_fechas[cont_fechas!='']
          # cont_ini <- cont_fechas %>% str_split('desde ') %>% sapply(function(x) x[2]) %>%
          #   str_split(' hasta') %>% sapply(function(x) x[1]) %>%
          #   strptime(format='%d-%m-%Y') %>% as_date()
          # cont_fin <- cont_fechas %>% str_split('hasta ') %>% sapply(function(x) x[2]) %>%
          #   strptime(format='%d-%m-%Y') %>% as_date()
          
          # Codigo descarga contaminantes
          cont_descarga <- html_nodes(web,'#medicion > tbody > tr > td > a') %>% 
            html_attr('href')
          codigo <- paste('macro=',cod_cont,sep='')
          
          
          ## DESCARGA CONTAMINANTES -----------
          # Recorro los contamiantnes a descargar
          for (p in 1:length(cont)){
            cont_df <- cont_descarga[grepl(codigo[p],cont_descarga)]
            
            ## TIPO DE DATO CONTAMINANTE 
            # Recorro cada tipo de dato de descarga
            for(p_df in 1:length(cont_df)){
              # Agrego las filas al dataframe
              df <- rbind(df,data.frame(region=regiones[r],
                                        url_region=regiones_url[r],
                                        estacion=estaciones[e],
                                        estacion_enlinea=est_enlinea[e],
                                        url_estacion=estaciones_url[e],
                                        provincia=e_provincia,
                                        comuna=e_comuna,
                                        coord_utm=e_coord_utm,
                                        huso=e_huso,
                                        longitud=long,
                                        latitud=lat,
                                        contaminante=cont[p],
                                        contaminante_desc=cont_desc[p],
                                        contaminante_cod=cod_cont[p],
                                        contaminante_tecnica=tecn[p],
                                        contaminante_fechaInicio=cont_ini[p],
                                        contaminante_fechaFin=cont_fin[p],
                                        contaminante_altura=NA,
                                        url_contaminante=cont_df[p_df],
                                        fecha_consulta=Sys.Date(),
                                        stringsAsFactors=FALSE))
        
            } #Fin metricas contamiantnes
          } #Fin contaminantes
        } #Condicion de existencia de contaminantes
        
        
        # DESCARGA METEOROLOGIA --------------
        meteo <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/th/span/a') %>% html_text()
        
        # Solo agrego datos de meteorologia si existen
        if(length(meteo)!=0){ 
          
          meteo_desc <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/th') %>% 
            html_text()
          meteo_cod <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/th/span/a') %>% 
            html_attr('name')
          meteo_fechas <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/td[@class="center"]') %>% 
            html_text()
          meteo_ini <- meteo_fechas[seq(3,length(meteo_fechas),5)] %>% 
            strptime(format='%d-%m-%Y') %>% as_date()
          meteo_fin <- meteo_fechas[seq(4,length(meteo_fechas),5)] %>% 
            strptime(format='%d-%m-%Y') %>% as_date()
          meteo_altura <- meteo_fechas[seq(2,length(meteo_fechas),5)] %>% 
            str_remove_all(" ")
          meteo_tecn <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/td[@class="helpTecnica center"]') %>% 
            html_text()
          
          # Codigo descarga contaminantes
          meteo_descarga <- html_nodes(web,xpath = '//*[@id="contenidoDinamico"]/table[@class="gob min"]/tbody/tr/td/a') %>% html_attr('href')
          
          #Meteorologia tiene datos unicos, las recorro para agregar una a una
          for (met in 1:length(meteo)){
            # Agrego las filas al dataframe
            df <- rbind(df,data.frame(region=regiones[r],
                                      url_region=regiones_url[r],
                                      estacion=estaciones[e],
                                      estacion_enlinea=est_enlinea[e],
                                      url_estacion=estaciones_url[e],
                                      provincia=e_provincia,
                                      comuna=e_comuna,
                                      coord_utm=e_coord_utm,
                                      huso=e_huso,
                                      longitud=long,
                                      latitud=lat,
                                      contaminante=meteo[met],
                                      contaminante_desc=meteo_desc[met],
                                      contaminante_cod=meteo_cod[met],
                                      contaminante_tecnica=meteo_tecn[met],
                                      contaminante_fechaInicio=meteo_ini[met],
                                      contaminante_fechaFin=meteo_fin[met],
                                      contaminante_altura=meteo_altura[met],
                                      url_contaminante=meteo_descarga[met],
                                      fecha_consulta=Sys.Date(),
                                      stringsAsFactors=FALSE))
          
          } #Meteorologia
    
        } #Condicion de existencia de meteorologia
      },  # TRY CATCH
      error = function(cond) return(NULL)
    )
  } #Estaciones
} #regiones

# Borrar workspace
rm(cont,cont_desc,cont_descarga,cont_df,cont_fin,cont_ini,e,est_enlinea,estaciones,estaciones_url,
   met,meteo,meteo_cod,meteo_desc,meteo_descarga,meteo_fechas,meteo_fin,meteo_ini,meteo_tecn,p,p_df,path_pagina,
   r,regiones,regiones_url,tecn,web,cod_cont,codigo,e_comuna,e_coord_utm,e_provincia,e_huso, lat, long)

## EoF