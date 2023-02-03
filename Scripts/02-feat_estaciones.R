## Proyecto SINCA
## Arreglo y feature de lo datos de estaciones del SINCA descargados
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020

##  Analizar DF creada ----------------
df_orig <- df

library(skimr)
df %>% head()
df %>% str()
df %>% skim()

# Estaciones en linea: 1807 de 2394
df %>% group_by(estacion_enlinea) %>% summarise(count=n(), freq= n()/nrow(df))


## Arreglar contaminantes a codigos entendibles --------------
df$contaminante_cod %>% unique()
df$contaminante %>% unique()

df <- df %>% mutate(pollutant=case_when(
    contaminante_cod=='PM25' ~ 'mp2.5',
    contaminante_cod=='GLOB' ~ 'rad',
    contaminante_cod=='RHUM' ~ 'hr',
    contaminante_cod=='TEMP' ~ 'temp',
    contaminante_cod=='WDIR' ~ 'wd',
    contaminante_cod=='WSPD' ~ 'ws',
    contaminante_cod=='0001' ~ 'so2',
    contaminante_cod=='PM10' ~ 'mp10',
    contaminante_cod=='00Cu' ~ 'cu',
    contaminante_cod=='00Pb' ~ 'pb',
    contaminante_cod=='ARSE' ~ 'as',
    contaminante_cod=='PRES' ~ 'pres',
    contaminante_cod=='0002' ~ 'no',
    contaminante_cod=='0003' ~ 'no2',
    contaminante_cod=='0004' ~ 'co',
    contaminante_cod=='0008' ~ 'o3',
    contaminante_cod=='0NOX' ~ 'nox',
    contaminante_cod=='RAIN' ~ 'pp',
    contaminante_cod=='OCH4' ~ 'ch4',
    contaminante_cod=='0CH4' ~ 'ch4',
    contaminante_cod=='NMHC' ~ 'hc_nm',
    contaminante_cod=='THCM' ~ 'hc_tot',
    contaminante_cod=='PM2D' ~ 'mp2.5_discrete',
    contaminante_cod=='CORG' ~ 'c_org',
    contaminante_cod=='CTOT' ~ 'c_tot',
    contaminante_cod=='TRSG' ~ 'trs',
    TRUE ~ "Otro"))

df$pollutant %>% unique()

# Agrego altura de medicion al contaminante
df$contaminante_altura %>% unique()
df <- df %>% 
  mutate(pollutant=if_else(is.na(contaminante_altura)|contaminante_altura=="S/I",
                           pollutant, 
                           paste(pollutant, contaminante_altura, sep="_")))

df$pollutant %>% unique()


## Unidad -------------
# a <- df$contaminante_desc %>% unique()
# a %>% str_view("mm", match=T)

# Orden de busqueda es clave !
df <- df %>% 
  mutate(unidad = case_when(
    str_detect(contaminante_desc, "ppb") ~ "ppb",
    str_detect(contaminante_desc, "ppm") ~ "ppm",
    str_detect(contaminante_desc, "mmHg") ~ "mmHg",
    str_detect(contaminante_desc, "mbar") ~ "mbar",
    str_detect(contaminante_desc, "hPa") ~ "hPa",
    str_detect(contaminante_desc, "°C") ~ "°C",
    str_detect(contaminante_desc, "°") ~ "°",
    str_detect(contaminante_desc, "%") ~ "%",
    str_detect(contaminante_desc, "W/m2") ~ "W/m2",
    str_detect(contaminante_desc, "m/s") ~ "m/s",
    str_detect(contaminante_desc, "mg/m3N") ~ "mg/m3N",
    str_detect(contaminante_desc, ".g/m3N") ~ "ug/m3N",
    str_detect(contaminante_desc, ".g/m3") ~ "ug/m3",
    str_detect(contaminante_desc, "m/m") ~ "mm",
    str_detect(contaminante_desc, "mm/h") ~ "mm/h",
    str_detect(contaminante_desc, "mm") ~ "mm",
    contaminante_desc=="Dirección del viento" ~ "°",
    contaminante_desc=="Humedad relativa del aire" ~ "%",
    contaminante_desc=="Temperatura ambiente" ~ "°C",
    contaminante_desc=="Velocidad del viento" ~ "m/s",
    TRUE ~ "s/i"))

# Verificacion
df %>% group_by(contaminante_desc, unidad) %>% summarise(count=n())


## Corregir datos para descargar .csv de concentraciones --------------

# Datos del contaminante y metrica para la descarga
# Datos de fecha inicio descarga
#Datos fecha fin descarga
df <- df %>% 
  mutate(macro = url_contaminante %>% str_split('&macropath=') %>% 
                      sapply(function(x) x[2]) %>%
                      str_split('&from') %>% sapply(function(x) x[1]) %>% 
                      str_replace('&macro=','/') %>% paste(.,'.ic',sep=''),
         from = url_contaminante %>% str_split('&from=') %>% 
           sapply(function(x) x[2]) %>% str_split('&to=') %>% sapply(function(x) x[1]),
         to = url_contaminante %>% str_split('&to=') %>% 
           sapply(function(x) x[2]) %>% str_remove('&'))

# Datos al dia
df <- df %>% mutate(fecha_fin_actual= contaminante_fechaFin==fecha_consulta)

# URL con formato para la descarga de datos del SINCA
url <-  '/cgi-bin/APUB-MMA/apub.tsindico2.cgi?outtype=xcl&macro=%s&from=%s&to=%s&path=/usr/airviro/data/CONAMA/&lang=esp'
df<- df %>% mutate(url_descarga=sprintf(url,macro,from,to))

## Otros ------------

# Metrica de resolucion de los datos
df <- df %>% mutate(metrica=case_when(
  str_detect(.$macro,'horario') ~ 'Horario',
  str_detect(.$macro,'diario') ~ 'Diario',
    TRUE ~ 'Otro'))
df$metrica %>% unique()

# Agregar fechas mejor
df %>% names
df <- df %>% mutate(ano_inicio=df$from %>% str_sub(0,2) %>% as.numeric() %>% {ifelse(.<30,.+2000,.+1900)},
               ano_fin=df$to %>% str_sub(0,2) %>% as.numeric() %>% {ifelse(.<30,.+2000,.+1900)},
               mes_inicio=df$from %>% str_sub(3,4),
               mes_fin=df$to %>% str_sub(3,4)) 


## Coordenadas oficiales -------------
# Usamos los datos en UTM pq en los mapas dan mas precicos que las lat/long extraidos
library(rgdal)

df$coord_utm
df$huso %>% unique()
df %>% group_by(huso) %>% summarise(count=n())

# Fix coordenadas de estacion Concon
df <- df %>% mutate(coord_utm=ifelse(url_estacion=="/index.php/estacion/index/id/202",
                                     "265070 E 6354089 N",
                                     coord_utm))

coord <- df %>% select(coord_utm, huso) %>% 
  mutate(easting=f_split_n(coord_utm," E",1) %>% as.numeric(),
         northing=f_split_n(coord_utm,"E ",2) %>% str_remove(" N") %>% as.numeric())

coord <- coord %>% select(easting, northing)

# Debo coventir para ambas zonas (husos)
sputm_18 <- SpatialPoints(coord, proj4string=CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
sputm_19 <- SpatialPoints(coord, proj4string=CRS("+proj=utm +zone=19 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))

# Convierto a lat/long
spgeo_18 <- spTransform(sputm_18, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) %>% 
  coordinates() %>% data.frame()
spgeo_19 <- spTransform(sputm_19, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) %>% 
  coordinates() %>% data.frame()

# Traspaso las coordenadas segun su huso
df <- df %>% 
  mutate(longitud_utm=if_else(huso==19, spgeo_19$easting, spgeo_18$easting),
         latitud_utm=if_else(huso==19, spgeo_19$northing, spgeo_18$northing))

# Ver que estacion este en Chile
library(maps)
df <- df %>% 
  mutate(pais=map.where(database="world", longitud_utm, latitud_utm))
df$pais %>% unique()

## Si la estacion esta en chile dejo las coord calculadas, en caso contrario las scrapeadas
df <- df %>% mutate(
  longitud=if_else(pais=="Chile", longitud_utm, longitud),
  latitud=if_else(pais=="Chile", latitud_utm, latitud),
  latitud_utm=NULL, longitud_utm=NULL, pais=NULL)

rm(coord)

## Guardar como .csv -------------
cat('sep=; \n',file = "Data/DatosEstacioneSINCA.csv")
write.table(df,'Data/DatosEstacioneSINCA.csv',sep=';',row.names = F, append = T,
            fileEncoding="UTF-8")


## Estaciones unicas ------------
# Estaciones sin redundancia de parametros

df %>% names()
df_estaciones <- df %>% group_by(region, url_region, estacion, estacion_enlinea,
                                 url_estacion, provincia, comuna, coord_utm, huso,
                                 longitud, latitud, fecha_consulta) %>% 
  summarise(n_parametros=n()) %>% ungroup()
df_estaciones

# Guardar
cat('sep=; \n',file = "Data/EstacionesSINCA.csv")
write.table(df_estaciones,'Data/EstacionesSINCA.csv',sep=';',row.names = F, append = T)

## EoF