## Feature data. Agregar variables para analisis
# PBH Jul 2020

# df %>% skim()

## Limpieza --------------


# Nuevas variables -----------

# Factores
df <- df %>% mutate(tipo_dato=tipo_dato %>% as.factor(),
                    site=site %>% as.factor(),
                    region=region %>% as.factor(),
                    provincia=provincia %>% as.factor(),
                    comuna=comuna %>% as.factor(),
                    pollutant=pollutant %>% as.factor())

# Cuartiles hora
df <- df %>% 
  mutate(cuartilHora=date %>% hour() %>% sapply(.,cuartilHora) %>% as.factor())


## Promedios diario (para metrica de Horario) -------------
# Filtro datos con al menos el 75% del dia (18 horas)
df_diario <- df %>% 
  group_by(site,pollutant,year,month,day, longitud, latitud) %>% 
  summarise(valor=mean(valor,na.rm=T),count=n()) %>% 
  filter(count>=18) %>% 
  mutate(date=paste(day,month,year,sep="-") %>% strptime(format="%d-%m-%Y") %>% 
           as_date())

## Agrego la clasificacion de tipo de dato segun mayoria
tipo_dato_day <- df %>% 
  group_by(site,pollutant,year,month,day, tipo_dato) %>% 
  summarise(count=n()) %>% 
  slice(which.max(count)) %>% ungroup() %>% 
  mutate(date=paste(day,month,year,sep="-") %>% strptime(format="%d-%m-%Y") %>% 
           as_date()) %>% 
  select(site, pollutant, date, tipo_dato)


# Reordeno los nivels del factor tipo_dato
tipo_dato_day <- tipo_dato_day %>% mutate(
  tipo_dato = factor(tipo_dato, levels=c("validados", "preliminares", "noValidados"))
)

df_diario <- left_join(df_diario, tipo_dato_day, by=c("site", "pollutant", "date"))

## Promedio mensual y anual ------------------
# Promedio mensual y anual se calculan a partir de los datos diarios
# Sin restricciones de dias, dado qe me interesa un analisis exploratorio completo
df_mes <- df_diario %>% 
  group_by(site,pollutant,year,month) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  mutate(date=paste("01",month,year,sep="-") %>% strptime(format="%d-%m-%Y") %>% 
           as_date())

df_anual <- df_diario %>% 
  group_by(site,pollutant,year, longitud, latitud) %>% 
  summarise(valor=mean(valor,na.rm=T) %>% round(0)) %>%
  mutate(date=paste("01","01",year,sep="-") %>% strptime(format="%d-%m-%Y") %>% 
           as_date())


## DF para open air ----------
# Formato columnas como contaminantes
df_openAir <- df %>% spread(pollutant, valor)


