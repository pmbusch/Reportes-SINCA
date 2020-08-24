## Load data. Carga los datos y los filtras por los parametros elegidos para el reporte
# PBH Jun 2020

## Lectura Archivo --------

# Tipo de las variables: c character, d double, D date, T date time
cols_type <- "cdTdddccccccdddccc"
df <- read_delim("Data/Datos_Concentraciones.csv", delim = ";", skip = 1, na = c("NA"),
                 col_types = cols_type,
                 locale = locale(encoding = "windows-1252"))
rm(cols_type)
spec(df)


## Filtros --------
## NA Data
df <- df %>% filter(!is.na(valor))

unidad <- paste("[",df$unidad %>% unique() %>% .[1],"]", sep="")

# label de los ejes
label_conc <- paste(cont,unidad,sep=" ")


## Normas de Calidad -----------
df_normas <- read_excel("Data/Normas.xlsx", na = "NA")
norma_anual <- df_normas %>% filter(pollutant==cont) %>% pull(norma_anual)
norma_diaria <- df_normas %>% filter(pollutant==cont) %>% pull(norma_diaria)
rm(df_normas)

# df %>% filter(tipo_dato=='validos')

n_estaciones <- df$site %>% unique() %>% length()

# EoF