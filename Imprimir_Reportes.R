### Proyecto SINCA
## Script para imprimir reportes masivamente
## PBH Jul 2020

# Imprimir reporte individual --------------
rmarkdown::render("Reporte_CalidadAire.Rmd",
                  params = list(reg="M",
                                estaciones=c("Las Condes", "Cerrillos",
                                             "Puente Alto", "Pudahuel",
                                             "La Florida", "Independencia"),
                                descarga_datos=T,
                                cont="mp2.5"),
                  output_file = paste("Reportes/Reporte", "prueba", sep="_"))


# Imprimir reportes masivamente --------------
source('Scripts/00-Funciones.R')
contaminantes <- c("mp2.5", "mp10", "so2", "temp", "temp_10m","temp_2m","ws_10m")
df_provincia <- read_delim("Data/EstacionesSINCA.csv", 
                            delim = ";", skip = 1, na = c("NA"), 
                           locale = locale(encoding = "windows-1252")) #encoding latino de windows
regiones <- df_provincia$region %>% unique()

# Definir función para generación de reportes
for (r in regiones){
  dir.create(paste("Reportes/Provincias/",r,sep=""), showWarnings = F)
  provincias <- df_provincia %>% filter(region==r) %>%  
    pull(provincia) %>% unique() %>% 
    map(f_remover_acentos) %>% unlist()

  for (p in provincias){
    dir.create(paste("Reportes/Provincias/",r,p,sep="/"), showWarnings = F)
    for (c in contaminantes){
      file_name <- paste("Reportes/Provincias/",r,"/",p,"/Reporte_",p,"_",
                         str_remove(c,"\\."), sep="")
      tryCatch(
        {
        rmarkdown::render("Reporte_CalidadAire.Rmd",
                          params = list(reg=r,
                                        prov=p,
                                        descarga_datos=T,
                                        cont=c),
                          output_file = file_name)
        }, 
        error = function(cond) return(NULL))
    }
  }
}
rm(df_provincia, contaminantes, regiones,p, c, r, file_name)


# EoF