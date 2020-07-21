### Proyecto SINCA
## Generacion de Mapas
## PBH Jun 2020
## Ultima atualizacion: PBH Jul 2020

# Filtro outliers para generar el mapa
## Fuente: https://ropensci.github.io/CoordinateCleaner/articles/Tutorial_geographic_outliers.html


estaciones <- df %>% group_by(site) %>% summarise(longitud=first(longitud),
                                                  latitud=first(latitud)) %>% ungroup()
## OLD
# estaciones <-CoordinateCleaner::cc_outl(estaciones, lon="longitud", lat="latitud", 
#                    species = "site", method = "quantile")
# distancia diagonal del rectangulo
# distancia_diag <- sqrt(
#   (max(estaciones$longitud)-min(estaciones$longitud))^2+
#   (max(estaciones$latitud)-min(estaciones$latitud))^2)

# 30% de margen segun la diagonal
# off_set <- 0.3*distancia_diag
# location <- c(min(estaciones$longitud)-off_set, min(estaciones$latitud)-off_set, 
#               max(estaciones$longitud)+off_set, max(estaciones$latitud)+off_set)


location <- make_bbox(estaciones$longitud,estaciones$latitud, f=0.35)

# Caso que solo sea 1 estacion
if (nrow(estaciones==1)){
  location <- location+0.01*c(-1, -1, 1, 1)
} 
#calc_zoom(estaciones$longitud, estaciones$latitud, f = 0.35)

map <- get_map(location=location, source = "stame", 
               maptype = "terrain", crop=T)
rm(location)

m <- ggmap(map, extent = "device")+
  geom_point(data=estaciones, aes(x=longitud, y=latitud, col=site), size=3)+
  scale_color_viridis_d()+
  theme(legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')+
  geom_label_repel(data=estaciones, aes(x=longitud, y=latitud, label=site))
# m
# ggsave(m, file="Mapa/Estaciones.png", dpi = 300)


## Promedio trianual-------------- 
ano_max <- max(df_anual$year)
df_map <- df_anual %>%
  filter(year>=ano_max-2) %>% 
  group_by(site, longitud, latitud) %>% 
  summarise(valor=mean(valor, na.rm=T) %>% round(1)) %>% ungroup() %>% 
  mutate(etiqueta=paste(site,valor,sep=": "))

m_anual <- ggmap(map, extent = "device")+
  geom_point(data=df_map, aes(x=longitud, y=latitud, col=valor, size=valor))+
  scale_color_viridis_c(option = "plasma", direction=-1, name=label_conc)+
  expand_limits(col=0, size=0)+
  scale_size(guide="none")+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+xlab('')+ylab('')+
  geom_label_repel(data=df_map, aes(x=longitud, y=latitud, label=etiqueta))+
  labs(caption = paste("Promedio trianual: ",ano_max,"-",ano_max-2,sep=""))
# m_anual
rm(df_anual, ano_max)

# ggsave(m_anual, file="Mapa/PromedioTrianual.png", dpi = 300)


rm(map)

## Grafico animado GIF-------------- 
# library(gganimate)
# library(magick) # para exportar
# df_anual <- df_anual %>% mutate(etiqueta=paste(site,valor,sep=": "))
# 
# m_gif <- ggmap(map, extent = "device")+
#   geom_point(data=df_anual, aes(x=longitud, y=latitud, col=valor, size=valor))+
#   scale_color_viridis_c(name=label_conc)+
#   scale_size(guide="none")+
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank())+xlab('')+ylab('')+
#   geom_label_repel(data=df_anual, aes(x=longitud, y=latitud,label=etiqueta))+
#   # Here comes the gganimate specific bits
#   labs(title = 'Año: {closest_state}') +
#   transition_states(year, 1)+
#   enter_fade() +
#   exit_fade()
# 
# m_gif
# animate(m_gif, nframes = 5, duration = 10)
# 
# image_write(animate(m_gif,nframes = 5, duration = 5), path="Mapa/Mapa.gif")

## EoF