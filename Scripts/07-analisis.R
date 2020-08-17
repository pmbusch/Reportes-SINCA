### Proyecto SINCA
## Rutinas para generar figuras para el archivo RMarkdown
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020


## TABLAS RESUMEN -------------
# Metrica horario, se de obtener el promedio por dia y dp se obtiene el promedio anual de los dias

tabla_estacion <-  df %>% 
  group_by(site,region,comuna,coord_utm,huso) %>% 
  summarise(count=n()) %>% select(-count) %>% 
  rename(estacion=site, `coordenadas utm`=coord_utm)

tabla_tecnica <-  df %>% 
  group_by(site,pollutant,unidad, tecnica) %>% 
  summarise(count=n()) %>% select(-count) %>% 
  rename(estacion=site, contaminante=pollutant)

# tabla disponibilidad (no contabiliza años bisiestos con 1 dias mas)
tabla_disponibilidad <- df_diario %>% 
  group_by(site,year) %>% summarise(valor=n()/365*100) %>% 
  spread(year, valor) %>% rename(estacion=site)


# promedio anual
tabla_anual <- df_diario %>% 
  group_by(site,year) %>% summarise(valor=mean(valor,na.rm=T)) %>% spread(year, valor) %>% 
  rename(estacion=site)

# Percentil 98 de la concentracion diaria
tabla_diario <- df_diario %>% 
  group_by(site,year) %>% 
  summarise(valor=quantile(valor,0.98,na.rm = T)) %>% spread(year, valor) %>% 
  rename(estacion=site)

## GGPLOT -----------
theme_set(theme_bw())

## Time Variation -----------
# Lo imprimo en un archivo temporal que dp elimino para evitar que vaya a la consola y al documento markdown
t <- tempfile()
pdf(file=t)
time_variation<- timeVariation(df_openAir, pollutant = cont, 
                                      key=T, key.columns = 4, 
                                      hemisphere='southern', group = "site")

time_variation_season<- timeVariation(df_openAir, pollutant = cont, 
                               key=T, key.columns = 4, type ="season", 
                               hemisphere='southern', group = "site")
dev.off()
file.remove(t)
rm(t)


# Serie de tiempo ---------------
# Para expandir los limites y mostrar bien los labels
ano_max <- max(df_anual$year)
ano_min <- min(df_anual$year)

serie_tiempo <- df_diario %>% ggplot(aes(x=date,y=valor,col=tipo_dato))+
  geom_line(aes(group=site))+
  geom_hline(yintercept = norma_diaria, col="red", linetype = "dashed")+
  # geom_text(x=as_date(paste(ano_min,"-01-01",sep="")), 
  #           y=norma_diaria, vjust=-.5, 
  #           label="Norma diaria del percentil 98", col="red", size=3)+
  facet_grid(site~.)+
  labs(x = "", y = label_conc, col = "Tipo Dato",
       caption="Línea punteada representa la norma diaria del percentil 98")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+xlab('')+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
serie_tiempo


# Tendencia mensual
tendencia_mensual <- df_mes %>% 
  ggplot(aes(x=date,y=valor,col=site))+
  geom_line()+geom_point()+
  geom_hline(yintercept = norma_anual, col="red", linetype = "dashed",size=1)+
  geom_text(x=as_date(paste(ano_min,"-06-01",sep="")),
            y=norma_anual, vjust=-0.5, label="Norma anual", col="red")+
  labs(x = "", y = label_conc, col = "Estación")+
  scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  scale_color_viridis_d()+xlab('')+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")
# tendencia_mensual

# Slope Graph  -------------

# Filter max and min values
df_max <- df_anual %>% group_by(site) %>% slice(which.max(year))
df_min <- df_anual %>% group_by(site) %>% slice(which.min(year))

slope_graph <- df_anual %>% ggplot(aes(x = year, y = valor)) +
  geom_line(aes(group = site, color = site), size=1.5) +
  geom_point(color = "white", size = 4) +
  geom_point(color = "#0072B2", size = 2) +
  geom_label_repel(data = df_min, 
                  aes(label = site, color=site) , 
                  hjust = "left", 
                  size = 4, 
                  nudge_x = -.75,
                  direction = "y")+
  geom_label_repel(data = df_max, 
                  aes(label = site, color=site),
                  hjust = "right", 
                  size = 4, 
                  nudge_x = .75, 
                  direction = "y")+
  geom_label(aes(label = valor), 
             size = 4, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0)+
  geom_hline(yintercept = norma_anual, col="red", linetype = "dashed", size=1)+
  scale_color_viridis_d()+
  scale_y_continuous(name = label_conc)+
  expand_limits(x=c(ano_min-.5,ano_max+.5))+
  scale_x_continuous(name="", breaks=ano_min:ano_max)+
  theme(legend.position = "none")+
  geom_text(x=ano_min,y=norma_anual, vjust=-1, label="Norma anual", col="red")
# slope_graph
rm(df_max, df_min, ano_max, ano_min)

# HEAT MAP  -------------
# Debo crear DF con datos promedio por mes de la hora
df_horaMensual <- df %>% 
  mutate(hora=hour(date),
         month=month(date, label=T)) %>% 
  group_by(site,pollutant,year,month, hora) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  mutate(date=paste("01",month,year,sep="-") %>% strptime(format="%d-%m-%Y") %>% 
           as_date())

heat_map <- df_horaMensual %>%
  ggplot( aes(x =month, y =hora, fill = valor, col=valor))+
  geom_tile(size=0.5) +
  facet_grid(year~site)+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                        na.value = "white", direction = 1,
                        name = label_conc)+
  scale_color_distiller(palette = "YlOrRd", type = 'seq', 
                        na.value = "white", direction = 1,
                        name = label_conc)+
  expand_limits(col=0, fill=0)+
  theme(axis.text.x = element_text(angle = 90),legend.title = element_text(size = 12),
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.major=element_blank(), panel.grid.minor = element_blank()) +
  labs(x = "Mes", y = "Hora")

rm(df_horaMensual)

## EoF