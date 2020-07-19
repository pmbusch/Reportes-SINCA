### Proyecto SINCA
## Analisis Particulares
## PBH Jul 2020
## Ultima atualizacion: PBH Jul 2020

# memory.limit()
# memory.limit(size=56000)

## Scripts necesarios ------

source("Scripts/00-CargaLibrerias.R")
source("Scripts/00-Funciones.R")
source("Scripts/Analisis Nacional/f_readFile.R")
source("Scripts/Analisis Nacional/load_ALL_data.R")
theme_set(theme_bw())

## Data estaciones ----------
cols_type <- "ccclccccdddDd"
df_estaciones <- read_delim("Data/EstacionesSINCA.csv", 
                            delim = ";", skip = 1, na = c("NA"),
                            col_types = cols_type,
                            locale = locale(encoding = "windows-1252"))
rm(cols_type)
spec(df_estaciones)


## Carga datos de promedios anuales por estacion  ------
df_conc <- data.frame()
df_conc <- f_loadAllData("Data Scrap/Provincias", "mp2.5",df_conc)

## Feat data  ------
# Levels regiones
levels_region <- c("XV","I","II","III","IV","V","M","VI","VII","VIII",
                   "IX","XIV","X","XI","XII")
# 
df_conc <- df_conc %>% 
  mutate(region=f_split_n(archivo,"/",3) %>% factor(levels = levels_region),
         provincia=f_split_n(archivo,"/",4) %>% factor(),
         site=f_split_n(archivo,"/",5) %>% str_remove_all("_|mp2.5|\\.csv"))

# Estacion la florida en Talca
df_conc <- df_conc %>% 
  mutate(estacion=if_else(site=="LaFlorida" & region=="VII",
                          "LaFloridaTalca",site) %>% factor())

# Cruze con estaciones
df_estaciones <- df_estaciones %>% mutate(site=str_remove_all(estacion," |'"))

df_conc <- left_join(df_conc, 
                     df_estaciones %>% select(-region,-estacion), 
                     by=c("site","provincia"))


## Resumen general datos  --------
range(df_conc$year) #rango años
df_conc$estacion %>% unique() %>% length() #Numero archivos
# df_conc %>% skim()


#### VISUALIZACIONES ------------

## Disponibilidad de datos --------
# heatmap 
ggplot(df_conc, aes(x = year, y = reorder(estacion,disponibilidad), 
                    fill = disponibilidad)) + 
  geom_tile() + 
  scale_fill_viridis_c(option = "B",direction = -1, 
                       name="Disponibilidad \n información") + 
  facet_grid(region~., scales = "free", space="free")+
  scale_y_discrete(name = NULL) +
  scale_x_continuous(name = "", limits = c(NA, 2021))
ggsave("Visualizaciones MP2.5 Nacional/Disponibilidad.png", last_plot(),dpi=600,
       width = 17.84, height = 11.16, units = "in")

# ecdf
ggplot(df_conc, aes(x=disponibilidad))+
  stat_ecdf()+scale_x_continuous(name="Disponibilidad información")
ggplot(df_conc, aes(x=disponibilidad))+
  stat_ecdf()+facet_wrap(~year)



## Promedio anual 2019 -----------
p1 <- df_conc %>% 
  filter(year==2019 & disponibilidad>0.8) %>% 
  ggplot(aes(x=reorder(estacion, valor), y=valor, fill=region)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  coord_flip(clip="off")+
  scale_fill_viridis_d()+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio Anual MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
p1

## Promedio anual 2019  superacion norma-----------
df_conc %>% 
  filter(year==2019 & disponibilidad>0.8 & valor>20) %>% 
  ggplot(aes(x=reorder(estacion, valor), y=valor, fill=region)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  coord_flip(clip="off")+
  scale_fill_viridis_d()+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio Anual MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

## Promedio anual 2019-2017  superacion norma-----------
df_conc %>% 
  filter(year>2016 & disponibilidad>0.8) %>% 
  group_by(estacion,region) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  filter(valor>20 & valor<150) %>% 
  ggplot(aes(x=reorder(estacion, valor), y=valor, fill=region)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  coord_flip(clip="off")+
  scale_fill_viridis_d()+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio Anual MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())


## Promedio 2017-2019: Norte a Sur -----------
df_conc %>% 
  filter(year>2016 & disponibilidad>0.8) %>% 
  group_by(estacion,region, latitud) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  filter(valor<150) %>%
  mutate(highlight=if_else(valor>20,"yes","no")) %>% 
  ggplot(aes(x=reorder(estacion, latitud), y=valor, fill=highlight)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  coord_flip(clip="off")+
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio trianual 2017-2019 MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("Visualizaciones MP2.5 Nacional/EstacionesChile.png", last_plot(),dpi=600,
       width = 10.62, height = 6.64, units = "in")


## Heatmap Promedio Anual ---------
df_conc %>% 
  filter(disponibilidad>0.8 & year>=2010 & valor<150) %>% 
  ggplot(aes(x = year, y = reorder(estacion, latitud), fill = valor)) + 
  geom_tile() + 
  facet_grid(region~., scales = "free", space="free")+
  scale_fill_viridis_c(option = "B", direction = -1, 
                       name="Promedio Anual MP2.5 [ug/m3]") + 
  scale_y_discrete(name = NULL) +
  scale_x_continuous(name="", limits = c(2010,2020), breaks = c(2010,2013,2016, 2019))

ggsave("Visualizaciones MP2.5 Nacional/HeatMap.png", last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")

## Promedio trianual ordenado por region ----------
df_conc %>% 
  filter(year>2016 & disponibilidad>0.8) %>% 
  group_by(estacion,region, latitud) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% 
  filter(valor<150) %>%
  mutate(highlight=if_else(valor>20,"yes","no")) %>% 
  ggplot(aes(x=reorder(estacion, valor), y=valor, fill=highlight)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  facet_grid(region~., scales = "free", space="free")+
  coord_flip(clip="off")+
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name="Promedio trianual 2017-2019 MP2.5 [ug/m3]",
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

ggsave("Visualizaciones MP2.5 Nacional/EstacionesChileRegion.png", 
       last_plot(),dpi=600,
       width = 14.87, height = 9.30, units = "in")


## Spatial ------------
library(chilemapas)
# Saco del mapa a Isla de Pascua y Juan Fernandez
regiones <- generar_regiones(mapa_comunas %>% filter(codigo_comuna!="05201" &
                                                       codigo_comuna!="05104"))
regiones$codigo_region %>% unique()
regiones <- regiones %>% mutate(
  region=case_when(
    codigo_region=="01" ~ "I",
    codigo_region=="02" ~ "II",
    codigo_region=="03" ~ "III",
    codigo_region=="04" ~ "IV",
    codigo_region=="05" ~ "V",
    codigo_region=="06" ~ "VI",
    codigo_region=="07" ~ "VII",
    codigo_region=="08" ~ "VIII",
    codigo_region=="09" ~ "IX",
    codigo_region=="10" ~ "X",
    codigo_region=="11" ~ "XI",
    codigo_region=="12" ~ "XII",
    codigo_region=="13" ~ "XIII",
    codigo_region=="14" ~ "XIV",
    codigo_region=="15" ~ "XV",
    codigo_region=="16" ~ "VIII",
    T ~ "otro") %>% factor(levels = levels_region))


df_conc_region <- df_conc %>% 
  filter(year==2019 & disponibilidad>0.8) %>% 
  left_join(regiones)

m <- ggplot(df_conc_region) +
  geom_sf(aes(fill=region, geometry=geometry))+
  theme(legend.position = "none")+
  labs(x="", y="") +
  coord_sf(datum = NA, expand = FALSE)

p1+theme(legend.position = "none")+annotation_custom(ggplotGrob(m))

## Point -----
ggplot(df_conc_region) +
  geom_sf(aes(geometry=geometry), fill="white")+
  geom_point(aes(x=longitud, y=latitud, col=valor))+
  scale_color_viridis_c(option="B", direction = -1, 
                        name="Promedio Anual MP2.5 2019 [ug/m3]")+
  labs(x="", y="") +
  coord_sf(datum = NA, expand = FALSE)

ggsave("Visualizaciones MP2.5 Nacional/MapaChileMP25.png", last_plot(),dpi=600,
       width = 10.62, height = 6.64, units = "in")
