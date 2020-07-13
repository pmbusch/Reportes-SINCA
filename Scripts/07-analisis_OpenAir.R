### Proyecto SINCA
## Rutinas para probar graficos con Open Air. Los originales se imprimen directamente el doc
## PBH Octubre 2018
## Ultima atualizacion: PBH Jul 2020

cont <- "mp2.5"

## SUMMARY PLOT --------------
summaryPlot(df_openAir, pollutant= cont, date.breaks = 6,xlab='Fecha')


## TIME PLOT --------------
df_openAir %>% timePlot(type="site", pollutant=cont,
                                     date.breaks = 4,avg.time = 'day')


## SMOOTH TREND --------------
df_openAir %>% smoothTrend(pollutant =cont,hemisphere='southern',
                                           avg.time='month', xlab='Año', type="site")

# # Por season (VER BIEN COMO DIVIDIRLO POR SITE!)
# df_openAir %>% smoothTrend(pollutant =cont,type='season',
#                            hemisphere='southern',avg.time='month', xlab='Año')
# 
# # por daylight (VER BIEN COMO DIVIDIRLO POR SITE!)
# df_openAir %>% smoothTrend(pollutant =cont,type='daylight',
#                            hemisphere='southern',avg.time='month', xlab='Año')

## TIME VARIATION --------------
df_openAir %>% timeVariation(pollutant = cont, hemisphere='southern', group = "site")


# time_variation<- df_openAir %>% timeVariation(pollutant = cont, key=T, type ="season",
#                                               hemisphere='southern', group = "site")
# print(time_variation, split = c(1, 1, 1, 1), subset = "hour", newpage = T) # acceder al objeto por separado



## CALENDAR PLOT --------
df_openAir %>% filter(site=="Valdivia" & year==2019) %>%
  calendarPlot(pollutant = cont, hemisphere='southern')

## TRENDLEVEL (parecido a HeatMap)  --------
df_openAir %>% filter(site=="Valdivia") %>%
  trendLevel(pollutant = cont, hemisphere='southern')


df_openAir %>%
  trendLevel(pollutant = cont, hemisphere='southern', group="site")


## GOOGLEMAPS PLOT   --------
## TO DO

# ## WIND ROSE ----------------
# Solo si cargo datos meteorologicos

# df_openAir %>% windRose(.,ws='ws','wd')
# df_openAir %>% windRose(.,ws='ws','wd',type='daylight')
# df_openAir %>% windRose(.,ws='ws','wd',type=c('cuartilHora'))

# 
# a <- pollutionRose(df_openAir,pollutant = 'MP10',wd='wd')
# b <- pollutionRose(df_openAir,pollutant = 'MP2.5',wd='wd')
# print(a, split = c(1, 1, 2, 1))
# print(b, split = c(2, 1, 2, 1), newpage = FALSE)
# 
# 
# a <- df_openAir %>% polarPlot(.,wd="wd",x='ws',pollutant = 'MP10',units='m/s')
# b <- df_openAir %>% polarPlot(.,wd="wd",x='ws',pollutant = 'MP2.5',units='m/s')
# print(a, split = c(1, 1, 2, 1))
# print(b, split = c(2, 1, 2, 1), newpage = FALSE)
