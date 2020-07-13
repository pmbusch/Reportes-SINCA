### Proyecto SINCA
## Seasonal Trend Decomposition
## PBH Jul 2020
## Ultima atualizacion: PBH Jul 2020

## DESCOMPOSICION -------------

# Funcion para generar grafico de STL. Recibe datos mensuales (df_mes) y la estacion
f_stl_plot <- function(datos, estacion){
  
# use complete sites only and month data
  df_dec <- datos %>% filter(site==estacion) %>% ungroup()
  
# Debe tener todos los datos mensuales y mas de 24 meses
  date_start <- min(df_dec$date)
  date_end <- max(df_dec$date)
  n_meses <- 12*(year(date_end)-year(date_start))+month(date_end)-month(date_start)+1
  
  if (nrow(df_dec)==n_meses & n_meses>=24){
    
    # convert to time series object
    df_ts <- ts(data = df_dec$valor, 
                start = c(year(date_start), month(date_start)), 
                end = c(year(date_end), month(date_end)), 
                frequency = 12)
    
    # detrend via STL method
    df_stl <- stl(df_ts, s.window = 7)
    df_detrended <- df_dec %>% 
      mutate(`monthly average`= valor,
             `seasonal fluctuations`= t(df_stl$time.series)[1, ],
             `long-term trend`= t(df_stl$time.series)[2, ],
             remainder = t(df_stl$time.series)[3, ])
    
    facet_labels <- c("monthly average", "long-term trend", "seasonal fluctuations", "remainder")
    
    df_detrended %>%
      select(date, `monthly average`, `seasonal fluctuations`, `long-term trend`, remainder) %>%
      gather(variable, value, -date) %>%
      mutate(variable = factor(variable, levels = facet_labels)) %>%
      filter(date >= 2014) %>%
      ggplot(aes(date, value)) +
      geom_line(color = "#0072B2", size = 0.6) +
      facet_grid(variable~., scales="free")+
      scale_y_continuous(name = label_conc)+
      scale_x_date(name = NULL, date_labels = "%Y", date_breaks = "1 year")+
      labs(caption = "Notar que cada gráfico tiene su propia escala en el eje Y")
  } 
  else {
    return("Estacion no tiene datos en todos los meses o menos de dos años de datos")
  }
}

## Prueba
# f_stl_plot(df_mes, "Puente Alto")

## EoF