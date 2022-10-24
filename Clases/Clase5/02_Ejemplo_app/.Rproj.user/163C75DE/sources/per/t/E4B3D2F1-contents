server <- function(input, output) { 
   ## cree un objeto reactivo, que contenga el dataset *vuelos* 
   #filtrado para el aeropuerto de destino seleccionado
  
  aux <- reactive({
    temp <- vuelos %>%
      mutate(input_dest = input$destino ) %>%
      mutate(col_filt = if_else(input_dest == "ALL",dest,input_dest)) %>%
      filter(dest == col_filt)
    return(temp)
  })
  
  aux_mapa <- reactive({ 
    temp <- aux() %>%
      #DEJAR ESTO COMO EJERCICIO
      left_join(aeropuertos, by = c("dest" = "faa") ) %>%
      dplyr::select(dest,
                    name,
                    lon,
                    lat,
                    arr_delay) %>% 
      group_by(name,dest) %>% 
      summarise(  lat = mean(lat,na.rm=T)
                  , lon = mean(lon,na.rm=T)
                  , n_vuelos = n()
                  , value = round(mean(arr_delay,na.rm = T),2)
      ) %>% ungroup() %>%
      mutate( color = colorize(value),
              z = n_vuelos) %>% 
      # DEJAR ESTO COMO EJERCICIO
      na.omit()
    return(temp)
  })
  

# outputs -----------------------------------------------------------------

# infoBox arr_del_prom ----------------------------------------------------
  
  output$arr_del_prom <- renderInfoBox({
    infoBox(
      title = "Retraso medio de llegada",
      value = round(mean(aux()$arr_delay,na.rm=T),2),
      icon  = icon("business-time"),
      width = 4,
      color = "olive"
    )
  })
  # infoBox dep_del_prom ----------------------------------------------------
  
  output$dep_del_prom <- renderInfoBox({
    infoBox(
      title = "Retraso medio de salida",
      value = round(mean(aux()$dep_delay,na.rm=T),2),
      icon  = icon("stopwatch"),
      width = 4,
      color = "light-blue"
    )
  })
  

# mapa --------------------------------------------------------------------

  output$mapa <- renderHighchart({
    hcmap("countries/us/us-all", showInLegend = F) %>% 
      hc_add_series(data = aux_mapa(),
                    type = "mapbubble",# si se quiere utilizar mapbubble se debe incluir variable z en dataset para dar el tamaño
                    name = "Aeropuerto",
                    dataLabels = list(enabled = F),
                    # minSize = 5, maxSize = 30,
                    tooltip = list(pointFormat = "{point.name}<br>Vuelos:{point.n_vuelos}<br>Retraseo medio (arr):{point.value}</b><br/>")
      ) %>%
      hc_mapNavigation(enabled = T) %>%
      hc_plotOptions(series = list(showInLegend = F)) %>% 
      hc_title(text = "Distribución de retrasos por aeropuerto de destino")
    
  })
  

# gráfico de barras stacked -----------------------------------------------

  output$stacked <- renderHighchart({
    
    vuelos_ori_des <- aux() %>% 
      pivot_wider(
        id_cols = c("origin","dest","arr_delay"),
        names_from = origin,
        values_from = arr_delay,
        values_fn = list(arr_delay = length),
        values_fill = list(arr_delay = 0)
        
      ) 
    # genere un dataset llamado vuelos_ori, donde cada fila represente 
    # un aeropuerto de destino, y en cada columna se resistre el conteo 
    # de vuelos proveniente de los distintos aeropuertos de origen.
    # Mostrar ejemplo y nombres del dataset.
    
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = vuelos_ori_des$dest) %>%
      hc_add_series(name="Newark Liberty Intl",
                    data = vuelos_ori_des$EWR,
                    stack = "destino") %>%
      hc_add_series(name="La Guardia",
                    data = vuelos_ori_des$LGA,
                    stack = "destino") %>%
      hc_add_series(name="John F Kennedy Intl",
                    data = vuelos_ori_des$JFK,
                    stack = "destino") %>%
      hc_title(text="Cantidad de vuelos por origen/destino")
  })
  

# Series  -----------------------------------------------------------------

  output$series_ts <- renderHighchart({
    df_ts <- vuelos %>%
      mutate(input_ori = input$origen ) %>%
      mutate(col_filt_ori = if_else(input_ori == "ALL",origin,input_ori)) %>%
      filter(origin == col_filt_ori) %>% 
      left_join(clima, by = c("origin","time_hour") ) %>%
      mutate(time_hour2 = as.character(as.Date(time_hour)))%>% 
      group_by(origin,time_hour2) %>%
      summarise(mean_dep_delay = mean(dep_delay,na.rm = T)
                # ,mean_visib = mean(visib,na.rm=TRUE)
                ,mean_wind_gust =  mean(wind_gust,na.rm=TRUE))
   
    
    # Utilice el código de la sesión anterior, para generar un gáfico de tipo stock con las series requeridas
    df_ts <- df_ts %>% 
      pivot_longer(
        cols = c("mean_dep_delay" , "mean_wind_gust") , # columnas que se quieren despivotear.
        names_to = "serie", #nombres de las columnas, se van como categorías a una nueva variable (origen)
        values_to = "valor", # los valores a rescatar se van a una nueva variable (cantidad_vuelos)
        values_drop_na = TRUE
      ) 
    
    df_ts %>%
      mutate(time_hour2 = as.Date(time_hour2)) %>%
      hchart(type = "line",
             hcaes(y = valor,
                   group = serie, x = time_hour2)
             )
    
         
    
  })
  

# correlación -------------------------------------------------------------

output$correlacion <- renderHighchart({
  df_ts <- vuelos %>%
    mutate(input_ori = input$origen ) %>%
    mutate(col_filt_ori = if_else(input_ori == "ALL",origin,input_ori)) %>%
    filter(origin == col_filt_ori) %>% 
    left_join(clima, by = c("origin","time_hour") ) %>%
    mutate(time_hour2 = as.character(as.Date(time_hour))) %>%
    select(dep_delay,temp,dewp,humid,wind_dir,wind_speed,
           wind_gust,precip,pressure,visib) %>%
    na.omit()
  
  hchart(cor(df_ts)) 
    
  
  
  
})  
  
  }