library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  # Header ------------------------------------------------------------------
  
  dashboardHeader(title = "Reporte vuelos NY"),
  ## FIN HEADER
  
  
  # Sidebar -----------------------------------------------------------------
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("General", tabName = "general", icon = icon("dashboard")),
      menuItem("Series", tabName = "series", icon = icon("th"))
    ),
    ## Incluya un selector de texto de tipo *selectInput* que muestre las opciones de aeropuerto de destino
    selectInput(inputId = "destino"
                , label = "Aeroppuerto de destino"
                , choices = c("ALL",unique(as.character(vuelos$dest)))
                , selected =  "ALL"
    )
  ),
  ## FIN SIDEBAR
  
  
  # Body --------------------------------------------------------------------
  
  dashboardBody(
    # las cajas (box) deben declararse dentro de filas ()
    tabItems(
      # First tab content
      tabItem(tabName = "general",
    fluidRow(
      ## Dentro de esta fila incluya dos cajas de tipo *infoBox*,
      # Que muestren el retraso promedio de salida y retraso promedio de llegada.
      infoBoxOutput("arr_del_prom",width = 6),
      infoBoxOutput("dep_del_prom",width = 6)
    ),
    fluidRow(
      column(6, highchartOutput("mapa")),
      column(6, highchartOutput("stacked")
      )
    )
    )#end tab
    ,
      tabItem(tabName = "series",
    fluidRow(
      # incluya arriba del gráfico de series un selector de tipo selectInput cuyas opciones sean los **nombres de los aeropuertos de origen**
      column(12,selectInput(inputId = "origen"
                            , label = "Aeroppuerto de origen"
                            , choices = c("Newark Liberty Intl" = "EWR", "La Guardia" = "LGA", "John F Kennedy Intl" = "JFK")
                            , selected =  "LGA"),
             highchartOutput("series_ts")),
      column(12,highchartOutput("correlacion"))
    )
    )
    )
  )
  ## FIN BODY
)


