library(shiny)
library(here)
library(readr)
library(tidyverse)
library(ggpmisc)

ui <- fluidPage(
  
  plotOutput("grafico",
             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
  uiOutput("hover_info")
  
)

server <- function(input, output, session) {
  compras <- read_csv(here("Csv", "comprasEstatalesrefactor.csv")) %>%
    separate(fecha_compra, c("dia", "mes", "año")) 
  compras$año <- recode(compras$año, "2018"="18", "2019"="19")
  compras <- compras %>% filter(!is.na(año) & año%in%c(18, 19))
  compras$fecha <- paste(compras$mes, compras$año, sep="/")
  compras_agrupadas <- compras %>% group_by(fecha, mes, año) %>%
    summarise(monto_total=sum(monto_adj)) 
  
  output$grafico <- renderPlot({
    
      ggplot(compras_agrupadas, aes(x=fecha, y=monto_total)) +
      geom_point() +
      geom_line(group=1) +
      stat_peaks(geom="point", color="red") +
      stat_peaks(geom="text", colour="red", vjust=-.5) +
      labs(x="Fecha (mes y año)", y="Monto total de las compras del mes")
    })

  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(compras_agrupadas, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    gastosmayores<-compras %>% filter(año==point$año, mes==point$mes) %>%
      arrange(desc(monto_adj)) %>% select(objeto)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Compras con mayor monto del mes: </b>", "<br/>",
                    "<b> </b>", gastosmayores[1,] , "<br/>",
                    "<b> </b>", gastosmayores[2,] , "<br/>",
                    "<b> </b>", gastosmayores[3,] , "<br/>",
                    "<b> </b>", gastosmayores[4,] , "<br/>",
                    "<b> </b>", gastosmayores[5,] , "<br/>",
                    "<b> </b>", gastosmayores[6,] , "<br/>",
                    "<b> </b>", gastosmayores[7,] , "<br/>",
                    "<b> </b>", gastosmayores[8,] , "<br/>",
                    "<b> </b>", gastosmayores[9,] , "<br/>",
                    "<b> </b>", gastosmayores[10,] , "<br/>"
                    )))
    )
  })    

}

shinyApp(ui, server)


#El dataframe tiene información de compras de muchos años, siendo 2004 (creo) el más viejo.
#Para esta visualización utilicé solamente datos de 2018 y 2019.

#No pude elegir el órden de los valores en el eje x (al ser un vector character).
#No logré hacerlo ordenando el dataframe (con arrange), ni poniendo dentro del aes
#fct_reorder(fecha, año, mes) en el aesthetic x.
#Podría hacerlo manualmente pero no es la idea.

#Tiene sentido graficar en el eje y el monto total gastado (la suma?).
#Porque en algunos meses tenemos más observaciones que en otros.
#No tendría más sentido hacer algo así como un promedio?
#Porque no sé si la base está completa, y contiene la info de todas las compras estatales
#realizadas, o solo algunas.

#La función stat_peaks() no está haciendo nada, por algún motivo.

#En algunas compras el valor de la variable objeto es NA, hacer filter(!is.na(objeto))
#no es una buena solución porque estamos perdiendo info.