
library(shiny)
library(shinythemes)
library(here)
library(tidyverse)
library(lubridate)
library(DT)


# generate data
path <- here::here()
path_oferentes <- paste(path,"/Data/Csv/metadataOferentes.csv", sep = "")
path_adj <- paste(path,"/Data/Csv/metadataAdjudicaciones.csv", sep = "")
path_comp <- paste(path,"/Data/Csv/metadataCompras.csv", sep = "")
path_base <- paste(path,"/Data/rds/compras.rds", sep="")
oferentes <- read_csv(path_oferentes)
adjudicaciones <- read_csv(path_adj)
compras <- read_csv(path_comp)
base_compras <- readr::read_rds(path_base)



compList <- c(
  #"apel", "es_reiteracion", 
  "estado_compra", 
  #"fondos_rotatorios",
  "id_inciso","id_moneda","id_tipo_resol","id_tipocompra","id_ue", "subtipo_compra", "id_ucc"
)
adjList <- c("id_moneda", "id_unidad", "tipo_doc_prov")

ofeList <- c("tipo_doc_prov")

incisoList <- rbind(c("No filtrar"), levels(as.factor(base_compras$inciso)))

monedaList <- rbind(c("No filtrar"), levels(as.factor(base_compras$pesos_uruguayo)))


# create Ui 
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "Compras Estatales",
    tabPanel("Metadata",
             sidebarPanel(
               radioButtons("metadata", "Metadata segun datasets: ",
                            c("Compras Estatales" = "compras" ,
                              "Adjudicaciones" = "adjudicaciones",
                              "Oferentes" = "oferentes"), 
                            selected = 'compras'),
               conditionalPanel(
                 condition = "input.metadata == 'compras'",
                 selectInput("cod" , "Codigos de Atributos en Compras", compList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'adjudicaciones'",
                 selectInput("cod" , "Codigos de Atributos en Adjudicaciones",
                             adjList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'oferentes'",
                 selectInput("cod" , "Codigos de Atributos en Oferentes",
                             ofeList)
               ),
               actionButton("controller", "Codigueras Existentes", icon("paper-plane"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ),
             mainPanel(
               tabsetPanel(id = "inTabset",
                           tabPanel("Metadata",
                                    # h4("Metadatos"),
                                    tableOutput("table")
                           ),
                           tabPanel(title = "Codigueras", value = "codiguera", tableOutput("codigueTable"))
               )
             )
    ),
    tabPanel("Análisis Exploratorio",
             sidebarPanel(
               radioButtons("visualizacion", "Gráfica a visualizar: ",
                            c("Serie de tiempo de monto total adjudicado por semana" = "monto_adjj" ,
                              "Serie de tiempo de cantidad de adjudicaciones por semana" = "cant_adjj"), 
                            selected = 'monto_adjj'
               ), h4("Filtros a aplicar a las visualizaciones:"),
               selectInput("filtro_inciso", "Inciso:", incisoList, selected = "Administración de Servicios de Salud del Estado"),
               selectInput("filtro_moneda", "Moneda:", monedaList, selected = "pesos_uruguayo")
             ),
             mainPanel(
               h3(textOutput("title_graph") , align="center"),
               plotOutput("monto_adj"),
               h3("Tabla de Datos de la visualizacion."),
               DT::dataTableOutput("mytable")
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$controller, {
    updateTabsetPanel(session, "inTabset",
                      selected = "codiguera"
    )
  })
  
  output$table <- function() {
    get(input$metadata) %>% 
      kableExtra::kable() %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = TRUE)
  }
  
  output$codigueTable <- function() {
    
    codTable <- readr::read_rds(paste(path, "/Data/rds/meta_", input$cod, ".rds", sep = ""))
    names(codTable) <- str_to_title(str_replace_all(string = names(codTable), pattern = "_", replacement = " "))
    
    codTable %>% 
      kableExtra::kable() %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = TRUE, full_width = FALSE)
  }
  
  da <- reactive({
    if ((input$filtro_inciso) != "No filtrar") {
      base_compras %>% filter(inciso == input$filtro_inciso)
    }else{base_compras}
  })
  
  dam <- reactive({
    if ((input$filtro_moneda) != "No filtrar") {
      da() %>% filter(pesos_uruguayo == input$filtro_moneda) %>% filter("2018-01-01" < fecha_compra, fecha_compra < "2019-04-30", monto_adj > 0) %>%
        group_by(floor_date(fecha_compra, "week")) %>% 
        summarise(total_comprado = sum(monto_adj_pesos, na.rm = TRUE), cantidad_comprado = n(),monto_promedio_por_semana = total_comprado/cantidad_comprado) %>%
        rename(semana = `floor_date(fecha_compra, "week")`)
    }else{base_compras %>% filter("2018-01-01" < fecha_compra, fecha_compra < "2019-04-30", monto_adj > 0) %>%
        group_by(floor_date(fecha_compra, "week")) %>% 
        summarise(total_comprado = sum(monto_adj_pesos, na.rm = TRUE), cantidad_comprado = n(),monto_promedio_por_semana = total_comprado/cantidad_comprado) %>%
        rename(semana = `floor_date(fecha_compra, "week")`)}
  })
  
  output$title_graph <- renderText({
    if (input$visualizacion == 'monto_adjj'){
      paste("Serie de tiempo de monto adjudicado por semana para ", input$filtro_inciso , "en ", input$filtro_moneda)
    }
    else{
      paste("Serie de tiempo de cantidad de adjudicaciones por semana ", input$filtro_inciso , "en ", input$filtro_moneda)
    }
    
  })
  
  output$monto_adj <- renderPlot({
    
    #genracion dataframe
    
    df <- dam()  %>% 
      mutate(is_low_monto = ifelse(total_comprado <= quantile(total_comprado,probs=0.05), 1, NA), 
             is_high_monto = ifelse(total_comprado >= quantile(total_comprado,probs=0.95), 1, NA), 
             is_outlier_monto = case_when(
               total_comprado < quantile(total_comprado,probs = 0.25) - 1.5 * IQR(total_comprado) | total_comprado > quantile(total_comprado, probs = 0.75) + 1.5 * IQR(total_comprado) ~ TRUE,
               TRUE ~ FALSE) 
      ) %>%
      mutate(is_low_cto = ifelse(cantidad_comprado <= quantile(cantidad_comprado,probs=0.05), 1, NA), 
             is_high_cto = ifelse(cantidad_comprado >= quantile(cantidad_comprado,probs=0.95), 1, NA), 
             is_outlier_cto = case_when(
               cantidad_comprado < quantile(cantidad_comprado,probs = 0.25) - 1.5 * IQR(cantidad_comprado) | cantidad_comprado > quantile(cantidad_comprado, probs = 0.75) + 1.5 * IQR(cantidad_comprado) ~ TRUE,
               TRUE ~ FALSE) 
      ) %>%
      mutate(is_low_mean = ifelse(monto_promedio_por_semana <= quantile(monto_promedio_por_semana,probs=0.05), 1, NA), 
             is_high_monto = ifelse(monto_promedio_por_semana >= quantile(monto_promedio_por_semana,probs=0.95), 1, NA), 
             is_outlier_monto = case_when(
               monto_promedio_por_semana < quantile(monto_promedio_por_semana,probs = 0.25) - 1.5 * IQR(monto_promedio_por_semana) | monto_promedio_por_semana > quantile(monto_promedio_por_semana, probs = 0.75) + 1.5 * IQR(monto_promedio_por_semana) ~ TRUE,
               TRUE ~ FALSE) 
      )
    
    if (input$visualizacion == 'monto_adjj'){
      ggplot(df) +
        geom_line(aes(semana, total_comprado), color = "purple") +
        labs(x = NULL, y = "Monto total adjudicado por semana\n(equivalente en millones de pesos corrientes)\n") +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
        ggthemes::theme_economist() +
        theme(axis.title = element_text(face = "bold"))+
        geom_point(data = df %>% dplyr::filter(is_outlier_monto == TRUE), aes(semana, total_comprado, colour = 'Anomaly'), size = 5, alpha = 0.5)+
        geom_point(data = df %>% dplyr::filter(is_high_monto == 1), aes(semana, total_comprado, colour = 'Percentile_95'), size = 2) + 
        scale_colour_manual(values=c( Percentil_5="blue", Percentile_95="red", Anomaly ="darkred" ))
    } else{
      ggplot(df) +
        geom_line(aes(semana, cantidad_comprado), color = "orange") +
        labs(x = NULL, y = "Cantidad de adjudicaciones mensuales\n") +
        scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
        ggthemes::theme_economist() +
        theme(axis.title = element_text(face = "bold"))+
        geom_point(data = df %>% dplyr::filter(is_outlier_cto == TRUE), aes(semana, cantidad_comprado, colour = 'Anomaly'), size = 5, alpha = 0.5)+
        geom_point(data = df %>% dplyr::filter(is_low_cto == 1), aes(semana, cantidad_comprado, colour = 'Percentil_5'), size = 2) +
        geom_point(data = df %>% dplyr::filter(is_high_cto == 1), aes(semana, cantidad_comprado, colour = 'Percentile_95'), size = 2) + 
        scale_colour_manual(values=c( Percentil_5="blue", Percentile_95="red", Anomaly ="darkred" ))
    }
    
  })
  
  output$mytable = DT::renderDataTable({
    dam()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)