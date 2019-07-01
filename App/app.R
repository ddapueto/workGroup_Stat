
library(shiny)
library(shinythemes)
library(here)
library(tidyverse)
library(lubridate)


# generate data
path <- here::here()
path_oferentes <- paste(path,"/Data/Csv/metadataOferentes.csv", sep = "")
path_adj <- paste(path,"/Data/Csv/metadataAdjudicaciones.csv", sep = "")
path_comp <- paste(path,"/Data/Csv/metadataCompras.csv", sep = "")
oferentes <- read_csv(path_oferentes)
adjudicaciones <- read_csv(path_adj)
compras <- read_csv(path_comp)
base_compras <- readr::read_rds("compras.rds")



compList <- c(
  #"apel", "es_reiteracion", 
  "estado_compra", 
  #"fondos_rotatorios",
  "id_inciso","id_moneda","id_tipo_resol","id_tipocompra","id_ue", "subtipo_compra", "id_ucc"
)
adjList <- c("id_moneda", "id_unidad", "tipo_doc_prov")

ofeList <- c("tipo_doc_prov")

incisoList <- rbind(c("No filtrar"), levels(as.factor(base_compras$inciso)))

monedaList <- rbind(c("No filtrar"), levels(as.factor(base_compras$moneda)))

estcompraList <- rbind(c("No filtrar"), levels(as.factor(base_compras$tipo_compra)))


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
                            selected = 'monto_adj'
               ), h4("Filtros a aplicar a las visualizaciones:"),
               selectInput("filtro_inciso", "Inciso:", incisoList, selected = "No filtrar"),
               selectInput("filtro_moneda", "Moneda:", monedaList, selected = "No filtrar"),
               selectInput("filtro_tipo_compra", "Tipo de Compra:", estcompraList, selected = "No filtrar")
             ),
             mainPanel(
               h3("Serie de tiempo de monto adjudicado por semana."), align="center",
               plotOutput("monto_adj"),
               h3("Serie de tiempo de cantidad de adjudicaciones por semana.", align="center"),
               plotOutput("cant_adj")
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
  
  datos <- base_compras
  reactive(if (!!sym(input$filtro_inciso) != "No filtrar") {datos <- datos %>% filter(inciso == !!sym(input$filtro_inciso))}
  )
  reactive(if (!!sym(input$filtro_moneda) != "No filtrar") {datos <- datos %>% filter(inciso == !!sym(input$filtro_moneda))}
  )
  reactive(if (!!sym(input$filtro_tipo_compra) != "No filtrar") {datos <- datos %>% filter(inciso == !!sym(input$filtro_tipo_compra))}
  )
  
  
  da <- reactive({
    if ((input$filtro_inciso) != "No filtrar") {
      base_compras %>% filter(inciso == input$filtro_inciso)
    }else{base_compras}
  })
  
  da_moneda <- reactive({
    if(input$filtro_moneda != "No filtrar"){
      da() %>% filter(moneda == input$filtro_moneda)
    }else{base_compras}
  })
  
  da_tipo <- reactive({
    if(input$filtro_tipo_compra != "No filtrar"){
      da_moneda() %>% filter(tipo_compra ==(input$filtro_tipo_compra))
    }else{base_compras}
  })
  
  output$monto_adj <- renderPlot({
    da_tipo() %>% 
      filter("2018-01-01" < fecha_compra, fecha_compra < "2018-12-31", monto_adj > 0) %>%
      group_by(floor_date(fecha_compra, "week")) %>% 
      summarise(total_comprado = sum(monto_adj_pesos, na.rm = TRUE)) %>% 
      ggplot() +
      geom_line(aes(`floor_date(fecha_compra, "week")`, total_comprado), color = "purple") +
      labs(x = NULL, y = "Monto total adjudicado por semana\n(equivalente en millones de pesos corrientes)\n") +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
      ggthemes::theme_economist() +
      theme(axis.title = element_text(face = "bold"))
  })
  
  output$cant_adj <- renderPlot({
    da_tipo() %>% 
      filter("2018-01-01" < fecha_compra, fecha_compra < "2018-12-30", monto_adj > 0) %>%
      group_by(floor_date(fecha_compra, "week")) %>% 
      tally() %>% 
      ggplot() +
      geom_line(aes(`floor_date(fecha_compra, "week")`, n), color = "orange") +
      labs(x = NULL, y = "Cantidad de adjudicaciones mensuales\n") +
      scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
      ggthemes::theme_economist() +
      theme(axis.title = element_text(face = "bold"))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)