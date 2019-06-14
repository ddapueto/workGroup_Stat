
library(shiny)
library(shinythemes)
library(here)
library(tidyverse)

# generate data
path <- here::here()
path_oferantes <- paste(path,"/Data/Csv/metadataOferantes.csv", sep="")
path_adj <- paste(path,"/Data/Csv/metadataAdjudicaciones.csv", sep="")
path_comp <- paste(path,"/Data/Csv/metadataCompras.csv", sep="")
oferantes <- read_csv(path_oferantes)
adjudicaciones <- read_csv(path_adj)
compras <- read_csv(path_comp)

compList <- c(
  #"apel", "es_reiteracion", 
  "estado_compra", 
  #"fondos_rotatorios",
  "id_inciso","id_moneda","id_tipo_resol","id_tipocompra","id_ue",
  "subtipo_compra",
  "id_ucc"
  )
adjList <- c("id_moneda", "id_unidad","tipo_doc_prov")

ofeList <- c("tipo_doc_prov")

# create Ui 
ui <- tagList(
  shinythemes::themeSelector(),
  navbarPage(
    "Metadata - Compras Estatales",
    tabPanel("Navbar 1",
             sidebarPanel(
               radioButtons("metadata", "Metadata segun datasets: ",
                            c("Compras Estatales" = "compras" ,
                              "Adjudicaciones" = "adjudicaciones",
                              "Oferentes" = "oferantes"), 
                            selected = 'compras'),
               conditionalPanel(
                 condition = "input.metadata == 'compras'",
                 selectInput("cod" , "Codigos de Atributos en Compras",
                             compList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'adjudicaciones'",
                 selectInput("cod" , "Codigos de Atributos en Adjudicaciones",
                             adjList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'oferantes'",
                 selectInput("cod" , "Codigos de Atributos en Oferentes",
                             ofeList)
               ),
               actionButton("controller", "Codigueras Existentes",icon("paper-plane"), 
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
    
    codTable <- readRDS(paste(path,"/Data/rds/meta_",input$cod,".rds", sep=""))
    
    codTable %>% 
      kableExtra::kable() %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = TRUE)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
