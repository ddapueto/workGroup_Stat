
library(shiny)
library(shinythemes)
library(here)
library(tidyverse)

# generate data
path <- here::here()
path_oferantes <- paste(path,"/Csv/metadataOferantes.csv", sep="")
path_adj <- paste(path,"/Csv/metadataAdjudicaciones.csv", sep="")
path_comp <- paste(path,"/Csv/metadataCompras.csv", sep="")
oferantes <- read_csv(path_oferantes)
adjudicaciones <- read_csv(path_adj)
compras <- read_csv(path_comp)

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
                 selectInput("codCompras" , "Codigos de Atributos en Compras",
                             unique(unlist(compras[c("Atributo")])))
               ),
               conditionalPanel(
                 condition = "input.metadata == 'adj'",
                 selectInput("codAdj" , "Codigos de Atributos en Adjudicaciones",
                             unique(unlist(adjudicaciones[c("Atributo")])))
               ),
               conditionalPanel(
                 condition = "input.metadata == 'ofe'",
                 selectInput("codOfe" , "Codigos de Atributos en Oferentes",
                             unique(unlist(oferantes[c("Atributo")])))
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Metadata",
                          # h4("Metadatos"),
                          tableOutput("table")
                          
                 ),
                 tabPanel("Codigueras", "This panel is intentionally left blank")
               )
             )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- function() {
    get(input$metadata) %>% 
      kableExtra::kable() %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = TRUE)
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
