
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

#create Ui 
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
                            selected = 'compras')
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Metadata Info",
                          h4("Table"),
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
  output$table <- renderTable({ get(input$metadata) })
}

# Run the application 
shinyApp(ui = ui, server = server)
