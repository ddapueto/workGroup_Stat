
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
                             unique(unlist(compras[c("Atributo")])))
               ),
               conditionalPanel(
                 condition = "input.metadata == 'adjudicaciones'",
                 selectInput("cod" , "Codigos de Atributos en Adjudicaciones",
                             unique(unlist(adjudicaciones[c("Atributo")])))
               ),
               conditionalPanel(
                 condition = "input.metadata == 'oferantes'",
                 selectInput("cod" , "Codigos de Atributos en Oferentes",
                             unique(unlist(oferantes[c("Atributo")])))
               ),
               actionButton("controller", "Controller")
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
