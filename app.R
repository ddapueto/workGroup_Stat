
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
path_base_adj <- paste(path,"/Data/rds/adjudicaciones.rds", sep="")
oferentes <- read_csv(path_oferentes)
adjudicaciones <- read_csv(path_adj)
compras <- read_csv(path_comp)
base_compras <- readr::read_rds(path_base)

base_adj <- readr::read_rds(path_base_adj)

adjudicaciones_arti_inciso <- base_adj %>%
  mutate(desc_articulo = replace(desc_articulo, desc_articulo=="PASAJE EN TRANSPORTE AEREO DE PASAJEROS FUERA DEL PAIS, CONTRATADO EN URUGUAY", "PASAJE AEREO")) %>%
  group_by(desc_articulo,inciso) %>% 
  summarise(counta = n(), total_comprado = sum(monto_adj_pesos), promedio_costo_unit = mean(monto_adj_pesos_unit)) %>% 
  arrange(desc(counta))



compList <- c(
  #"apel", "es_reiteracion", 
  "estado_compra", "id_inciso","id_moneda","id_tipo_resol","id_tipocompra","id_ue", 
  "subtipo_compra", "id_ucc"
)
adjList <- c("id_moneda", "id_unidad", "tipo_doc_prov")

ofeList <- c("tipo_doc_prov")

incisoList <- rbind(c("Todos los entes"), levels(as.factor(base_compras$inciso)))

monedaList <- rbind(c("Todo las monedas"), levels(as.factor(base_compras$pesos_uruguayo)))

#funciones

skewness=function(x) {
  m3=mean((x-mean(x))^3)
  skew=m3/(sd(x)^3)
  skew}

kurtosis=function(x) {
  m4=mean((x-mean(x))^4) 
  kurt=m4/(sd(x)^4)-3  
  kurt}

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
                 selectInput("cod_com" , "Codigos de Atributos en Compras", compList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'adjudicaciones'",
                 selectInput("cod_adj" , "Codigos de Atributos en Adjudicaciones",
                             adjList)
               ),
               conditionalPanel(
                 condition = "input.metadata == 'oferentes'",
                 selectInput("cod_ofe" , "Codigos de Atributos en Oferentes",
                             ofeList)
               ),
               actionButton("controller", "Codigueras Existentes", icon("paper-plane"), 
                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               br(),
               br(),
               img(src="icono_stat.jpg",height=100,width=100)
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
                            c("Calculo sobre monto adjudicado " = "monto_adjj" ,
                              "Calculo sobre cantidad comprado" = "cant_adjj"), 
                            selected = 'monto_adjj'
               ), h4("Filtros a aplicar a las visualizaciones:"),
               selectInput("filtro_inciso", "Inciso:", incisoList, selected = "Administración de Servicios de Salud del Estado"),
               selectInput("filtro_moneda", "Moneda:", monedaList, selected = "pesos_uruguayo"),
               width = 3,
               br(),
               br(),
               img(src="icono_stat.jpg",height=100,width=100)
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Compras",icon = icon("bar-chart-o"),h3(textOutput("title_graph") , align="center"),
                          plotOutput("monto_adj"),
                          h3("Descripcion de la variable",align="center"),
                          verbatimTextOutput("sum_ofe"),
                          h3("Tabla de Datos de la visualizacion.",align="center"),
                          DT::dataTableOutput("mytable") ),
                 tabPanel("Adjudicaciones", icon = icon("briefcase"),
                          h3(textOutput("title_graph_adj"), align = "center" ),
                          plotOutput("plot_adj"),
                          h3("Tabla de Datos de la visualizacion"),
                          DT::dataTableOutput("adj_table")
                          )
               
             ))
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
    if (input$metadata == 'compras'){
      codTable <- readr::read_rds(paste(path, "/Data/rds/meta_", input$cod_com, ".rds", sep = ""))
    }
    if (input$metadata == "adjudicaciones"){
      codTable <- readr::read_rds(paste(path, "/Data/rds/meta_", input$cod_adj, ".rds", sep = ""))
    }
    if(input$metadata == "oferentes"){
      codTable <- readr::read_rds(paste(path, "/Data/rds/meta_", input$cod_ofe, ".rds", sep = ""))
    }
    names(codTable) <- str_to_title(str_replace_all(string = names(codTable), pattern = "_", replacement = " "))
    
    codTable %>% 
      kableExtra::kable() %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), fixed_thead = TRUE, full_width = FALSE)
  }
  
  da <- reactive({
    if ((input$filtro_inciso) != "Todos los entes") {
      base_compras %>% filter(inciso == input$filtro_inciso)
    }else{base_compras}
  })
  
  dam <- reactive({
    if ((input$filtro_moneda) != "Todo las monedas") {
      da() %>% filter(pesos_uruguayo == input$filtro_moneda) %>% filter("2018-01-01" < fecha_compra, fecha_compra < "2019-04-30", monto_adj > 0) %>%
        group_by(floor_date(fecha_compra, "week")) %>% 
        summarise(total_comprado = round(sum(monto_adj_pesos, na.rm = TRUE),digits = 0), cantidad_comprado = n(),monto_promedio_por_semana = round(total_comprado/cantidad_comprado,digits = 0)) %>%
        rename(semana = `floor_date(fecha_compra, "week")`)
    }else{base_compras %>% filter("2018-01-01" < fecha_compra, fecha_compra < "2019-04-30", monto_adj > 0) %>%
        group_by(floor_date(fecha_compra, "week")) %>% 
        summarise(total_comprado = round(sum(monto_adj_pesos, na.rm = TRUE), digits = 0), cantidad_comprado = n(),monto_promedio_por_semana = round(total_comprado/cantidad_comprado,digits = 0)) %>%
        rename(semana = `floor_date(fecha_compra, "week")`)}
  })
  
  output$title_graph <- renderText({
    if (input$visualizacion == 'monto_adjj'){
      paste("Serie de tiempo de monto adjudicado por semana para ", input$filtro_inciso , "en ", str_replace_all(string = input$filtro_moneda, pattern = "_", replacement = " "))
    }
    else{
      paste("Serie de tiempo de cantidad de adjudicaciones por semana ", input$filtro_inciso , "en ", str_replace_all(string = input$filtro_moneda, pattern = "_", replacement = " "))
    }
    
  })
  
  output$title_graph_adj <- renderText({
    if (input$visualizacion == 'monto_adjj'){
      paste("Grafico de barra segun articulo y monto gastado por ", input$filtro_inciso)
    }
    else{
      paste("Grafico de barra segun articulo y cantidad de adjudicaciones por ", input$filtro_inciso)
    }
    
  })
  
  output$sum_ofe <- renderPrint({
    if (input$visualizacion == 'monto_adjj'){
      df <- dam()[["total_comprado"]]
      qq <- c(summary(df),skewness(df))
      names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Sesgo")
      format(qq, scientific = FALSE,digits=2)
    }else{
      df <- dam()[["cantidad_comprado"]]
      qq <- c(summary(df),skewness(df))
      names(qq) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "Sesgo")
      format(qq, scientific = FALSE,digits=2)
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
  
  adj_fil <- reactive({
    if ((input$filtro_inciso) != "Todos los entes") {
      adjudicaciones_arti_inciso %>% filter(inciso == input$filtro_inciso) %>% 
        group_by(desc_articulo) %>% 
        summarise(veces_comprado = sum(counta) , total_gastado = round(sum(total_comprado)), 
                  gastado_unit_mediana = round(median(promedio_costo_unit)), max_gastado = round(max(promedio_costo_unit))) 
    }else{adjudicaciones_arti_inciso %>% 
        group_by(desc_articulo) %>% 
        summarise(veces_comprado = sum(counta) , total_gastado = round(sum(total_comprado)), 
                  gastado_unit_mediana = round(median(promedio_costo_unit)), max_gastado = round(max(promedio_costo_unit)))
      }
  })
  
  
  output$adj_table = DT::renderDataTable({
    adj_fil()  
  })
  
  output$plot_adj <- renderPlot({
    if (input$visualizacion == 'monto_adjj'){
      ggplot(adj_fil() %>% 
               arrange(desc(total_gastado)) %>% head(10) ,
             aes( y=total_gastado, x=fct_reorder(desc_articulo, total_gastado) , fill=total_gastado)) + 
        geom_bar( stat="identity") +
        coord_flip() +
        ggthemes::theme_economist() +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
        scale_fill_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
        theme(axis.title = element_text(face = "bold"),
              axis.text.y= element_text( size = 8, vjust = 0.3, hjust = 0.5),
              legend.position = "right"
              
              #axis.ticks.x = element_blank()
        ) +
        labs(y="Total comprado(en millones de pesos)", x="Articulo")
    }else{
      ggplot(adj_fil() %>% 
               arrange(desc(veces_comprado)) %>% head(10) ,
             aes( y=veces_comprado, x=fct_reorder(desc_articulo, veces_comprado) , fill=veces_comprado, text=paste(desc_articulo,": ",veces_comprado))) + 
        geom_bar( stat="identity") +
        coord_flip() +
        ggthemes::theme_economist() +
        scale_y_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
        scale_fill_continuous(labels = scales::dollar_format(prefix = "$U", big.mark = ".", decimal.mark = ",", scale = 1/1e6)) +
        theme(axis.title = element_text(face = "bold"),
              axis.text.y= element_text( size = 8, vjust = 0.3, hjust = 0.5),
              legend.position = "right"
        ) +
        labs(y="Veces comprado", x="Articulo")
    }
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)