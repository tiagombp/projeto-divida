library(shiny)
library(shinydashboard)
library(flexdashboard)
library(ggplot2)

load("GN.RData", envir=.GlobalEnv)

header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))

sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("Grandes Números", tabName = "GrandesNumeros" ,icon = icon("bar-chart-o")))
)

alturas <- 250
largura <- 4

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "GrandesNumeros",
            
            fluidRow(
              box(plotOutput("EstoqueDPF"), 
                  title = paste("Estoque DPF:", lista_valores["Estoque DPF"]),
                  solidHeader = T , width = largura, height = alturas, background = "blue"),
              
              box(plotOutput("Prefixado"), 
                  title = paste("Composição Prefixado:", lista_valores["Prefixado"]),
                  solidHeader = T , width = largura, height = alturas, background = "orange"),
              
              box(plotOutput("IndicePrecos"), 
                  title = paste("Composição Índice de Preços:", lista_valores["Índice Preços"]),
                  solidHeader = T , width = largura, height = alturas, background = "orange"),
              
              box(plotOutput("Flutuante"), 
                  title = paste("Composição Flutuante:", lista_valores["Taxa Flutuante"]),
                  solidHeader = T , width = largura, height = alturas, background = "orange"),

              box(plotOutput("Cambio"), 
                  title = paste("Composição Câmbio:", lista_valores["Câmbio"]),
                  solidHeader = T , width = largura, height = alturas, background = "orange"),          

              box(plotOutput("PercentVincendo"), 
                  title = paste("Vincendo em 12 meses:", lista_valores["% Vincendo em 12 meses"]),
                  solidHeader = T , width = largura, height = alturas, background = "maroon"), 

              box(plotOutput("PrazoMedio"), 
                  title = paste("Prazo Médio:", lista_valores["Prazo Médio (anos)"]),
                  solidHeader = T , width = largura, height = alturas, background = "maroon")
              )
    ))
)

# Define UI for application that draws a histogram
ui = dashboardPage(header, sidebar, body)