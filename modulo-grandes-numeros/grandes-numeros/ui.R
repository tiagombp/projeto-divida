library(shiny)
library(shinydashboard)
library(flexdashboard)

load("GN.RData")


header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))

sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("Grandes Números", tabName = "GrandesNumeros" ,icon = icon("bar-chart-o")))
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "GrandesNumeros",
            
            fluidRow(
              box(plotOutput("EstoqueDPF"), title = paste("Estoque DPF:",lista_valores["Estoque DPF"]),
                  solidHeader = T ,width = 4, height = 250, background = "blue", collapsible = T)
            ),
            strong("Composição"),
            fluidRow(
              box(plotOutput("Prefixado"), title ="Indexador Prefixado", solidHeader = T , width = 3, height = 350),
              box(plotOutput("IndicePrecos"), title ="Índice de Preços", solidHeader = T , width = 5, height = 350),
              box(plotOutput("Flutuante"), title ="Indexador Flutuante", solidHeader = T , width = 6, height = 350),
              box(plotOutput("Cambio"), title ="Câmbio", solidHeader = T , width = 6, height = 350)
            ),
            strong("Estrutura de Vencimentos"),
            fluidRow(
              box(plotOutput("PercentVincendo"), title ="% Vincendo em 12 meses", solidHeader = T , width = 6, height = 350),
              box(plotOutput("PrazoMedio"), title ="Prazo Médio", solidHeader = T , width = 6, height = 350)
            )
    ))
)

# Define UI for application that draws a histogram
ui = dashboardPage(header, sidebar, body)