library(shiny)
library(shinydashboard)
library(flexdashboard)

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
            strong("Estoque Nominal"),
            fluidRow(
              box(plotOutput("EstoqueDPF"), title ="Estoque DPF - Bilhões R$", solidHeader = T ,width = 6)
            ),
            strong("Composição"),
            fluidRow(
              box(plotOutput("Prefixado"), title ="Indexador Prefixado", solidHeader = T , width = 6),
              box(plotOutput("IndicePrecos"), title ="Índice de Preços", solidHeader = T , width = 6),
              box(plotOutput("Flutuante"), title ="Indexador Flutuante", solidHeader = T , width = 6),
              box(plotOutput("Cambio"), title ="Câmbio", solidHeader = T , width = 6)
            ),
            strong("Estrutura de Vencimentos"),
            fluidRow(
              box(plotOutput("PercentVincendo"), title ="% Vincendo em 12 meses", solidHeader = T , width = 6),
              box(plotOutput("PrazoMedio"), title ="Prazo Médio", solidHeader = T , width = 6)
            )
    ))
)

# Define UI for application that draws a histogram
ui = dashboardPage(header, sidebar, body)