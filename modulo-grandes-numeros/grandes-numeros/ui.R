library(shiny)
library(shinydashboard)
library(flexdashboard)

header <- dashboardHeader(title = "D?vida P?blica Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))

sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("DPF em Mercado", tabName = "estoque" ,icon = icon("bar-chart-o"),
                                       menuSubItem("Evolu??o", tabName = "evolucao"),
                                       menuSubItem("Fatores de Varia??o", tabName = "fatores"),
                                       menuSubItem("Indexadores", tabName = "composicao"),
                                       menuSubItem("Detentores", tabName = "detentores")))
)

body <- dashboardBody(
  tabItems(
    tabItem("GrandesNumeros",
            strong("Estoque Nominal"),
            fluidRow(
              box(plotOutput("EstoqueDPF"), title ="Estoque DPF - Bilh?es R$", solidHeader = T ,width = 4)
            ),
            strong("Composi??o"),
            fluidRow(
              box(plotOutput("Prefixado"), title ="Indexador Prefixado", solidHeader = T , width = 4),
              box(plotOutput("IndicePrecos"), title ="?ndice de Pre?os", solidHeader = T , width = 4),
              box(plotOutput("Flutuante"), title ="Indexador Flutuante", solidHeader = T , width = 4),
              box(plotOutput("Cambio"), title ="C?mbio", solidHeader = T , width = 4)
            ),
            strong("Estrutura de Vencimentos"),
            fluidRow(
              box(plotOutput("PercentVincendo"), title ="% Vincendo em 12 meses", solidHeader = T , width = 4),
              box(plotOutput("PrazoMedio"), title ="Prazo M?dio", solidHeader = T , width = 4)
            )
    ))
)

# Define UI for application that draws a histogram
ui = dashboardPage(header, sidebar, body)