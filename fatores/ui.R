library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(plotly)
library(RColorBrewer)

header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))

sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("DPF em Mercado", tabName = "estoque" ,icon = icon("bar-chart-o"),
                                       menuSubItem("Evolução", tabName = "evolucao"),
                                       menuSubItem("Fatores de Variação", tabName = "fatores"),
                                       menuSubItem("Indexadores", tabName = "composicao"),
                                       menuSubItem("Detentores", tabName = "detentores")))
)

body <- dashboardBody(
  
  tabItem(tabName = "fatores",
            fluidRow(
              tabBox(id="graf_fatores", width = 1000, height=600,
                            tabPanel("Fatores de Variação e Estoque",
                                     tabBox(id="fatores_estoque", width = 1000, height=600,
                                            tabPanel("DPMFi", plotlyOutput("DPMFi_completo")),
                                            tabPanel("DPFe", plotlyOutput("DPFe_completo")),
                                            tabPanel("Total", plotlyOutput("Total_completo"))
                                            )
                                     ),
                            tabPanel("Apenas fatores de Variação",
                                     tabBox(id="so_fatores", width = 1000, height=600,
                                            tabPanel("DPMFi", plotlyOutput("DPMFi_so_fatores")),
                                            tabPanel("DPFe", plotlyOutput("DPFe_so_fatores")),
                                            tabPanel("Total", plotlyOutput("Total_so_fatores"))
                                            )
                                     )
                     )
              )
          )
  )

ui = dashboardPage(header, sidebar, body)