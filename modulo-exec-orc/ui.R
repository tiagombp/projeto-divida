library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)
library(plotly)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(RColorBrewer)

header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))

load("exec_orcam.RData")
intervalo_anos <- as.numeric(as.character(unique(dados_sankey$Ano)))

sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("Execução Orçamentária", 
                                       tabName = "exec_orcam" ,
                                       icon = icon("bar-chart-o"))
                              )
)

body <- dashboardBody(
  
  tabItem(tabName = "exec_orcam",
          tabBox(id="graf_exec_orc", width = 1000, height=800,
                 tabPanel("Execução Orçamentária da Dívida",
                          inputPanel(
                          sliderInput("ano", "Escolha o exercício", 2009, min = min(intervalo_anos) , max = max(intervalo_anos), sep = ""),
                          radioButtons("criterios", "Escolha a variável", choiceNames = c("Tipo de Dívida", "Tipo de Carteira", "Tipo de Movimentação Financeira (Juros / Principal)"), choiceValues = c("Mod", "Carteira", "Mov"), selected = "Mov"),
                          checkboxInput("carteira", "Apenas Mercado?", value = FALSE)),
                          plotlyOutput("sankey"))
                 )
          )
  )

ui = dashboardPage(header, sidebar, body)