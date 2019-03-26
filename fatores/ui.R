library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(plotly)

header <- dashboardHeader(title = "Dívida Pública Federal", titleWidth = 270, tags$li(class="dropdown", tags$a(href="https://github.com/gt-cead", icon("github"), "Source Code", target ="_blank")))



sidebar <- dashboardSidebar(width =270,
                            
                            sidebarSearchForm(label = "Procurar", "searchText", "searchButton"),
                            sidebarMenu(
                              # Setting id makes input$tabs give the tabName of currently-selected tab
                              
                              id = "tabs",
                              
                              menuItem("Grandes Números", tabName = "grandesnumeros", icon = icon("dashboard"),
                                       menuSubItem("Resultados Esperados 2018", tabName = "resultados"),
                                       menuSubItem("Diretrizes e Objetivos", tabName = "objetivos")),
                              
                              menuItem("Necessidade Financiamento", tabName = "necessidade" ,icon = icon("code-fork"), 
                                       menuSubItem("Bruta", tabName = "bruta"),
                                       menuSubItem("Líquida", tabName = "liquida"),
                                       menuSubItem("Execução Orçamentária", tabName = "orcamento")),
                              
                              menuItem("Operações no Mercado", tabName = "mercadoprimario" ,icon = icon("calendar"),
                                       menuSubItem("Leilões Realizados", tabName = "leiloes"),
                                       menuSubItem("Emissões e Resgastes", tabName = "resgates")),
                              
                              menuItem("DPF em Mercado", tabName = "estoque" ,icon = icon("bar-chart-o"),
                                       menuSubItem("Evolução", tabName = "evolucao"),
                                       menuSubItem("Fatores de Variação", tabName = "fatores"),
                                       menuSubItem("Indexadores", tabName = "composicao"),
                                       menuSubItem("Detentores", tabName = "detentores")),
                              
                              menuItem("Perfil Vencimentos", tabName = "vencimentos", icon = icon("bar-chart-o"),
                                       menuSubItem("Composição dos Vencimentos", tabName = "vincendo"),
                                       menuSubItem("Prazo Médio", tabName = "prazomedio"),
                                       menuSubItem("Vida Média", tabName = "vidamedia")),
                              
                              menuItem("Custo Médio", tabName = "custos", icon = icon("bar-chart-o"),
                                       menuSubItem("Custo Médio Estoque", tabName = "custoestoque"),
                                       menuSubItem("Custo Médio Emissão", tabName = "custoemissao")),
                              
                              menuItem("Programa Tesouro Direto", tabName = "tesourodireto", icon = icon("child"),
                                       menuSubItem("Estoque, Emissões e Resgates", tabName = "tesouroestoque"),
                                       menuSubItem("Detentores", tabName = "tesourodetentores")),
                              
                              menuItem("Relatórios Publicados", tabName = "relatoriopublicados", icon = icon("file"),
                                       menuSubItem("Plano Anual de Financiamento - PAF", tabName = "paf"),
                                       menuSubItem("Relatorio Anual da Dívida - RAD", tabName = "rad"),
                                       menuSubItem("Relatorio Mensal da Dívida - RMD", tabName = "rmd"),
                                       menuSubItem("Relatorio de Garantias - RG", tabName = "garantias")),
                              
                              menuItem("Projeção Dívida - Faça a sua", tabName = "projecaodivida", icon = icon("cog", class = "fa-pulse"))
                              
                            )
)

body <- dashboardBody(
  
  tabItem(tabName = "fatores",
            fluidRow(tabBox(id="graf_fatores", width = 1000, height=560,
                            tabPanel("Fatores de Variação e Estoque",
                                     tabBox(id="DPMFi",
                                            tabPanel("DPMFi", plotlyOutput("DPMFi_completo"))),
                                     tabBox(id="DPFe",
                                            tabPanel("DPFe", plotlyOutput("DPFe_completo"))),
                                     tabBox(id="Total",
                                            tabPanel("Total", plotlyOutput("Total_completo")))
                            ),
                            tabPanel("Apenas fatores de Variação",
                                     tabBox(id="DPMFi",
                                            tabPanel("DPMFi_fat", 
                                                     plotlyOutput("DPMFi_so_fatores"))),
                                     tabBox(id="DPFe",
                                            tabPanel("DPFe_fat", 
                                                     plotlyOutput("DPFe_so_fatores"))),
                                     tabBox(id="Total",
                                            tabPanel("Total_fat", 
                                                     plotlyOutput("Total_so_fatores")))
                            )
            )
            )
    )
  )

ui = dashboardPage(header, sidebar, body)