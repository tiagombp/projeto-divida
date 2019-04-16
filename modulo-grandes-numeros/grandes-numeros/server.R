library(shiny)
library(shinydashboard)
library(flexdashboard)
library(scales)
library(tidyverse)

load("GN.RData", envir=.GlobalEnv)

# # preparacao (estava no analise.R, agora trouxe pra cá)
# 
# indicadores <- unique(base_GN$Indicador)
# lista_graficos <- purrr::map(indicadores, gera_graf)
# names(lista_graficos) <- indicadores
# 
# # fim preparacao

server = function(input, output, session) { 
  
  alturas = 180
  
  output$EstoqueDPF      <- renderPlot(lista_graficos[["Estoque DPF"]], height = alturas)
  output$Prefixado       <- renderPlot(lista_graficos[["Prefixado"]], height = alturas)
  output$IndicePrecos    <- renderPlot(lista_graficos[["Índice Preços"]], height = alturas)
  output$Flutuante       <- renderPlot(lista_graficos[["Taxa Flutuante"]], height = alturas)
  output$Cambio          <- renderPlot(lista_graficos[["Câmbio"]], height = alturas)
  output$PercentVincendo <- renderPlot(lista_graficos[["% Vincendo em 12 meses"]], height = alturas)
  output$PrazoMedio      <- renderPlot(lista_graficos[["Prazo Médio (anos)"]], height = alturas)
}

