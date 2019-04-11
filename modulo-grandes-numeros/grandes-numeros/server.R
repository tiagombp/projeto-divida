#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(scales)

server = function(input, output, session) { 
  
  load("GN.RData")
  output$EstoqueDPF      <- renderPlot(lista_graficos[["Estoque DPF"]], height = 250)
  output$Prefixado       <- renderPlot(lista_graficos[["Prefixado"]], height = 250)
  output$IndicePrecos    <- renderPlot(lista_graficos[["Índice Preços"]], height = 250)
  output$Flutuante       <- renderPlot(lista_graficos[["Taxa Flutuante"]], height = 250)
  output$Cambio          <- renderPlot(lista_graficos[["Câmbio"]], height = 250)
  output$PercentVincendo <- renderPlot(lista_graficos[["% Vincendo em 12 meses"]], height = 250)
  output$PrazoMedio      <- renderPlot(lista_graficos[["Prazo Médio (anos)"]], height = 250)
}

