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

server = function(input, output, session) { 
  
  load("GN.RData")
  output$EstoqueDPF      <- renderPlot(lista_graficos[["Estoque DPF"]])
  output$Prefixado       <- renderPlot(lista_graficos[["Prefixado"]])
  output$IndicePrecos    <- renderPlot(lista_graficos[["Índice Preços"]])
  output$Flutuante       <- renderPlot(lista_graficos[["Taxa Flutuante"]])
  output$Cambio          <- renderPlot(lista_graficos[["Câmbio"]])
  output$PercentVincendo <- renderPlot(lista_graficos[["% Vincendo em 12 meses"]])
  output$PrazoMedio      <- renderPlot(lista_graficos[["Prazo Médio (anos)"]])
}

