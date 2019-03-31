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
  output$EstoqueDPF <- lista_graficos[["Estoque DPF"]]
  output$Prefixado  <- lista_graficos[["Prefixado"]]
  output$IndicePrecos <- lista_graficos[["?ndice Pre?os"]]
  output$Flutuante <- lista_graficos[["Taxa Flutuante"]]
  output$Cambio <- lista_graficos[["C?mbio"]]
  output$PercentVincendo <- lista_graficos[["% Vincendo em 12 meses"]]
  output$PrazoMedio <- lista_graficos[["Prazo M?dio (anos)"]]
}

