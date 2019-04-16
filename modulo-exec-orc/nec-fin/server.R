library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(plotly)
library(RColorBrewer)
library(scales)

cores_pasteis <- c("#D5845E", "#F3987D", "#FECE60", "#F7EC64", "#CED950", "#63BEAF", 
                   "#149339", "#CED9D8", "#049DAC", "#7A9CBD", "#BE8EBF")

server = function(input, output, session) { 
  
  load("nec_fin.RData", envir=.GlobalEnv)
  
  observeEvent(input$graf1, {
    output$grafico <- renderPlotly(graf1)
  })
  
  observeEvent(input$graf2, {
    output$grafico <- renderPlotly(graf2)
  })
  
  observeEvent(input$graf3, {
    output$grafico <- renderPlotly(graf3)
  })
  
}
  
