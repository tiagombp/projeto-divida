

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidRow(
             tabBox(id="graf_fatores",
                    tabPanel("Fatores de Variação",
                             tabBox(id="dpfe",
                                    tabPanel("DPFe", plotlyOutput("DPFe_completo"))),
                             tabBox(id="dpmfi",
                                    tabPanel("DPMFi", plotlyOutput("DPMFi_completo")))
                    )
             )
           )
   




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  load("fatores.RData")
  desloc <- 15
  
  plota_completo_por_tipo <- function(tipo_divida) {
    
    sub_bases <- lista_subs[[tipo_divida]]
    
    formata <- function(x){
      paste0("R$", format(round(x/1e3, 2), big.mark = ".", decimal.mark = ","), " bi")
    }
    
    graf <- plot_ly() %>%
      add_segments(data = sub_bases[["Juros"]], 
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend, name = "Juros") %>%
      add_segments(data = sub_bases[["Emissoes"]], text = ~paste(fatores, "<br />",
                                                                 Periodo, "<br />",
                                                                 formata(valor)), hoverinfo = "text",
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend, name = "Emissões") %>%
      add_segments(data = sub_bases[["transf_positiva"]],
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend, name = "Transferências(+)") %>%
      add_segments(data = sub_bases[["transf_negativa"]], 
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend, name = "Transferências(-)") %>%
      add_segments(data = sub_bases[["Resgates"]], 
                   x = ~Periodo+desloc, xend = ~Periodo+desloc, y = ~y, yend = ~yend, name = "Resgates") %>%
      add_segments(data = sub_bases[["Emissoes"]], 
                   x = ~Periodo, xend = ~Periodo+desloc, y = ~yend, yend = ~yend,
                   line = list(dash = "dot", width = 1), name = "") %>%
      add_segments(data = sub_bases[["Resgates"]], 
                   x = ~Periodo+desloc, xend = ~prox_periodo, 
                   y = ~yend, yend = ~yend, line = list(dash = "dot", width = 1), name = "") %>%
      add_markers(data = sub_bases[["Estoque_ant"]], 
                  x = ~Periodo, y= ~valor, name = "Estoque")
   
    return(graf) 
  }
  
  # plota_completo_por_tipo("DPFe") 
  
   output$DPFe_completo <- renderPlotly({
     plota_completo_por_tipo("DPFe")
   })
   
   output$DPMFi_completo <- renderPlotly({
     plota_completo_por_tipo("DPMFi")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

