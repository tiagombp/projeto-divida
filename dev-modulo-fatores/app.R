

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(plotly)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tabItem(tabName = "fatores",
              fluidRow(tabBox(id="graf_fatores",
                              tabPanel("Fatores de Variação e Estoque",
                                       tabBox(id="dpmfi_comp",
                                              tabPanel("DPMFi", plotlyOutput("DPMFi_completo"))),
                                       tabBox(id="dpfe_comp",
                                              tabPanel("DPFe", plotlyOutput("DPFe_completo"))),
                                       tabBox(id="total_comp",
                                             tabPanel("Total", plotlyOutput("Total_completo")))
                              ),
                              tabPanel("Apenas fatores de Variação",
                                       tabBox(id="dpmfi_fat",
                                              tabPanel("DPMFi_fat", 
                                                       plotlyOutput("DPMFi_so_fatores"))),
                                       tabBox(id="dpfe_fat",
                                              tabPanel("DPFe_fat", 
                                                       plotlyOutput("DPFe_so_fatores"))),
                                       tabBox(id="total_fat",
                                              tabPanel("Total_fat", 
                                                       plotlyOutput("Total_so_fatores")))
                                       )
                              )
                       )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  load("fatores.RData")
  desloc <- 15
  formata <- function(x){
    paste0("R$", format(round(x, 2), big.mark = ".", decimal.mark = ","), " mi")
  }
  
  plota_completo_por_tipo <- function(tipo_divida) {

    sub_bases <- lista_subs[[tipo_divida]]
    
    graf <- plot_ly(name = ~nomes, 
                    text = ~paste(nomes, "<br />",
                                  mes, "/", ano, "<br />",
                                  formata(valor)), hoverinfo = "text") %>%
      add_segments(data = sub_bases[["Juros"]], 
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend) %>%
      add_segments(data = sub_bases[["Emissoes"]], 
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend) %>%
      add_segments(data = sub_bases[["Resgates"]], 
                   x = ~Periodo+desloc, xend = ~Periodo+desloc, y = ~y, yend = ~yend) %>%
      add_segments(data = sub_bases[["transf_positiva"]],
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend) %>%
      add_segments(data = sub_bases[["transf_negativa"]], 
                   x = ~Periodo, xend = ~Periodo, y = ~y, yend = ~yend) %>%
      add_segments(data = sub_bases[["Emissoes"]], 
                   x = ~Periodo, xend = ~Periodo+desloc, y = ~yend, yend = ~yend,
                   line = list(dash = "dot", width = 1), name = "", showlegend = FALSE) %>%
      add_segments(data = sub_bases[["Resgates"]], 
                   x = ~Periodo+desloc, xend = ~prox_periodo, 
                   y = ~yend, yend = ~yend, line = list(dash = "dot", width = 1), name = "", showlegend = FALSE) %>%
      add_markers(data = sub_bases[["Estoque_ant"]], 
                  x = ~Periodo, y= ~valor) %>%
      layout(annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.9 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r')) %>%
      config(displayModeBar = FALSE)
    
    return(graf) 
  }
  
  plota_so_fatores_por_tipo <- function(tipo_divida) {
    sub_bases <- lista_dados_sem_estoque[[tipo_divida]]
    graf <- plot_ly(data = sub_bases, x = ~Periodo) %>%
      add_bars(y = ~Juros, name = "Juros", text = ~paste("Juros <br />", 
                                                         mes, "/", ano, "<br />",
                                                         formata(Juros)), hoverinfo = "text") %>%
      add_bars(y = ~Emissoes, name = "Emissões", text = ~paste("Emissões <br />", 
                                                               mes, "/", ano, "<br />",
                                                               formata(Emissoes)), hoverinfo = "text") %>%
      add_bars(y = ~Resgates, name = "Resgates", text = ~paste("Resgates <br />", 
                                                               mes, "/", ano, "<br />",
                                                               formata(Resgates)), hoverinfo = "text") %>%
      add_markers(y = ~var_pos, name = "Variação positiva", marker = list(color = "#4682B4"), symbol = I(2), text = ~paste("Variação <br />positiva <br />", mes, "/", ano, "<br />", formata(var_pos)), hoverinfo = "text") %>%
      add_markers(y = ~var_neg, name = "Variação negativa", marker = list(color = "#DC143C"), symbol = I(6), text = ~paste("Variação <br />negativa <br />", mes, "/", ano, "<br />", formata(var_neg)), hoverinfo = "text") %>%
      layout(annotations = 
               list(x = 1, y = -0.1, text = "<b>Fonte:</b> Anexo 2.9 - RMD.", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=100, yshift=-80,
                    font=list(size=12, color="#696969")),
             title = "",
             xaxis = list(title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 meses",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 ano",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 3,
                                label = "3 anos",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 5,
                                label = "5 anos",
                                step = "year",
                                stepmode = "backward"),
                              
                              list(step = "all"))),
                          
                          rangeslider = list(type = "date")),
             
             yaxis = list(title = "R$ - Milhões", zeroline = TRUE,
                          showline = FALSE, showgrid = FALSE, hoverformat = ',.4r')) %>%
      config(displayModeBar = FALSE)
  
    return(graf)
}
  
   output$DPFe_completo <- renderPlotly({
     plota_completo_por_tipo("DPFe")
   })
   
   output$DPMFi_completo <- renderPlotly({
     plota_completo_por_tipo("DPMFi")
   })
   
   output$Total_completo <- renderPlotly({
     plota_completo_por_tipo("Total")
   })
   
   output$DPFe_so_fatores <- renderPlotly({
     plota_so_fatores_por_tipo("DPFe")
   })
   
   output$DPMFi_so_fatores <- renderPlotly({
     plota_so_fatores_por_tipo("DPMFi")
   })
   
   output$Total_so_fatores <- renderPlotly({
     plota_so_fatores_por_tipo("Total")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

