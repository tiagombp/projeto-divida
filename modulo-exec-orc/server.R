library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)
library(plotly)
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(RColorBrewer)

server = function(input, output, session) { 
  
  load("exec_orcam.RData")
  
  intervalo_anos <- as.numeric(as.character(unique(dados_sankey$Ano)))
  
  plota_sankey <- function(ano, criterio, apenas_mercado) {
    if (apenas_mercado) {
      dadinhos <- dados_sankey %>% filter(Carteira == "MERCADO")
    }
    else {
      dadinhos <- dados_sankey
    }
    
    dadinhos <- dadinhos %>%
      filter(Ano == ano) %>%
      group_by(origem  = Fonte_cod,
               meiuca  = Fonte,
               destino = !!sym(criterio)) %>%
      summarise(Valor = sum(Valor)) %>%
      ungroup() %>%
      arrange(meiuca, Valor)
    
    
    qde_grupos_fontes <- length(unique(dados_sankey$Fonte))
    qde_grupos_fontes_selecao <- length(unique(dadinhos$meiuca))
    
    # tabela com nomes e números de nós
    
    vetor_nos_origem  <- unique(as.character(dadinhos$origem))
    vetor_nos_meiuca  <- unique(as.character(dadinhos$meiuca))
    vetor_nos_destino <- unique(as.character(dadinhos$destino))
    
    nomes_nos <- c(vetor_nos_origem,
                   vetor_nos_meiuca,
                   vetor_nos_destino)
    
    numeros_nos <- 0:(length(nomes_nos)-1)
    
    tbl_nos <- data.frame(nomes_nos, numeros_nos)
    
    # cores. cores dos nos e tabela de cores para os ramos, conforme os grupos de fontes.
    
    qde_cores_necessarias <- qde_grupos_fontes + length(vetor_nos_destino)
    
    paleta <- viridis(qde_cores_necessarias)
    
    cores_nos_fontes   <- paleta[1:qde_grupos_fontes]
    cores_nos_destinos <- paleta[(qde_grupos_fontes + 1):qde_cores_necessarias]
    cores_ramos <- viridis(qde_cores_necessarias, 0.5)[1:qde_grupos_fontes] # com alpha = 0.5
    
    tbl_cores_fontes <- data.frame("nomes_nos"  = vetor_nos_meiuca,
                                   "cor_no"     = cores_nos_fontes[1:qde_grupos_fontes_selecao],
                                   "cor_ramo"   = cores_ramos[1:qde_grupos_fontes_selecao]) #  pq quero fixar as cores dos ramos de acordo com as fontes
    
    tbl_cores_destinos <- data.frame("nomes_nos" = vetor_nos_destino,
                                     "cor_no"    = cores_nos_destinos)
    
    # a matriz (ou sua metade esquerda)
    
    origem_meiuca <- dadinhos %>%
      group_by(origem, meiuca) %>%
      summarise(Valor = sum(Valor)) %>%
      ungroup() %>%
      left_join(tbl_cores_fontes, by = c("meiuca" = "nomes_nos")) %>%
      rename(src = origem,
             trg = meiuca)
    
    ## abre parênteses
    
    # gerar tabela de cores.
    
    tbl_cores_origens <- origem_meiuca %>%
      select("nomes_nos" = src, cor_no) %>%
      group_by(nomes_nos) %>%
      filter(row_number() == 1) %>% 
      ungroup()
    
    tbl_cores_nos <- rbind(tbl_cores_origens,
                           tbl_cores_fontes[,-3], #para tirar a coluna cor_ramo
                           tbl_cores_destinos) %>%
      left_join(tbl_nos) %>%
      arrange(numeros_nos)
    
    ## fecha parênteses. metade direita.
    
    meiuca_destino <- dadinhos %>%
      group_by(meiuca, destino) %>%
      summarise(Valor = sum(Valor)) %>%
      ungroup() %>%
      left_join(tbl_cores_fontes, by = c("meiuca" = "nomes_nos")) %>%
      select(-cor_no) %>%
      left_join(tbl_cores_destinos, by = c("destino" = "nomes_nos")) %>%
      rename(src = meiuca,
             trg = destino)
    
    matriz <- rbind(origem_meiuca, meiuca_destino) %>%
      left_join(tbl_nos, by = c("src" = "nomes_nos")) %>%
      rename("no_src" = numeros_nos) %>%
      left_join(tbl_nos, by = c("trg" = "nomes_nos")) %>%
      rename("no_trg" = numeros_nos) %>%
      select(-cor_no)
    
    # plotar
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      opacity = 0.6,
      textfont = list(
        family = "Calibri Light",
        color = "#444444",
        size = 12
      ),
      
      node = list(
        label = tbl_cores_nos$nomes_nos,
        color = tbl_cores_nos$cor_no,
        pad = 10,
        thickness = 25,
        line = list(
          color = "",
          width = 0
        )
      ),
      
      hoverlabel = list(
        font = list(
          family = "Calibri Light"
        )
      ),
      
      link = list(
        source = matriz$no_src,
        target = matriz$no_trg,
        value  = matriz$Valor,
        color  = matriz$cor_ramo
        #color =  "rgba(255,213,0,0.4)"
        # para deixar a cor translucida, é preciso usar rgba
        
      )
    ) %>%
      layout(
        title = "",
        font = list(
          family = "Calibri Light",
          size = 11,
          color = "#004a93"
        )
      )
  }
  
  ano_selecionado <- reactive(
    as.character(input$ano)
  )
  
  criterios_selecionados <- reactive(
    input$criterios
  )
  
  carteira_selecionada <- reactive(
    input$carteira
  )
  
  output$texto <- renderText({print(ano_selecionado())})
  # output$exec-orcam <- renderPlotly({
  #   plota_sankey(ano_selecionado(), criterios_selecionados(), carteira_selecionada())
  #})
}
    

