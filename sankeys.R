library(ckanr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)
library(purrr)
library(plotly)
library(shiny)
loadfonts()

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri Light", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Calibri Light", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Calibri Light", colour = "grey20", size = 8),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(.25, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none')
}

### Leitura do Arquivo diretamente do ckan

# por fonte: 0c7cf480-69a8-446c-9942-a01bd5abdd5a
# por nd   : 530e012d-1f9d-41a9-8cab-49849313e018

recurso_TT <- resource_show(id="0c7cf480-69a8-446c-9942-a01bd5abdd5a", url="https://apickan.tesouro.gov.br/ckan")
download.file(recurso_TT$url, destfile = "./exec_divida.csv", mode = 'wb')
tabela <- read.csv2("exec_divida.csv")

dados <- tabela %>%
  filter(!is.na(Exercicio)) %>%
  mutate(Fonte.Recursos.Código = str_pad(Fonte.Recursos.Código, 2, pad = "0"),
         Grupo.Fonte = str_sub(Fonte.SOF.Código, 1, 1),
         Tipo.de.Fonte = as.factor(ifelse(Fonte.Recursos.Código %in% c("43", "44"),
                                          "Emissões",
                                          as.character(Tipo.de.Fonte))),
         Exercicio = as.factor(Exercicio),
         Saldo.Atual = as.numeric(str_replace(as.character(Saldo.Atual), ",", ".")),
         Saldo.Atual = ifelse(is.na(Saldo.Atual), 0, Saldo.Atual)) # (1)

# (1) o pessoal da codiv passou a colocar um " -   " no lugar dos zeros :/

dados_sankey <- dados %>%
  mutate(Fonte_cod = paste(
    str_pad(`Fonte.Recursos.Código`, 2, pad = "0"),
    Fonte.Recursos.Nome)) %>%
  group_by(Ano = Exercicio, 
           Fonte = Tipo.de.Fonte,
           Fonte_cod,
           Mod = `Modalidade.da.Dívida`, 
           Carteira, 
           Mov = `Movimentação.Financeira`) %>%
  summarise(Valor = sum(Saldo.Atual)) %>%
  ungroup()

# funções

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
  
  
  qde_grupos_fontes <- length(unique(dados_tidy_2$Fonte))
  
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
                                 "cor_no"     = cores_nos_fontes,
                                 "cor_ramo"   = cores_ramos) #  pq quero fixar as cores dos ramos de acordo com as fontes
  
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
    select("nomes_nos" = src, cor_no)
  
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

### Painel

intervalo_anos <- as.numeric(as.character(unique(dados_sankey$Ano)))

inputPanel(
  sliderInput("ano", "Escolha o exercício", 2009, min = min(intervalo_anos) , max = max(intervalo_anos), sep = ""),
  
  radioButtons("criterios", "Escolha a variável", choices = c("Mod", "Carteira", "Mov"), selected = "Mov"),
  
  checkboxInput("carteira", "Apenas Mercado?", value = FALSE)
)

renderPlotly({
  plota_sankey(as.character(input$ano), input$criterios, input$carteira)
})