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

### aproveitando o RData da execução orçamentária

load("../exec_orcam.RData")

vazamento <- read_excel("vazamento.xlsx", skip = 9)
names(vazamento) <- c("Ano", "OutrasDesp")

dados_nec_fin <- dados %>%
  mutate(
    Classificador = case_when(
      Tipo.de.Dívida == "Externa" & Carteira == "MERCADO" ~ "Externa",
      Tipo.de.Dívida == "Interna" & Carteira == "MERCADO" ~ "Interna",
      Carteira == "BACEN" & Movimentação.Financeira == "Juros e Encargos" ~ "Encargos Bacen",
      TRUE ~ "Principal Bacen"),
    Necessidade = "Vencimentos DPF"
    ) %>%
  group_by(Necessidade,
           Classificador,
           MovFin = Movimentação.Financeira,
           Ano = Exercicio) %>%
  summarise(Valor = sum(Saldo.Atual)) %>%
  ungroup() %>%
  filter(Classificador != "Principal Bacen")

# # encargos Bacen
# dados %>% filter(Movimentação.Financeira == "Principal" & Carteira == "BACEN") %>%
#    group_by(Exercicio) %>%
#    summarise(soma = sum(Saldo.Atual)) %>%
#   ggplot(aes(Exercicio, soma)) + geom_line(group = 1) + geom_label(aes(label = round(soma/1e9, 0)))

ggplot(dados_nec_fin %>%
         group_by(Ano, Classificador) %>%
         summarise(Valor = sum(Valor)), aes(x = Ano, y = Valor, group = Classificador, color = Classificador, fill = Classificador)) + 
  geom_area() + 
  scale_y_continuous(labels = function(x) {format(x/1e9, big.mark = ".", decimal.mark = ",")})

dados_plotly <- dados_nec_fin %>%
  mutate(
    serie = case_when(
      Classificador == "Encargos Bacen" ~ "Encargos Bacen",
      Classificador == "Externa" ~ "Externa",
      Classificador == "Interna" ~ as.character(MovFin))
  ) %>%
  group_by(Ano, serie) %>%
  summarise(Valor = sum(Valor)) %>%
  ungroup() %>%
  spread(key = serie, value = Valor) %>%
  left_join(vazamento, by = "Ano") %>%
  mutate(Interna       = `Juros e Encargos` + Principal,
         Vencimentos   = Interna + Externa + `Encargos Bacen`,
         graf_Juros    = Principal + `Juros e Encargos`,
         graf_DivEx    = graf_Juros + Externa,
         graf_EncBCB   = graf_DivEx + `Encargos Bacen`,
         graf_OutDesp  = graf_EncBCB + OutrasDesp)
  

cores <- viridis(7)

formata <- function(x){
  paste0("R$ ", format(round(x/1e9, 2), big.mark = ".", decimal.mark = ","), " bi")
}

graf1 <- 
  plot_ly(dados_plotly, x = ~Ano, type = "scatter", fill = 'tozeroy', y = ~`graf_OutDesp`, 
        name = "Outras Despesas", mode = "none", stackgroup = 'one', fillcolor = cores[1],
        text = ~paste0("Outras Despesas", " (", Ano, "): ", formata(OutrasDesp)),
        hoverinfo = "text")%>%
  add_trace(y = ~Vencimentos, name = "Vencimentos da DPF", fillcolor = cores[2],
            text = ~paste0("Vencimentos", " (", Ano, "): ", formata(Vencimentos)),
            hoverinfo = "text")
  
  

