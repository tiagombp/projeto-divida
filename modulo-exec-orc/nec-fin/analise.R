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


