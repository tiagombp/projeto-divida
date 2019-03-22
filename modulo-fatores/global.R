
# carga dos temas ---------------------------------------------------------



library(ckanr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)
library(RColorBrewer)
library(purrr)
library(plotly)
library(shiny)
library(flexdashboard)
# loadfonts()
# extrafont::font_import()


# definicao do tema -------------------------------------------------------

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Source Sans Pro", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Source Sans Pro", colour = "grey20", size = 8),
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

vermelho <- brewer.pal(3, name = "Set1")[1]
azul <- "#1f476a" 

vermelho_claro <- "#ee7576"
azul_claro     <- "#2c90bf" # "#87b1d4"


# importacao dos dados ----------------------------------------------------

tabela <- read_excel("../Anexo_RMD_Dez_18.xlsx", skip = 4, sheet = "2.9")

# correção de nomes de colunas duplicados
colnames(tabela)[which(colnames(tabela) == "Out/16")+1] <- "Nov/16"
colnames(tabela)[which(colnames(tabela) == "Out/17")+1] <- "Nov/17"

linhas_de_interesse <- c("Estoque anterior1",
                         "Estoque mês em análise",
                         "Variação Nominal",
                         "I.1.1 - Emissões",
                         "I.1.2 - Resgates",
                         "I.2 - Juros  Apropriados",
                         "II.1 - Transferência de carteira 11")

classificadores <- c("DPMFi", "DPFe")

lista_fatores <- c("Estoque_ant",
                   "Estoque_atu",
                   "Variacao",
                   "Emissoes",
                   "Resgates",
                   "Juros",
                   "Transf")

tab_novos_titulos <- tibble(linhas_de_interesse, lista_fatores)

meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

dados <- tabela %>%
  mutate(Fatores = ifelse(Indicadores %in% linhas_de_interesse,
                          Indicadores,
                          NA)) %>%
  fill(Fatores) %>%
  mutate(TipoDivida = ife
  filter(Indicadores %in% linhas_de_interesse) %>%
  left_join(tab_novos_titulos, by = c("Indicadores" = "linhas_de_interesse")) %>%
  select(-Indicadores) %>%
  select(fatores = lista_fatores, everything()) %>%
  gather(-fatores, key = mes_ano, value = valor) %>%
  filter(str_detect(mes_ano, "/")) %>%
  separate(mes_ano, into = c("Mes", "Ano"), sep = "/") %>%
  mutate(Ano = paste0('20', Ano),
         Mes_num = match(Mes, meses),
         Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes_num, 2), "-",
                                  "01")),
         valor = as.numeric(valor)) %>%
  spread(fatores, value = valor) %>%
  mutate(transf_positiva = ifelse(Transf > 0, Transf, 0),
         transf_negativa = ifelse(Transf > 0, 0,      Transf)) %>%
  select(-Transf, -Variacao) %>%
  gather(Emissoes:transf_negativa, key = "fatores", value = "valor")
