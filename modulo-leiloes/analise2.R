library(tidyverse)
library(viridis)
library(extrafont)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(scales)
library(ggTimeSeries)
library(purrr)

# funcao para processamento dos dados -------------------------------------

gera_base <- function(arq) {
  print(arq)
  tabela_leiloes <- read_excel(arq, sheet = "1.4", skip = 3)
  
  ajusta_cabecalhos <- data.frame("linha1" = names(tabela_leiloes), "linha2" = t(tabela_leiloes[1,])) %>%
    mutate_all(as.character) %>%
    mutate(parte1 = ifelse(str_detect(linha1, "\\.\\."), NA, linha1)) %>%
    fill(parte1) %>%
    replace_na(list(linha2 = "")) %>%
    mutate(linha1 = case_when(linha1 == "Normativo" ~ "Comunicado/Portaria",
                              TRUE ~ linha1),
           linha2 = case_when(linha2 == "Númro" ~ "Número",
                              linha2 == "Tipo (f)" ~ "Tipo",
                              TRUE ~ linha2)) %>%
    mutate(titulos = str_squish(paste(parte1, linha2)))
  
  names(tabela_leiloes) <- ajusta_cabecalhos$titulos
  
  dados <- tabela_leiloes %>%
    filter(row_number() > 1) %>%
    select(-`Data-Base`) %>%
    rename(Data_emissao   = `Data de Emissão`,
           taxa_media     = `Taxas (% a.a.) (b) Média`,
           demanda       = `Demanda / Oferta`,
           titulo         = `Título`,
           valor_venda    = `Venda R$ Milhões`, #\r\n
           Data_vencimento     = `Data de Vencimento`) %>%
           titulo_vcto    = paste(titulo, Data_vencimento),
           taxa_media     = as.numeric(taxa_media)) %>%
    mutate_at(vars(starts_with("Data")), .funs = ~ifelse(as.character(.),
                                                         as.Date(as.numeric(.), origin = "1899-12-30")
                                                         as.Date(.)))
  
  return(dados)
}


# lista de arquivos a processar -------------------------------------------

caminho <- "C:/arqs_RMD"
arquivos <- list.files(caminho)
setwd(caminho)

amostra <- arquivos[1:20]

# gera lista de dfs -------------------------------------------------------

lista_dados <-purrr::map(arquivos, gera_base)
base <- bind_rows(teste)


# área de testes ----------------------------------------------------------

arq_teste <- "Anexo RMD Agosto 2010.xlsx"
tabela_leiloes <- read_excel(arq_teste, sheet = "1.4", skip = 3)
str(dados %>% select(starts_with("Data")))

