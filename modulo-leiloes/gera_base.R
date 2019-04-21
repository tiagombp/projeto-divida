library(tidyverse)
library(viridis)
library(extrafont)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(scales)
#library(ggTimeSeries)
library(purrr)

# funcao para processamento dos dados -------------------------------------

gera_base <- function(arq) {
  print(arq)
  tabela_leiloes <- read_excel(arq, sheet = "1.4", skip = 3) %>%
    select(-c(21:22)) # deixar isso mais elegante um dia
  
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
  
  
  linha_final <- which(str_detect(tabela_leiloes$`Comunicado/Portaria Tipo`, "Total liquidado em"))
  linha_final <- ifelse(length(linha_final) > 0, linha_final, length(tabela_leiloes$`Comunicado/Portaria Tipo`))
  
  dados <- tabela_leiloes %>%
    filter(row_number() > 1 & row_number() <= linha_final) %>%
    filter(!is.na(`Comunicado/Portaria Tipo`)) %>%
    select(-`Data-Base`) %>%
    rename(Data_emissao   = `Data de Emissão`,
           taxa_media     = `Taxas (% a.a.) (b) Média`,
           demanda        = `Demanda / Oferta`,
           titulo         = `Título`,
           valor_venda    = `Venda R$ Milhões`, #\r\n
           Data_vencimento= `Data de Vencimento`,
           compras        = `Compra R$ Milhões`) %>%
    mutate(titulo_vcto    = paste(titulo, Data_vencimento),
           taxa_media     = as.numeric(taxa_media),
           demanda        = as.character(demanda),
           valor_venda    = as.numeric(valor_venda),
           compras        = as.numeric(compras)) %>%
    mutate_at(vars(starts_with("Data")), .funs = ~ifelse(class(.)[1] == "character",
                                                         as.Date(as.numeric(.), origin = "1899-12-30"),
                                                         as.Date(.)))
  
  return(dados)
}


# lista de arquivos a processar -------------------------------------------

caminho <- "C:/Temp/arqs_RMD"
arquivos <- list.files(caminho)
setwd(caminho)
##amostra <- arquivos[1:20]

# gera lista de dfs -------------------------------------------------------

lista_dados <-purrr::map(arquivos, gera_base)
base <- bind_rows(lista_dados) %>% select(-contains("Normativo"))


# save --------------------------------------------------------------------

save(base, file = "leiloes.RData")

# área de testes ----------------------------------------------------------

arq_teste <- "Anexo_RMD_Mai_18.xlsx"
tabela_leiloes <- read_excel(arq_teste, sheet = "1.4", skip = 3)
str(dados %>% select(starts_with("Data")))

