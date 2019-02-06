library(ckanr)
library(readxl)
library(readr)
library(tidyverse)

# Leitura do Arquivo diretamente do ckan

recurso_TT <- resource_show(id="530e012d-1f9d-41a9-8cab-49849313e018", url="https://apickan.tesouro.gov.br/ckan")
download.file(recurso_TT$url, destfile = "./exec_divida.csv", mode = 'wb')
tabela <- read.csv2("exec_divida.csv")

# Processamento

dados <- tabela %>%
  mutate_at(c("janeiro":"dezembro"), fun = as.numeric(as.character(.))) %>%
  gather(c("janeiro":"dezembro"), key = "Mês", value = "Valor")



