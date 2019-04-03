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


# salva ------------------------------------------------------------

save(dados, dados_sankey, file = "exec_orcam.RData")
