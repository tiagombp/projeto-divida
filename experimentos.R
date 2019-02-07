library(ckanr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)

loadfonts()

### Leitura do Arquivo diretamente do ckan

# por fonte: 0c7cf480-69a8-446c-9942-a01bd5abdd5a
# por nd   : 530e012d-1f9d-41a9-8cab-49849313e018

recurso_TT <- resource_show(id="0c7cf480-69a8-446c-9942-a01bd5abdd5a", url="https://apickan.tesouro.gov.br/ckan")
download.file(recurso_TT$url, destfile = "./exec_divida.csv", mode = 'wb')
tabela <- read.csv2("exec_divida.csv")

### Processamento dos dados

dados <- tabela %>%
  mutate(Fonte.Recursos.Código = str_pad(Fonte.Recursos.Código, 2, pad = "0"),
         Grupo.Fonte = str_sub(Fonte.SOF.Código, 1, 1),
         Tipo.de.Fonte = as.factor(ifelse(Fonte.Recursos.Código %in% c("43", "44"),
                                          "Emissões",
                                          as.character(Tipo.de.Fonte))),
         Exercicio = as.factor(Exercicio))

### Visualizando um pouquinho

ggplot(dados, aes(x = Exercicio, y = Saldo.Atual, fill = Tipo.de.Fonte)) + 
  geom_col(position = "fill") +
  scale_fill_viridis_d() +
  theme_minimal() + theme(text = element_text(family = "Calibri", colour = "grey20"))
  
  




