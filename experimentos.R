library(ckanr)
library(readxl)
library(readr)
library(tidyverse)
library(stringr)
library(viridis)
library(extrafont)
library(purrr)

loadfonts()

### Leitura do Arquivo diretamente do ckan

# por fonte: 0c7cf480-69a8-446c-9942-a01bd5abdd5a
# por nd   : 530e012d-1f9d-41a9-8cab-49849313e018

recurso_TT <- resource_show(id="0c7cf480-69a8-446c-9942-a01bd5abdd5a", url="https://apickan.tesouro.gov.br/ckan")
download.file(recurso_TT$url, destfile = "./exec_divida.csv", mode = 'wb')
tabela <- read.csv2("exec_divida.csv")

### Processamento dos dados

dados <- tabela %>%
  filter(!is.na(Exercicio)) %>%
  mutate(Fonte.Recursos.Código = str_pad(Fonte.Recursos.Código, 2, pad = "0"),
         Grupo.Fonte = str_sub(Fonte.SOF.Código, 1, 1),
         Tipo.de.Fonte = as.factor(ifelse(Fonte.Recursos.Código %in% c("43", "44"),
                                          "Emissões",
                                          as.character(Tipo.de.Fonte))),
         Exercicio = as.factor(Exercicio))

### Visualizando um pouquinho

plota_percentual_por_exercicio <- function(criterio) {
  ggplot(dados, aes(x = Exercicio, y = Saldo.Atual, fill = !!sym(criterio))) +  # (1)
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis_d() +
  theme_minimal() + theme(text = element_text(family = "Calibri Light", colour = "grey20"),
                          legend.position = "bottom")
  }

# (1) essa é a maneira de dizer pra ele que quero que ele interprete o parâmetro "criterio", que vai ser passado pelo map, como um objeto/variavel/coluna.

criterios <- c("Tipo.de.Fonte", "Carteira", "Modalidade.da.Dívida", "Movimentação.Financeira")

map(criterios, plota_percentual_por_exercicio)


ggplot(dados, aes(x = Exercicio, y = Saldo.Atual, fill = Tipo.de.Fonte)) +
  geom_area()
  




