library(tidyverse)
library(viridis)
library(extrafont)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(scales)
library(ggTimeSeries)

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Source Sans Pro", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Source Sans Pro", colour = "grey40", size = 10),
      plot.caption = element_text(face = "italic"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      axis.ticks = element_line(size = 0.5, color = "grey40"),
      axis.ticks.length = unit(.15, "cm"),
      axis.title = element_text(size = 8, colour = "grey20"),
      legend.position = 'none')
}

# leitura dos dados -------------------------------------------------------

# leitura dos cabeçalhos
tabela_leiloes <- read_excel("../Anexo_RMD_Dez_18.xlsx", sheet = "1.4", skip = 4)

ajusta_cabecalhos <- data.frame("linha1" = names(tabela_leiloes), "linha2" = t(tabela_leiloes[1,])) %>%
  mutate_all(as.character) %>%
  mutate(parte1 = ifelse(str_detect(linha1, "\\.\\."), NA, linha1)) %>%
  fill(parte1) %>%
  replace_na(list(linha2 = "")) %>%
  mutate(titulos = paste(parte1, linha2))

names(tabela_leiloes) <- ajusta_cabecalhos$titulos

dados <- tabela_leiloes %>%
  filter(row_number() > 1) %>%
  rename(data_emissao   = `Data de Emissão `,
         taxa_media     = `Taxas (% a.a.) (b) Média`,
         demanda       = `Demanda / Oferta `,
         titulo         = `Título `,
         valor_venda    = `Venda\r\nR$ Milhões `,
         vencimento     = `Data de Vencimento `) %>%
  mutate(titulo_vcto    = paste(titulo, vencimento),
         taxa_media     = as.numeric(taxa_media))

ggplot(dados, aes(y = taxa_media, x = vencimento, color = titulo, size = demanda)) + geom_point(alpha = 0.5) 

ggplot(dados, aes(y = vencimento, x = data_emissao, color = titulo, size = valor_venda)) + geom_point(alpha = 0.5) 

ggplot(dados, aes(y = vencimento, x = data_emissao, color = titulo, size = taxa_media)) + geom_point(alpha = 0.5) 

ggplot(dados, aes(y = taxa_media, x = data_emissao, color = titulo, size = vencimento)) + geom_point(alpha = 0.5) 

ggplot(dados, aes(y = taxa_media, x = data_emissao, color = titulo, size = demanda, alpha = vencimento)) + geom_point() + scale_alpha_discrete(rev)

ggplot(dados %>% filter(vencimento < "2030-01-01"), aes(y = taxa_media, x = vencimento, color = titulo, size = valor_venda)) + geom_point()


ggplot_calendar_heatmap(
  dados %>% filter(titulo_vcto == "LTN 2019-04-01"),
  'data_emissao',
  'taxa_media',
  monthBorderSize = 0.75,
  dayBorderColour = "lightgrey"
)   +
  xlab(NULL) + 
  ylab(NULL) + 
  tema() +
  labs(
    fill = "Saldo (em R$ milhões)"
  )





