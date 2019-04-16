library(tidyverse)
library(viridis)
library(extrafont)
library(RColorBrewer)
library(readxl)
library(lubridate)
library(scales)

tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(family = "Source Sans Pro", colour = "grey20"),
      title = element_text(face = "bold", size = 10, color = "#1E4C7A"), 
      plot.subtitle = element_text(family = "Source Sans Pro", 
                                   color = "grey20", face = "plain", size = 10),
      axis.text = element_text(family = "Source Sans Pro", colour = "grey40", size = 12),
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

radical_nome_arq <- "Anexo_RMD_"
#ano_atu <- 2019
meses <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")


# DPF ---------------------------------------------------------------------

tabela_dpf <- read_excel("Anexo_RMD_Fev_19.xlsx", sheet = "2.1", skip = 4)

ano_atu <- as.numeric(paste0("20", str_sub(names(tabela_dpf)[length(names(tabela_dpf))], 5, 6)))

dados_dpf <- tabela_dpf %>% 
  rename(rotulos = 1) %>%
  filter(rotulos == "DPF EM PODER DO PÚBLICO") %>%
  select(-1) %>%
  gather(key = Mes_Ano, value = Valor) %>%
  separate(Mes_Ano, into = c("Mes", "Ano"), sep = "/") %>%
  mutate(Ano = as.numeric(paste0('20', Ano)),
         Mes = match(Mes, meses),
         Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes, 2), "-",
                                  "01")),
         Valor = as.numeric(Valor)/1e3,
         Agrupador = "Estoque Nominal",
         Indicador = "Estoque DPF",
         Unidade = "R$ tri") %>%
  filter(Ano >= ano_atu - 1)


# Composição --------------------------------------------------------------

tabela_composicao <- read_excel("Anexo_RMD_Fev_19.xlsx", sheet = "2.4", skip = 4)

tipos_titulos <- c("Prefixado",	"Índice Preços", "Taxa Flutuante",	"Câmbio",	"Demais",	"Total")

dados_composicao <- tabela_composicao %>%
  rename(Periodo = `Mês`) %>%
  filter(!is.na(Periodo)) %>%
  gather(-Periodo, key = rotulos, value = Valor) %>%
  mutate(Indicador = ifelse(rotulos %in% tipos_titulos, rotulos, NA),
         Unidade  = ifelse(is.na(Indicador), "Percentual", "Saldo"),
         Periodo = as.Date(Periodo),
         Mes = month(Periodo),
         Ano = year(Periodo),
         Agrupador = "Composição") %>%
  fill(Indicador) %>%
  select(-rotulos) %>%
  filter(Unidade == "Percentual", 
         !(Indicador %in% c("Demais", "Total"))) %>%
  filter(Ano >= ano_atu - 1)


# Vencimentos -------------------------------------------------------------

tabela_vencimentos <- read_excel("Anexo_RMD_Fev_19.xlsx", sheet = "3.1", skip = 6)

tipos_vencimentos <- c("Até 12 meses", "De 1 a 2 anos", "De 2 a 3 anos", "De 3 a 4 anos", "De 4 a 5 anos", "Acima de 5 anos", "Total") 

posicao_DPMFi <- which(tabela_vencimentos[1] == "DPMFi")

dados_vencimentos <- tabela_vencimentos %>%
  filter(row_number() < posicao_DPMFi) %>%
  rename(Periodo = `Mês`) %>%
  filter(!is.na(Periodo)) %>%
  gather(-Periodo, key = rotulos, value = Valor) %>%
  mutate(Indicador = ifelse(rotulos %in% tipos_vencimentos, rotulos, NA),
         Unidade  = ifelse(is.na(Indicador), "Percentual", "Saldo"),
         Periodo = as.Date(as.numeric(Periodo), origin = "1899-12-30"),
         Mes = month(Periodo),
         Ano = year(Periodo),
         Valor = as.numeric(Valor),
         Agrupador = "Estrutura de Vencimentos") %>%
  fill(Indicador) %>%
  select(-rotulos) %>%
  filter(Unidade == "Percentual", 
         Indicador == "Até 12 meses") %>%
  mutate(Indicador = "% Vincendo em 12 meses") %>%
  filter(Ano >= ano_atu - 1)


# Prazo Médio -------------------------------------------------------------

tabela_prazo_medio <- read_excel("Anexo_RMD_Fev_19.xlsx", sheet = "3.7", skip = 4)

dados_prazo_medio <- tabela_prazo_medio %>% 
  rename(rotulos = 1) %>%
  filter(rotulos == "DPF") %>%
  select(-1) %>%
  gather(key = Mes_Ano, value = Valor) %>%
  separate(Mes_Ano, into = c("Mes", "Ano"), sep = "/") %>%
  mutate(Ano = as.numeric(paste0('20', Ano)),
         Mes = match(Mes, meses),
         Periodo = as.Date(paste0(Ano, "-",
                                  str_pad(Mes, 2), "-",
                                  "01")),
         Valor = as.numeric(Valor),
         Agrupador = "Estrutura de Vencimentos",
         Indicador = "Prazo Médio (anos)",
         Unidade = "Anos") %>%
  filter(Ano >= ano_atu - 1)


# juntando todos ----------------------------------------------------------


dados_grandes_numeros <- bind_rows(
  dados_dpf,
  dados_composicao,
  dados_vencimentos,
  dados_prazo_medio)

dados_PAF <- read_excel("dados_PAF.xlsx")

base_GN <- dados_grandes_numeros %>%
  left_join(dados_PAF) %>%
  mutate(dentro_fora = ifelse((Valor >= PAFmin) & (Valor <= PAFmax), "Dentro", "Fora"),
         mes_texto = meses[Mes])


# gera gráficos e salva RData ---------------------------------------------
ultima_data <- max(base_GN$Periodo)

gera_graf <- function(indicador) {
  base <- base_GN %>% filter(Indicador == indicador)
  
  graf_basico <- ggplot(base, aes(x = reorder(mes_texto, Mes), group = Ano, y = Valor)) +
    geom_ribbon(aes(ymin = PAFmin, ymax = PAFmax), fill = "lightcyan") +
    geom_hline(yintercept = base$PAFmin[1], 
               color = "SteelBlue", size = 0.5, linetype = "dotted") +
    geom_hline(yintercept = base$PAFmax[1], 
               color = "SteelBlue", size = 0.5, linetype = "dotted") +
    geom_line(aes(y = ifelse(Ano == ano_atu, Valor, NA), color = dentro_fora),
              size = 1) +
    geom_line(aes(y = ifelse(Ano == ano_atu-1, Valor, NA)),
              color = "grey") +
    # geom_text(aes(label = ifelse(Mes == 1, 
    #                              Ano, 
    #                              NA),
    #               x = Mes - 0.3), color = "darkgrey", 
    #           family = "Source Sans Pro", size = 3.5, hjust = "right", fontface = "italic",
    #           check_overlap = TRUE) +
    # geom_text(aes(label = ifelse(Mes == 1, 
    #                              paste("Intervalo\nPAF", ano_atu), 
    #                              NA),
    #               y = PAFmin + (PAFmax - PAFmin)/2,
    #               x = Mes - 0.1), color = "SteelBlue", 
    #           family = "Source Sans Pro", size = 3.5, hjust = "right", fontface = "italic",
    #           check_overlap = TRUE) +
    geom_point(aes(color = ifelse(Ano == ano_atu, dentro_fora, NA)), size = 3) +
    geom_point(aes(color = ifelse(Periodo == ultima_data, dentro_fora, NA)), size = 6, shape = 1) +
    scale_color_manual(values = c("Dentro" = "steelblue", "Fora" = "firebrick")) +
    scale_fill_manual(values = c("Dentro" = "steelblue", "Fora" = "firebrick")) +
    scale_x_discrete(labels = str_sub(meses, 1, 1)) +
    labs(x = NULL, y = NULL) +
    #scale_x_discrete(expand = expand_scale(add = c(1,1))) +
    tema()
  
  unidade <- base$Unidade[1]
  
  if (unidade == "R$ tri") {
    graf_final <- graf_basico +
      # geom_text(aes(label = ifelse(Periodo == max(base_GN$Periodo), 
      #                              format(round(Valor,0), big.mark = ".", decimal.mark = ","), 
      #                              NA),
      #               color = dentro_fora,
      #               x = Mes + 0.2), family = "Source Sans Pro", hjust = "left", size = 5) +
      scale_y_continuous(position = "right", 
                         labels = function(x) {format(x,
                                                      accuracy = 0.1,
                                                      big.mark = ".", 
                                                      decimal.mark=",", 
                                                      scientific = FALSE)})
  } else if (unidade == "Percentual") {
    graf_final <- graf_basico +
      # geom_text(aes(label = ifelse(Periodo == max(base_GN$Periodo), 
      #                              percent(Valor), 
      #                              NA),
      #               color = dentro_fora,
      #               x = Mes + 0.2), family = "Source Sans Pro", hjust = "left", size = 5) +
      scale_y_continuous(position = "right", labels = function(x) {percent(x,
                                                                           accuracy = 1, 
                                                                           decimal.mark=",")})
  } else if (unidade == "Anos") {
    graf_final <- graf_basico +
      # geom_text(aes(label = ifelse(Periodo == max(base_GN$Periodo), 
      #                              paste(format(round(Valor,2), big.mark = ".", decimal.mark = ","), "anos"), NA), 
      #               color = dentro_fora,
      #               x = Mes + 0.2), family = "Source Sans Pro", hjust = "left", size = 5) +
      scale_y_continuous(position = "right")    
  }
  
  return(graf_final)
  
}

obtem_ultimo_valor <- function(indicador) {
  base <- base_GN %>% filter(Indicador == indicador, Periodo == ultima_data)
  
  if (base$Unidade[1] == "Percentual") 
    valor <- scales::percent(round(base$Valor[1],3), decimal.mark = ",")
  else if (base$Unidade[1] == "Anos")
    valor <- paste(format(round(base$Valor[1],2), big.mark = ".", decimal.mark = ","), "anos")
  else
    valor <- paste("R$", format(round(base$Valor[1],2), big.mark = ".", decimal.mark = ","), "bi")
  return(valor)
}

indicadores <- unique(base_GN$Indicador)

lista_graficos <- purrr::map(indicadores, gera_graf)
names(lista_graficos) <- indicadores

lista_valores <- purrr::map(indicadores, obtem_ultimo_valor)
names(lista_valores) <- indicadores

#save(lista_graficos, lista_valores, base_GN, ultima_data, file = "GN.RData")
save(tema, gera_graf, ano_atu, meses, lista_valores, base_GN, ultima_data, file = "GN.RData")

#gera_graf("Estoque DPF")
#gera_graf("Prefixado")
#obtem_ultimo_valor("Prefixado")
#obtem_unidade("Prefixado")

#gera_graf("Estoque DPF") + coord_cartesian(clip = 'off') + annotate(geom = "rect", ymin = 3, ymax = Inf, xmin = 1.9, xmax = 2.1, fill = "yellow", alpha = 0.5) 

#unique(base_GN$Unidade)
