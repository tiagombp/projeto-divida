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
library(scales)
loadfonts()

### aproveitando o RData da execução orçamentária

load("../exec_orcam.RData")

vazamento <- read_excel("vazamento.xlsx", skip = 9)
names(vazamento) <- c("Ano", "OutrasDesp")

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
  ungroup() %>%
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

dados_plotly <- dados_nec_fin %>%
  mutate(
    serie = case_when(
      Classificador == "Encargos Bacen" ~ "Encargos Bacen",
      Classificador == "Externa" ~ "Externa",
      Classificador == "Interna" ~ as.character(MovFin))
  ) %>%
  group_by(Ano, serie) %>%
  summarise(Valor = sum(Valor)) %>%
  ungroup() %>%
  spread(key = serie, value = Valor) %>%
  left_join(vazamento, by = "Ano") %>%
  mutate(Interna       = `Juros e Encargos` + Principal,
         Vencimentos   = Interna + Externa + `Encargos Bacen`,
         graf_Juros    = Principal + `Juros e Encargos`,
         graf_DivEx    = graf_Juros + Externa,
         graf_EncBCB   = graf_DivEx + `Encargos Bacen`,
         graf_OutDesp  = graf_EncBCB + OutrasDesp,
         pct_OD_NF     = percent(OutrasDesp / (OutrasDesp + Vencimentos), decimal.mark = ","),
         pct_Vct_NF    = percent(Vencimentos / (OutrasDesp + Vencimentos), decimal.mark = ","),
         pct_EncBc_Vct = percent(`Encargos Bacen` / Vencimentos, decimal.mark = ","),
         pct_DivEx_Vct = percent(Externa / Vencimentos, decimal.mark = ","),
         pct_DivIn_Vct = percent(Interna / Vencimentos, decimal.mark = ","),
         pct_Jur_Vct   = percent(`Juros e Encargos` / Vencimentos, decimal.mark = ","),
         pct_Jur_Int   = percent(`Juros e Encargos` / Interna, decimal.mark = ","),
         pct_Princ_Vct = percent(Principal / Vencimentos, decimal.mark = ","),
         pct_Princ_Int = percent(Principal / Interna, decimal.mark = ",")
         ) %>%
  mutate_at(vars(-Ano, -contains("pct_")), .funs = ~./1e9)
  
formata <- function(x){
  paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ","), " bi")
}

graf1 <- 
  plot_ly(dados_plotly, x = ~Ano, type = "scatter", fill = 'tozeroy', y = ~graf_OutDesp, 
        name = "Outras Despesas", mode = "none", stackgroup = 'one', fillcolor = cores_pasteis[1],
        text = ~paste0("<em>Outras Despesas", " (", Ano, "): ", formata(OutrasDesp), 
                       "</em><br />", pct_OD_NF, " das Necessidades de Financiamento"),
        hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline"))%>%
  
  add_trace(y = ~Vencimentos, name = "Vencimentos DPF", fillcolor = cores_pasteis[2], line = list(color = "white", width = 1, shape = "spline"),
            text = ~paste0("<em>Vencimentos", " (", Ano, "): ", formata(Vencimentos),
                           "</em><br />", pct_Vct_NF, " das Necessidades de Financiamento"),
            hoverinfo = "text") %>%
  layout(xaxis = list(title = "", showgrid = FALSE),
         yaxis = list(title = "R$ bi", showgrid = FALSE, zerolinecolor = "white"))



graf2 <- 
  plot_ly(dados_plotly, x = ~Ano, type = "scatter", fill = 'tozeroy', y = ~graf_OutDesp, 
          name = "Outras Despesas", mode = "none", stackgroup = 'one', fillcolor = "lightgray",
          text = ~paste0("Outras Despesas", " (", Ano, "): ", formata(OutrasDesp)),
          hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline"))%>%
  
  add_trace(y = ~graf_EncBCB, name = "Encargos Bacen", fillcolor = cores_pasteis[3], color = "white",
            text = ~paste0("<em>Encargos Bacen", " (", Ano, "): ", formata(`Encargos Bacen`),
                           "</em><br />", pct_EncBc_Vct, " dos Vencimentos"),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  
  add_trace(y = ~graf_DivEx, name = "Dívida Externa", fillcolor = cores_pasteis[4],
            text = ~paste0("<em>Dívida Externa", " (", Ano, "): ", formata(Externa),
                           "</em><br />", pct_DivEx_Vct, " dos Vencimentos"),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  
  add_trace(y = ~Interna, name = "Dívida Interna", fillcolor = cores_pasteis[5],
            text = ~paste0("<em>Dívida Interna", " (", Ano, "): ", formata(Interna),
                           "</em><br />", pct_DivIn_Vct, " dos Vencimentos"),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  layout(xaxis = list(title = "", showgrid = FALSE),
         yaxis = list(title = "R$ bi", showgrid = FALSE, zerolinecolor = "white"))



graf3 <- 
  plot_ly(dados_plotly, x = ~Ano, type = "scatter", fill = 'tozeroy', y = ~graf_OutDesp, 
          name = "Outras Despesas", mode = "none", stackgroup = 'one', fillcolor = "lightgray",
          text = ~paste0("Outras Despesas", " (", Ano, "): ", formata(OutrasDesp)),
          hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline"))%>%
  add_trace(y = ~graf_EncBCB, name = "Encargos Bacen", fillcolor = "lightgray",
            text = ~paste0("Encargos Bacen", " (", Ano, "): ", formata(`Encargos Bacen`)),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  add_trace(y = ~graf_DivEx, name = "Dívida Externa", fillcolor = "lightgray",
            text = ~paste0("Dívida Externa", " (", Ano, "): ", formata(Externa)),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  
  add_trace(y = ~graf_Juros, name = "Juros", fillcolor = cores_pasteis[6],
            text = ~paste0("<em>Juros", " (", Ano, "): ", formata(`Juros e Encargos`),
                           "</em><br />", pct_Jur_Vct, " dos Vencimentos",
                           "<br />", pct_Jur_Int, " da Dívida Interna"),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  
  add_trace(y = ~Principal, name = "Principal", fillcolor = cores_pasteis[7],
            text = ~paste0("<em>Principal", " (", Ano, "): ", formata(Principal),
                           "</em><br />", pct_Princ_Vct, " dos Vencimentos",
                           "<br />", pct_Princ_Int, " da Dívida Interna"),
            hoverinfo = "text", line = list(color = "white", width = 1, shape = "spline")) %>%
  layout(xaxis = list(title = "", showgrid = FALSE),
         yaxis = list(title = "R$ bi", showgrid = FALSE, zerolinecolor = "white"))
  
save(dados_plotly, graf1, graf2, graf3, formata, file = "nec_fin.RData")
  
  

