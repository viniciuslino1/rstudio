library(jtools)
library(huxtable)
library(officer)
library(gt)
library(gapminder)
library(tidyverse)
library(ggplot2)

#Gera tabela dos modelos de regressão no console e coloca ela como data frame
a <- export_summs(modelo_saepe_port_rel, modelo_saepe_mat_rel, modelo_saeb_mat_rel,
             modelo_saeb_port_rel,
             scale = TRUE, error_format = "(p = {p.value})")
#limpeza remove primeira e última linha
linhas_para_manter <- !(rownames(a) %in% c("1", ".1"))
df_sem_linhas <- a[linhas_para_manter, ]

#Gerar figura da tabela de regressão ver https://gt.rstudio.com/
df_sem_linhas %>%
  gt() %>%
  tab_header(
    title = "Modelos de Regressão",
    subtitle = glue::glue("a")
  ) %>%
  tab_footnote(
    footnote = "Todos os preditores contínuos são centrados na média e dimensionados em 1 desvio padrão. 
    A variável dependente está em suas unidades originais.  *** p < 0.001;  ** p < 0.01;  * p < 0.05."
  )






prop$`% do corpus (γ)` <- prop$`% do corpus (γ)` * 100

  
tabelprop <- prop %>%
  gt(groupname_col = "agreg") %>%
  tab_header(
    title = md("**Tabela 1: Tópicos do STM e Agregação - Proposições**")
  ) %>%
  tab_footnote(
    footnote = "Fonte: elaboração própria"
  ) %>%
  summary_rows(
    columns = c(`% do corpus (γ)`, `nº de documentos`),
    fns = list(label = md("**Total Agregação**") , fn = "sum"),
    fmt = NULL,
    side = "bottom",
    missing_text = "",
    formatter = NULL
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    summary_row.background.color = "gray95",
    row_group.background.color = "#FFEFDB",
  )

tabelprop %>% gtsave(filename = "tabelprop3.png", vwidth = 1500,
                     vheight = 4500)

decr$`% do corpus (γ)` <- decr$`% do corpus (γ)` * 100

tabeldecr <- decr %>%
  gt(groupname_col = "agreg") %>%
  tab_header(
    title = md("**Tabela 2: Tópicos do STM e Agregação - Decretos**")
  ) %>%
  tab_footnote(
    footnote = "Fonte: elaboração própria"
  ) %>%
  summary_rows(
    columns = c(`% do corpus (γ)`, `nº de documentos`),
    fns = list(label = md("**Total Agregação**") , fn = "sum"),
    fmt = NULL,
    side = "bottom",
    missing_text = "",
    formatter = NULL
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    summary_row.background.color = "gray95",
    row_group.background.color = "#FFEFDB",
  )

tabeldecr %>% gtsave(filename = "tabeldecr.png", vwidth = 1500,
                     vheight = 4500)


tabelmun <- na.omit(base_final) %>%
  select (municipio, itmpe_17, saeb_total, saepe_total, educacao, quadrantes_total, houve_reeleicao, dist_rec) %>%
  gt(groupname_col = "quadrantes_total",
     rowname_col = "municipio") %>%
  tab_header(
    title = md("**Tabela: Municípios e sua divisão de quadrantes**")
  ) %>%
  tab_footnote(
    footnote = "Fonte: elaboração própria"
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    summary_row.background.color = "gray95",
    row_group.background.color = "#FFEFDB",
  )

tabelmun

tabelmun %>% gtsave("tabmun.docx")
