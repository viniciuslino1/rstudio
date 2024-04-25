# Regressão linear para saeb_port
modelo_saeb_port <- lm(itmpe_17 ~ saeb_port, data = base_final)
summary(modelo_saeb_port)

# Regressão linear para saeb_mat
modelo_saeb_mat <- lm(itmpe_17 ~ saeb_mat, data = base_final)
summary(modelo_saeb_mat)

# Regressão linear para saepe_port
modelo_saepe_port <- lm(itmpe_17 ~ saepe_port, data = base_final)
summary(modelo_saepe_port)

# Regressão linear para saepe_mat
modelo_saepe_mat <- lm(itmpe_17 ~ saepe_mat, data = base_final)
summary(modelo_saepe_mat)

#Regressão mulvariada com todas elas
modelo_todas <- lm(itmpe_17 ~ saepe_mat + saepe_port + saeb_mat + saeb_port, data = base_final)
summary(modelo_todas)

# Gráfico de dispersão para saeb_port versus itmpe_17
plot(base_final$saeb_port, base_final$itmpe_17, 
     xlab = "saeb_port", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saeb_port vs. itmpe_17")

# Gráfico de dispersão para saeb_mat versus itmpe_17
plot(base_final$saeb_mat, base_final$itmpe_17, 
     xlab = "saeb_mat", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saeb_mat vs. itmpe_17")

# Gráfico de dispersão para saepe_port versus itmpe_17
plot(base_final$saepe_port, base_final$itmpe_17, 
     xlab = "saepe_port", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saepe_port vs. itmpe_17")

# Gráfico de dispersão para saepe_mat versus itmpe_17
plot(base_final$saepe_mat, base_final$itmpe_17, 
     xlab = "saepe_mat", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saepe_mat vs. itmpe_17")


library(dplyr)
library(ggplot2)
# Definir os números para filtrar
numeros <- c(2600054, 2600104, 2600302, 2600500, 2600906, 2601052, 2601201, 2601904, 2602100, 2602308, 2602506, 2602902, 2603207, 2603306, 2603603, 2603702, 2603801, 2603900, 2604700, 2604809, 2605152, 2605202, 2606002, 2606606, 2606705, 2606804, 2607109, 2607208, 2607703, 2607950, 2608008, 2608107, 2608404, 2608503, 2608602, 2608800, 2609105, 2609154, 2609709, 2609808, 2610301, 2610707, 2611002, 2611507, 2611533, 2611606, 2611705, 2612000, 2612307, 2612455, 2612471, 2612505, 2612703, 2612901, 2613008, 2613206, 2613305, 2613800, 2613909, 2614006, 2614204, 2614402, 2614600, 2614808, 2615201, 2615508, 2615607, 2615706, 2616001, 2616209, 2616506)

# Criar base filtrada
base_reeleitos <- base_final %>% 
  filter(codigo_mun %in% numeros)

# Regressão linear para saeb_port
modelo_saeb_port_rel <- lm(itmpe_17 ~ saeb_port, data = base_reeleitos)
summary(modelo_saeb_port_rel)

# Regressão linear para saeb_mat
modelo_saeb_mat_rel <- lm(itmpe_17 ~ saeb_mat, data = base_reeleitos)
summary(modelo_saeb_mat_rel)

# Regressão linear para saepe_port
modelo_saepe_port_rel <- lm(itmpe_17 ~ saepe_port, data = base_reeleitos)
summary(modelo_saepe_port_rel)

# Regressão linear para saepe_mat
modelo_saepe_mat_rel <- lm(itmpe_17 ~ saepe_mat, data = base_reeleitos)
summary(modelo_saepe_mat_rel)

#Regressão mulvariada com todas elas
modelo_todas <- lm(itmpe_17 ~ saepe_mat + saepe_port + saeb_mat + saeb_port, data = base_final)
summary(modelo_todas)



reg_saepe_mat <- lm(itmpe_17 ~ saepe_mat, data = base_final)
reg_saepe_port <- lm(itmpe_17 ~ saepe_port, data = base_final)
reg_saeb_mat <- lm(itmpe_17 ~ saeb_mat, data = base_final)
reg_saeb_port <- lm(itmpe_17 ~ saeb_port, data = base_final)

#Gera tabela dos modelos de regressão no console e coloca ela como data frame

a <- export_summs(reg_saepe_mat, reg_saepe_port, reg_saeb_mat,
             reg_saeb_port,scale = FALSE, error_format = "(p = {p.value})")

tabela_regressões <- export_summs(reg_saepe_mat, reg_saepe_port, reg_saeb_mat,
                                  reg_saeb_port,scale = TRUE, error_format = "(p = {p.value})")
linhas_para_manter2 <- !(rownames(tabela_regressões_sem_linhas) %in% c("1", "1"))
tabela_regressões_sem_linhas <- tabela_regressões_sem_linhas[linhas_para_manter2, ]

names(tabela_regressões_sem_linhas)[names(tabela_regressões_sem_linhas) == "Model 1"] <- "Matemática SAEPE"
names(tabela_regressões_sem_linhas)[names(tabela_regressões_sem_linhas) == "Model 2"] <- "Português SAEPE"
names(tabela_regressões_sem_linhas)[names(tabela_regressões_sem_linhas) == "Model 3"] <- "Matemática SAEB"
names(tabela_regressões_sem_linhas)[names(tabela_regressões_sem_linhas) == "Model 4"] <- "Português SAEB"
names(tabela_regressões_sem_linhas)[names(tabela_regressões_sem_linhas) == "names"] <- "Modelos"

tabela_regressões_sem_linhas <- tabela_regressões_sem_linhas[-1,]
tabela_regressões_sem_linhas %>%
  gt() %>%
  tab_header(
    title = "Tabela de Modelos de Regressão",
    subtitle = glue::glue("Relação entre o desempenho da escolas municipais de Pernambuco nas provas do SAEPE e SAEB com o Indice de Transparência do TCE-PE em 2016")
  ) %>%
  tab_footnote(
    footnote = "Todos os preditores contínuos são centrados na média e dimensionados em 1 desvio padrão. 
    A variável dependente está em suas unidades originais.  *** p < 0.001;  ** p < 0.01;  * p < 0.05."
  ) %>%
  fmt_number(decimals = 2)

tabela_regressao %>% gtsave(filename = "tabela_regressao.png")



modelo_inverso <- lm(saepe_port ~ itmpe_17, data = base_final)
summary(modelo_inverso)
