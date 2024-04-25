#tranformar colunas que estavam com NAs em numéricas
base_final$saeb_mat <- as.numeric(base_final$saeb_mat)
base_final$saeb_port <- as.numeric(base_final$saeb_port)


# Calcular a média de saepe_port e saepe_mat
base_final$saepe_total <- (base_final$saepe_port + base_final$saepe_mat) / 2

# Calcular a média de saeb_port e saeb_mat
base_final$saeb_total <- (base_final$saeb_port + base_final$saeb_mat) / 2

summary(base_final$saepe_total)
summary(base_final$saeb_total)

# Calcular a média de saeb_total e saepe_total
base_final$educacao <- (base_final$saepe_total + base_final$saeb_total) / 2
summary(base_final$educacao)


# Gráfico de dispersão de saepe_total versus itmpe_17
plot(base_final$saepe_total, base_final$itmpe_17,
     xlab = "saepe_total", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saepe_total vs. itmpe_17")




# Criar colunas de referência para o gráfico para fazer os quadrantes
base_final$transparencia <- ifelse(base_final$itmpe_17 > 523.5, "Sim", "Não")
base_final$educ_saepe <- ifelse(base_final$saepe_total > 297.7, "Sim", "Não")
base_final$educ_saeb <- ifelse(base_final$saeb_total > 213.4, "Sim", "Não")
base_final$educacao_cat <- ifelse(base_final$educacao > 254.5, "Sim", "Não")

summary(base_final$itmpe_17)

# Verificando e criando a coluna "quadrantes"
base_final$quadrantes_saepe <- ifelse(base_final$transparencia == "Sim" & base_final$educ_saepe == "Sim", 
                                "Alta transparencia e educação",
                                ifelse(base_final$transparencia == "Sim" & base_final$educ_saepe == "Não",
                                       "Alta transparencia e baixa educação",
                                       ifelse(base_final$transparencia == "Não" & base_final$educ_saepe == "Sim",
                                              "Baixa transparencia e alta educação",
                                              "Baixa transparencia e educação"
                                       )
                                )
)

base_final$quadrantes_saeb <- ifelse(base_final$transparencia == "Sim" & base_final$educ_saeb == "Sim", 
                                "Alta transparencia e educação",
                                ifelse(base_final$transparencia == "Sim" & base_final$educ_saeb == "Não",
                                       "Alta transparencia e baixa educação",
                                       ifelse(base_final$transparencia == "Não" & base_final$educ_saeb == "Sim",
                                              "Baixa transparencia e alta educação",
                                              "Baixa transparencia e educação"
                                       )
                                )
)

base_final$quadrantes_total <- ifelse(base_final$transparencia == "Sim" & base_final$educacao_cat == "Sim", 
                                     "Alta transparencia e educação",
                                     ifelse(base_final$transparencia == "Sim" & base_final$educacao_cat == "Não",
                                            "Alta transparencia e baixa educação",
                                            ifelse(base_final$transparencia == "Não" & base_final$educacao_cat == "Sim",
                                                   "Baixa transparencia e alta educação",
                                                   "Baixa transparencia e educação"
                                            )
                                     )
)

#criando coluna se houve reeleição
base_final$houve_reeleicao <- ifelse(base_final$codigo_mun %in% numeros, "Sim", "Não")

#inserindo a distancia das cidades de recife
base_final <- merge(base_final, dist_rec, by = "codigo_mun", all.x = TRUE)
base_final <- subset(base_final, select = -municipio.y)
names(base_final)[names(base_final) == "quadrantes"] <- "quadrantes_saepe"
names(base_final)[names(base_final) == "municipio.x"] <- "municipio"
