# Definir os valores para a interseção dos eixos
intersecao_x <- 297.7
intersecao_y <- 523.5
intersecao_x_saeb <- 213.4
intersecao_x_educacaototal <- 254.5

# Criar o gráfico de dispersão
plot(base_reeleitos$saepe_total, base_reeleitos$itmpe_17,
     xlab = "saepe_total", ylab = "itmpe_17",
     main = "Gráfico de Dispersão: saepe_total vs. itmpe_17",
     )  # Limitar os eixos para criar os quadrantes

# Adicionar linhas verticais e horizontais para mover os eixos
abline(v = intersecao_x, col = "red")  # linha vertical para o eixo y
abline(h = intersecao_y, col = "blue") # linha horizontal para o eixo x

# Adicionar legendas para as linhas
legend("topright", legend = c("saepe_total = 297.7", "itmpe_17 = 523.5"),
       col = c("red", "blue"), lty = 1, cex = 0.8)

library(ggplot2)


# Criar o gráfico com nomes das cidades
grafico_todos_nm_municipio <- ggplot(base_final, aes(x = (saepe_port + saepe_mat) / 2, y = itmpe_17, color = quadrantes, label = municipio)) +
  geom_text(size = 3) +
  labs(x = "Proeficiência Média SAEPE", y = "Indice de Transparência de Municípios de Pernambuco",
       title = "Transparência x Prova Saepe") +
  xlim(250, 350) + ylim(300, 800) +  # Limitar os eixos para criar os quadrantes
  geom_vline(xintercept = intersecao_x, color = "black") +  # Linha vertical para o eixo y
  geom_hline(yintercept = intersecao_y, color = "black") + theme_minimal()

grafico_todos_nm_municipio
#salvar ultimo gráfico plotado
ggsave(
  "grafico_todos_pontos.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# Criar o gráfico de dispersão com nomes os municipios

ggplot(na.omit(base_final), aes(x = educacao, y = itmpe_17, color = quadrantes_total, label = municipio)) +
  geom_point() +
  labs(x = "Média da Proficiência no SAEB e SAEPE", y = "Indice de Transparência de Municípios de Pernambuco",
       title = "Transparência x Desempenho Educação Municipal (2016)", color = "") +
  xlim(210, 310) + ylim(0, 830) +  # Limitar os eixos para criar os quadrantes
  geom_vline(xintercept = intersecao_x_educacaototal, color = "black") +  # Linha vertical para o eixo y
  geom_hline(yintercept = intersecao_y, color = "black") + 
  annotate("text", x = intersecao_x_educacaototal + 10, y = intersecao_y + 300,
           label = "Mediana = 254.5", color = "black", size = 2, family = "mono", fontface = "bold") +  # Anotação para a linha vertical
  annotate("text", x = intersecao_x_educacaototal + 50, y = intersecao_y - 20,
           label = "Mediana = 523.5", color = "black", size = 2, family = "mono", fontface = "bold") +
  theme_minimal()

ggsave(
  "grafico_pontos_total.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 600,
  plot = last_plot())

#Gráfico filtrado

na.omit(base_final) %>%
  filter(houve_reeleicao == "Sim", dist_rec < 100) %>%
ggplot(aes(x = educacao, y = itmpe_17, color = quadrantes_total, label = municipio)) +
  geom_text(size = 3, family = "mono", fontface = "bold") +
  labs(x = "Média da Proeficiência no SAEB e SAEPE", y = "Indice de Transparência de Municípios de Pernambuco",
       title = "Transparência x Desempenho Educação Municipal (2016) - Cidades onde houve Reeleição em 2016 até 100km de Recife", color = "") +
  xlim(210, 310) + ylim(0, 830) +  # Limitar os eixos para criar os quadrantes
  geom_vline(xintercept = intersecao_x_educacaototal, color = "black") +  # Linha vertical para o eixo y
  geom_hline(yintercept = intersecao_y, color = "black") + 
  annotate("text", x = intersecao_x_educacaototal + 10, y = intersecao_y + 300,
           label = "Mediana = 254.5", color = "black", size = 2, family = "mono", fontface = "bold") +  # Anotação para a linha vertical
  annotate("text", x = intersecao_x_educacaototal + 50, y = intersecao_y - 20,
           label = "Mediana = 523.5", color = "black", size = 2, family = "mono", fontface = "bold") +
  theme_minimal() +
  theme(plot.title = element_text(size = 8, family = "mono", face = "bold"))


ggsave(
  "grafico_nomes_houve_reeleicao_100km.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 600,
  plot = last_plot())


base_final %>%
  top_n(20, itmpe_17) %>%
  ggplot(aes(x = reorder(municipio, desc(itmpe_17)), y = itmpe_17, fill = municipio)) +
  geom_bar(stat = "identity", fill = "#189AB4") +
  labs(x = "Município", y = "Indice de Transparência de Municípios do TCE-PE",
       title = "Ranking 20 Municípios - ITMPE 2017") +
  geom_text(aes(label = round(itmpe_17)), 
            vjust = 1.5, hjust =  0.5,
            size = 3, color = "#FFFFFF", fontface = "bold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

pal <- ghibli_palette(name = "LaputaMedium", n = 20, type = "continuous")

ggsave(
  "grafico_barras_top20_itmpe17.png",
  bg = "white",
  width = 10,
  height = 8,
  dpi = 1200,
  plot = last_plot())





base_final_long <- pivot_longer(base_final, cols = c(saeb_mat, saeb_port, saepe_mat, saepe_port), names_to = "variavel", values_to = "valor")
library(tidyr)
library(ggplot2)

base_final_long %>%
  top_n(80, itmpe_17) %>%
  ggplot(aes(x = reorder(municipio, desc(itmpe_17)), y = valor, color = variavel, group = variavel)) +
  geom_line(size = 1.0, alpha = 0.5) +
  geom_point(size = 2) +
  labs(x = "Município", y = "", title = "Desempenho Municipal nas Provas do SAEB 2015 e 
       SAEPE 2016 (Top 20 ranking de transparência do ITMPE)", color = "") +
  scale_color_manual(values = c("saeb_mat" = "#569FF0", "saeb_port" = "#00468A", 
                                "saepe_mat" = "#FF8E86", "saepe_port" = "#BB001C")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  theme(plot.title = element_text(size = 12, family = "sans", face = "bold"))

ggsave(
  "grafico_linhas_saeb_saepe.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 1200,
  plot = last_plot())

ggplot(base_final, aes(x = itmpe_17)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +  # Definir a largura do intervalo da barra, a cor de preenchimento, a cor da borda e a transparência
  labs(x = "Índice de Transparência de Municípios do TCE-PE", y = "Frequência", title = "Histograma de itmpe_17") +
  theme_minimal()

# Calculando a mediana de itmpe_17
mediana <- median(base_final$itmpe_17, na.rm = TRUE)

# Criar o histograma com ggplot2
ggplot(base_final, aes(x = itmpe_17)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mediana, color = "black", linetype = "dashed", size = 1) +  # Adicionar linha representando a mediana
  labs(x = "Índice de Transparência de Municípios do TCE-PE", y = "Frequência", title = "Histograma de itmpe_17") +
  annotate("text", x = mediana, y = 10, label = paste("Mediana:", round(mediana, 2)), vjust = -1) +  # Adicionar texto com o valor da mediana
  theme_minimal()

# Calculando a mediana de saeb_total e saepe_total
mediana_saeb_total <- median(base_final$saeb_total, na.rm = TRUE)
mediana_saepe_total <- median(base_final$saepe_total, na.rm = TRUE)

# Criar o histograma para saeb_total
hist_saeb_total <- ggplot(base_final, aes(x = saeb_total)) +
  geom_histogram(binwidth = 10, fill = "#00468A", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mediana_saeb_total, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = mediana_saeb_total, y = 65, label = paste("Mediana:", round(mediana_saeb_total, 2)), vjust = -1) +
  labs(x = "Proficiência Média SAEB", y = "Frequência", title = "Histograma Proficiência Média SAEB") +
  theme_minimal()

# Criar o histograma para saepe_total
hist_saepe_total <- ggplot(base_final, aes(x = saepe_total)) +
  geom_histogram(binwidth = 10, fill = "#BB001C", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mediana_saepe_total, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = mediana_saepe_total, y = 45, label = paste("Mediana:", round(mediana_saepe_total, 2)), vjust = -1) +
  labs(x = "Proficiência Média SAEPE", y = "Frequência", title = "Histograma Proficiência Média SAEPE") +
  theme_minimal()

# Exibir os histogramas
hist_saeb_total
hist_saepe_total

mediana_itmpe_17 <- median(base_final$itmpe_17, na.rm = TRUE)
hist_itmpe_17 <- ggplot(base_final, aes(x = itmpe_17)) +
  geom_histogram(binwidth = 50, fill = "#F14C24", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mediana_itmpe_17, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = mediana_itmpe_17, y = 37.5, label = paste("Mediana:", round(mediana_itmpe_17, 2)), vjust = -1) +
  labs(x = "ITMPE", y = "Frequência", title = "Histograma ITMPE") +
  theme_minimal()
hist_itmpe_17

ggsave(
  "histograma_saeb.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 600,
  plot = last_plot())


mediana_educacao <- median(base_final$educacao, na.rm = TRUE)
hist_educacao <- ggplot(base_final, aes(x = educacao)) +
  geom_histogram(binwidth = 10, fill = "#0BAD7E", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mediana_educacao, color = "black", linetype = "dashed", size = 1) +
  annotate("text", x = mediana_educacao, y = 65, label = paste("Mediana:", round(mediana_educacao, 2)), vjust = -1) +
  labs(x = "Média da Proficiência no SAEB e SAEPE", y = "Frequência", title = "Histograma Média da Proficiência no SAEB e SAEPE") +
  theme_minimal()
hist_educacao

ggsave(
  "histograma_educacaototal.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 600,
  plot = last_plot())
