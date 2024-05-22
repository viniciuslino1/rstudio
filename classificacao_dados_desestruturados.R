#Copiando df só com o outros
df <- Padronização_area_formacao[Padronização_area_formacao$padrao == "Outros", ]
table(df$area)

#criando coluna copiada de area para minupular texto
df$texto <- df$area

#limpar coluna de texto para padronização
install.packages("stringi")
library(stringi)
df$texto <- tolower(df$texto) #deixar minusculo
df$texto <- gsub("[0-9]", "", df$texto) #tirar numeros
df$texto <- stri_trans_general(df$texto, "Latin-ASCII") #tirar acentos
df$texto <- gsub("[^a-z ]", "", df$texto) #tirar caracteres especiais
df$texto <- trimws(df$texto) #remover espaços iniciais e finais que ficam no texto

table(df$texto)

#tranformar categorias
install.packages("tidyverse")
library(tidyverse)
df_trans <- df %>%
  mutate(texto = case_when(
    grepl("matematica", texto) ~ "Matemática",
    grepl("fisica", texto) ~ "Física",
    grepl("portugues", texto) ~ "Lingua Portuguesa e Literatura",
    grepl("administracao", texto) ~ "Administração",
    grepl("artes", texto) ~ "Artes",
    grepl("artes e religiao", texto) ~ "Artes e religião",
    grepl("biologia", texto) ~ "Biologia",
    grepl("biologia e quimica", texto) ~ "Biologia e Química",
    grepl("ciencias", texto) ~ "Ciências",
    grepl("educaçao", texto) ~ "Educação",
    grepl("engenharia", texto) ~ "Engenharia",
    grepl("geografia", texto) ~ "Geografia",
    grepl("historia", texto) ~ "História",
    grepl("ingles", texto) ~ "Lingua Inglesa e Literatura",
    grepl("religiao", texto) ~ "Artes e religião",
    grepl("religioso", texto) ~ "Artes e religião",
    grepl("docencia", texto) ~ "Educação",
    grepl("cultura", texto) ~ "Artes",
    grepl("gestao", texto) ~ "Administração",
    grepl("linguistica", texto) ~ "Lingua Portuguesa e Literatura",
    grepl("quimica", texto) ~ "Química",
    grepl("esportivo", texto) ~ "Educação Física",
    grepl("desportivo", texto) ~ "Educação Física",
    grepl("pedagogia", texto) ~ "Educação",
    grepl("esporte", texto) ~ "Educação Física",
    grepl("libras", texto) ~ "Libras",
    grepl("exercicio", texto) ~ "Educação Física",
    grepl("pedagogica", texto) ~ "Educação",
    grepl("escolar", texto) ~ "Educação",
    grepl("letras", texto) ~ "Lingua Portuguesa e Literatura",
    grepl("lingua", texto) ~ "Lingua Portuguesa e Literatura",
    grepl("inglesa", texto) ~ "Lingua Inglesa e Literatura",
    grepl("treinador", texto) ~ "Educação Física",
    grepl("futebol", texto) ~ "Educação Física",
    grepl("futsal", texto) ~ "Educação Física",
    grepl("agricola", texto) ~ "Agricultura",
    grepl("agricultura", texto) ~ "Agricultura",
    grepl("internet", texto) ~ "Informática",
    grepl("informacao", texto) ~ "Informática",
    grepl("informatica", texto) ~ "Informática",
    grepl("saude", texto) ~ "Saúde",
    grepl("nutricao", texto) ~ "Saúde",
    grepl("mecatronica", texto) ~ "Engenharia",
    grepl("industrial", texto) ~ "Engenharia",
    grepl("gerenciamento", texto) ~ "Administração",
    grepl("ambiente", texto) ~ "Ciências",
    grepl("ecologia", texto) ~ "Ciências",
    grepl("veterinaria", texto) ~ "Veterinária",
        TRUE ~ texto
  ))

table(df_trans$texto)


#baixar em excel a base
install.packages("openxlsx")
library(openxlsx)
write.xlsx(df_trans, file = "df_trans.xlsx")
