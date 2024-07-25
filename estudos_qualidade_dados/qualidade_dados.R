
#########################################################
###                     BIBLIOTECAS                   ###
#########################################################
install.packages("dplyr") # Manipulação de dados
install.packages("ggplot2") # Visualização de dados
install.packages("janitor") # Limpeza e resumo dos dados
install.packages("lubridate") # Manipulação de datas
install.packages("naniar") # Visualização de dados faltantes
install.packages("stringr") # Manipulação de textos
install.packages("tidyr") # Transformação de dados

library("dplyr") 
library("ggplot2") 
library("janitor") 
library("lubridate") 
library("naniar")
library("stringr") 
library("tidyr")

#########################################################
###                 DADOS CATEGÓRICOS                 ###
#########################################################

# Carregando o dataset 'iris' e criando dados ausentes para demonstração
data("iris")
set.seed(1234) # Para reprodutibilidade

# Introduzindo NA's aleatoriamente em algumas observações da coluna Species
# Apenas para carater didático 
iris$Species[sample(51:150, 25)] <- NA 

# Análise de balanceamento de classes com 'tabyl' do 'janitor'
iris_tabyl <- iris |> 
  janitor::tabyl(Species)

# Exibindo análise
iris_tabyl

#########################################################
###                  DADOS NUMÉRICOS                  ###
#########################################################

# Carregando o dataset 'airquality'
data("airquality")

# Calculando as estatísticas descritivas e percentual de dados faltantes
stats <- airq# Carregando o dataset 'airquality'
data("airquality")

# Calculando as estatísticas descritivas e percentual de dados faltantes
stats <- airquality |>
  dplyr::select(-c("Month", "Day")) |> # Retiradas variáveis que,
  # apesar de numéricas, sua análise deve ser feita como categoria.
  dplyr::summarise(
    dplyr::across( #Identifica uma sequencia de colunas
      dplyr::where(is.numeric), #Quais sao as colunas utilizadas no across
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        na_percentage = ~sum(is.na(.)) / n() * 100
      ),
      .names = "{.col}-{.fn}"  # Usando hífen como separador
    )
  ) |>
  tidyr::pivot_longer( #Transforma esses dados em uma tabela por colunas
    cols = everything(), 
    names_to = c("Variable", ".value"), 
    names_sep = "-"  # Alinhando o separador com o usado acima
  )

# Exibindo as estatísticas descritivas
stats

#########################################################
###                  DADOS TEXTUAIS                   ###
#########################################################
setwd("/Users/isabellastersa/Desktop/Isabella/CIENCIA_DE_DADOS_WLAD/sandbox/estudos_qualidade_dados")
# Lendo o dataset
customer_satisfaction_df <- read.csv("customer_satisfaction.csv")

# Análise de variáveis texto (coluna Feedback)
text_analysis <- customer_satisfaction_df %>%
  dplyr::summarise(
    average_length = mean(stringr::str_length(Feedback), na.rm = TRUE),
    max_length = max(stringr::str_length(Feedback), na.rm = TRUE),
    min_length = min(stringr::str_length(Feedback), na.rm = TRUE),
    na_empty_percent = sum(is.na(Feedback) | Feedback == "") / n() * 100,
    unique_char_lines = sum(stringr::str_detect(Feedback, "^([a-zA-Z0-9])\\1*$"), na.rm = TRUE),
    n = n()
  )

# Exibindo resultados
text_analysis

#########################################################
###                  DADOS TEMPORAIS                  ###
#########################################################

#Utilizaremos o mesmo dataset acima 

# Tendo certeza que 'Data_Entrada' é do tipo data
str(customer_satisfaction_df$Data_Entrada)

# Convertendo 'Data_Entrada' para o tipo data com ymd (year-month-day)
# Registros incorretos serão convertidos em NA
customer_satisfaction_df$Data_Entrada <- ymd(customer_satisfaction_df$Data_Entrada)
str(customer_satisfaction_df$Data_Entrada)

# Análise de variáveis data
date_analysis <- customer_satisfaction_df %>%
  summarise(
    min_date = min(Data_Entrada, na.rm = TRUE),
    max_date = max(Data_Entrada, na.rm = TRUE),
    na_percentage = sum(is.na(Data_Entrada)) / n() * 100
  )

# Exibindo resultados
date_analysis

#########################################################
###             REMOVENDO DADOS FALTANTES             ###
#########################################################

# Uma das formas de fazer o tratamentos desses erros encontrados é fazer
# a remoção de dados faltantes (Há também como imputar resultador por média ou 
# regressão mas não é recomendado)

# Remover as linhas que contem valores nulos
customer_satisfaction_tratado <- customer_satisfaction_df %>% na.omit()

# Remover colunas que possuem grande proporção de dados NA ou não são relevantes para a análise
customer_satisfaction_tratado <- customer_satisfaction_tratado %>% select(-ID_Cliente)

# Exibindo o novo dataset
customer_satisfaction_tratado

#########################################################
###              REMOVENDO OS OUTLIERS                ###
#########################################################

# Outliers são valores que estão significativamente fora do esperado
# Não necessariamente deve ser feita a remoção dos outliers, depende do contexto
# Deve analisar se os outliers são de fatos erros ou representam variações reais

# Calculando o IQR para 'Ozone'
ozone_IQR <- IQR(airquality$Ozone, na.rm = TRUE)
ozone_IQR

# Calculando os Quartis (Q1 e Q3)
ozone_quartiles <- quantile(airquality$Ozone, c(0.25, 0.75), na.rm = TRUE)
ozone_quartiles

# Definindo limites para outliers
lower_bound <- ozone_quartiles[1] - 1.5 * ozone_IQR
lower_bound

upper_bound <- ozone_quartiles[2] + 1.5 * ozone_IQR
upper_bound

# Filtrando outliers (tirando os outliers)
airquality_no_outliers <- airquality |>
  dplyr::filter(Ozone > lower_bound & Ozone < upper_bound)
airquality_no_outliers
