library(readr)
library(dplyr)

# Importar dados, string vazio como NA, string como fatores
df_churn <- read.csv("Churn.csv", sep = ";", na.strings = "")

# Verificando o tipo do dataset
class(df_churn)

# Transformando de 
