library(readr)
library(dplyr)
library(ggplot2)

# Importar dados, string vazio como NA, string como fatores
df_churn <- readr::read.csv("Churn.csv", sep = ";", na.strings = "")

# Verificando o tipo do dataset
class(df_churn)

# Transformando de para o formato tibble
df_churn <- dplyr::as_tibble(df_churn)

# Ver o cabeçalho do dataset, apresentando os tipos dos dados e primeiras linhas
head(df_churn)

# Entendendo a distribuição com um resumo estatísticos e NAs
summary(df_churn)

# Renomear colunas
colnames(df_churn) = c("Id", "Score", "Estado", "Genero", "Idade", "Patrimonio",
                       "Saldo", "Produtos", "TemCartCred", "Ativo", "Salario", "Saiu")

###################################
### Explorar dados do tipo char ###
###################################

# Estado
contagem_estado = table(df_churn$Estado) # obter tabela
barplot(contagem_estado, main = "Distribuição dos Estados", xlab = "Estado", ylab = "Contagem")
# Com o gráfico podemos perceber valores como RP e TD
# Alternativa para o gráfico:
ggplot(df_churn, aes(x = Estado)) +
  geom_bar() +
  labs(title = "Distribuição dos Estados", x = "Estado", y = "Contagem")

# Gênero
ggplot(df_churn, aes(x = Genero)) +
  geom_bar() +
  labs(title = "Distribuição dos Generos", x = "Genero", y = "Contagem")
# Percebemos que a variável apresenta mais de um tipo de resposta como "F ou Fem para a mesma caracteristica)

################################
### Explorar dados numéricos ###
################################

# Score
summary(df_churn$Score)
boxplot(df_churn$Score)
hist(df_churn$Score)
# Percebemos que não nenhum dado discrepante

# Idade
summary(df_churn$Idade) # minimo: -20, maximo: 140 (tem algo errado)
boxplot(df_churn$Saldo)
hist(df_churn$Saldo)

# Saldo
summary(df_churn$Saldo)
boxplot(df_churn$Saldo)
hist(df_churn$Saldo)

# Salário
summary(df_churn$Salario)
boxplot(df_churn$Salario)
boxplot(df_churn$Salario, outline = F) # sem os outliers
hist(df_churn$Salario)
# Percebemos que há NA e outliers

########################
## Limpeza dos dados ###
########################

# Remoção de dados duplicados (caso haja)
df_churn <- df_churn %>% dplyr::distinct()

# Visualizar dados faltante
df_churn[!complete.cases(df_churn), ]

# Como são 15 linhas com NAs e m um total de 998, serão removidas as linhas
df_churn_semna <- stats::na.omit(df_churn)

# Visualizar o novo df sem dados faltante
df_churn_semna[!complete.cases(df_churn_semna), ]

###########################
## Tratamento dos dados ###
###########################

# Estado
# Transformação para factor para facilitar a manipulação e análise dos dados
df_churn_semna$Estado <- as.factor(df_churn_semna$Estado) 
# Visualização
unique(df_churn_semna$Estado) # retorna os valores unicos da coluna
summary(df_churn_semna$Estado) # retorna as frequencias de cada fator
# Tranformação RP = PR
df_churn_semna[df_churn_semna$Estado == "RP", ]$Estado = "PR"
# Excluindo linhas com as SP e TD
df_churn_semna <- df_churn_semna %>%
  dplyr::filter(Estado %in% c("PR", "SC", "RS"))
# Remover fator não utilizados
df_churn_semna$Estado <- factor(df_churn_semna$Estado) 
# Visualização pós-tratamento
summary(df_churn_semna$Estado)

# Genero: falta de padronização

# Transformação para factor para facilitar a manipulação e análise dos dados
df_churn_semna$Genero <- as.factor(df_churn_semna$Genero) 
# Visualização
unique(df_churn_semna$Genero) # retorna os valores unicos da coluna
summary(df_churn_semna$Genero) # retorna as frequencias de cada fator
# Tranformação F, Fem em Feminino e M em Masculino
df_churn_semna[df_churn_semna$Genero == "F" | df_churn_semna$Genero == "Fem", ]$Genero = "Feminino"
df_churn_semna[df_churn_semna$Genero == "M", ]$Genero = "Masculino"
# Remover fator não utilizados
df_churn_semna$Genero <- factor(df_churn_semna$Genero) 
# Visualização pós-tratamento
summary(df_churn_semna$Genero)

# Idade
# Visualização
summary(df_churn_semna$Idade) # percebemos que há valores fora do intervalo normal.
df_churn_semna[df_churn_semna$Idade < 0 | df_churn_semna$Idade > 100, "Idade"] # encontramos 3 linhas que apresentam -20, -10 e 140 como idade
# Excluindo linhas com as idades fora da faixa 0 a 100
df_churn_semna <- df_churn_semna %>%
  dplyr::filter(Idade >= 0 & Idade <= 100)
# Visualização pós-tratamento
summary(df_churn_semna$Idade)

# Salario
# Visualização
summary(df_churn_semna$Salario)
nrow(df_churn_semna[df_churn_semna$Salario, ])
# Calculando IQR
salario_IQR <- stats::IQR(df_churn_semna$Salario)
# Calculando os Quartis Q1 e Q3
salario_quartiles <- stats::quantile(df_churn_semna$Salario, c(0.25, 0.75))
# Definindo limites para outliers
limite_inferior <- salario_quartiles[1]-1.5*saldo_IQR
limite_superior <- salario_quartiles[2]+1.5*saldo_IQR
# Filtrando outliers
df_churn_no_outliers <- df_churn_semna %>% 
  dplyr::filter(Salario>limite_inferior & Salario < limite_superior)
# Visualização pós-tratamento
summary(df_churn_no_outliers$Salario)
nrow(df_churn_no_outliers[df_churn_no_outliers$Salario, ])
