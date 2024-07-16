#Script das aulas de Estatística Básica em R 

#Tipos de Dados
qualitativas <- c("Positivo", "Negativo", "Positivo", "Negativo")
qualitativas

quantitativas <- c(7.8, 6.5, 8.9, 7.2)
quantitativas

#Podemos transformas os dados em uma tabela = data frame. 
#Veja que cada vetor é uma coluna do data frame. 

data.frame(
  "qualitativas" = c("Positivo", "Negativo", "Positivo", "Negativo"),
  "quantitativas" = c(7.8, 6.5, 8.9, 7.2)
)

#Cálculo de medidas de tendencia central
#Média, Mediana e Moda, respectivamente
notas <- c(85, 90, 78, 85, 98, 88, 60, 71, 23, 95, 65, 78, 82, 52, 37, 89, 90, 62, 100, 67)


mean(notas)
median(notas)

moda <- function(v){
  uniqv <- unique(v)
  freq <- tabulate(match(v, uniqv))
  max_freq <- max(freq)
  if(max_freq == 1){
    return(NA) #se todos os valores são unicos, não há moda
  } else {
    return (uniqv[which(freq== max_freq)])
  }
}

moda(notas)

#Cálculo de medidas de dispersão
#Variância, Desvio padrão, amplitude e IQR, respectivamente

variancia <- var(notas)
desvio_padrao <- sd(notas)
amplitude <- diff(range(notas)) #range pega o min e o max do vetor, diff faz a diferenca de numeros consecutivos de um vetor

#Consegue ver os quartis
quantile(notas)
#IQR = Q3 - Q1
IQR(notas)

#Utilizando boxplot.stats ele mostra os quartis sem os outliers (stats), 
#o numero de elementos da distribuicao (n)
#o intervalo de confianca para mediana (conf)
#os outliers detectados (out)

#Outliers
outliers <- c(boxplot.stats(notas)$out)

##############################
#     Criação de boxplots.   #
##############################

#Biblioteca highcharter
install.packages("highcharter")
library(highcharter)

df <- data.frame("notas" = notas)

#Sem Outliers
dat <- data_to_boxplot(df, notas)
highchart() %>%
  hc_chart(type = "boxplot") %>%
  hc_title(text = "Boxplot das Notas") %>%
  hc_xAxis(categories = c("Notas")) %>%
  hc_add_series_list(dat)

#Com Outliers
dat <- data_to_boxplot(df, notas, add_outliers = TRUE)
highchart() %>%
  hc_xAxis(type = "category") %>%
  hc_add_series_list(dat)


### Exemplos de aplicações - Boxplots de mais de um grupo
#Biblioteca echarts4r
install.packages("echarts4r")
library(echarts4r)
library(dplyr)

data <- readr:: read_csv(
  "https://raw.githubusercontent.com/wrprates/open-data/master/ibm_hr_emplyee_attrition.csv"
)

#Criando um df com as medianas agrupadas por JobRole
median_income_df <- data %>%
  group_by(JobRole) %>%
  summarize(median_income = median(MonthlyIncome, na.rm = TRUE))

#Ordenando JobRole pela mediana e criando um fator JobRole
sorted_data <- data %>%
  left_join(median_income_df, by = "JobRole") %>%
  mutate(JobRole = factor(JobRole, levels = rev(median_income_df$JobRole[order(median_income_df$median_income)]))) %>%
  arrange(JobRole)

#Criando boxplots múltiplos a partir do df ordenado
sorted_data %>%
  group_by(JobRole) %>%
  e_chart() %>%
  e_boxplot(MonthlyIncome) %>%
  e_color(color = "#4292b5") %>%
  e_tooltip() %>%
  e_title(text = "Renda mensal por JobRole", subtext = "Em milhares de USD.")

#####################################
###          CORRELAÇÃO           ###
#####################################

x1 <- mtcars$mpg
y1 <- mtcars$hp

#Calculando o coeficiente de correlação entre x e y
#Por padrão, a funcao cor utiliza o método Pearson
#Nesse caso, x1 e y1 são continuas, entao ok usar Pearson
cor(x1,y1) 

#Nesse caso x2 e y2 são discretas 
x2 <- mtcars$cyl
y2 <- mtcars$gear

#Para calcular esse coeficiente de correlação, devemos usar o método Spearman ou Kendall
cor(x2, y2, method = "spearman")

#MATRIZ DE CORRELAÇÃO 
cor(mtcars) %>%
  as_tibble()

#Mantendo apenas uma diagonal (para não ter valores duplicado. Mais fácil visualizar)
install.packages("rstatix")
library("rstatix")

replace_upper_triangle(cor(mtcars), by = NA, diagonal = TRUE) %>%
  as_tibble()

#Mapa de calor

library(echarts4r)
cor_chart <- cor(mtcars) %>%
  e_charts() %>%
  e_correlations(
    order = "hclust",
    visual_map = FALSE
  ) %>%
  e_visual_map(
    min = -1,
    max = 1
  ) %>%
  e_tooltip()

#chamando o objeto criado
cor_chart

#Gráfico mais usado para correcalcao = scatterplot (nem sempre viável)
mtcars %>%
  e_charts(mpg) %>%
  e_scatter(wt, qsec)

#Exemplo
library(dplyr)
library(readr)

#lendo o arquivo
dados_rh <- read_csv(
  "https://raw.githubusercontent.com/wrprates/open-data/master/ibm_hr_emplyee_attrition.csv")

#deixando apenas colunas numericas e tirando outras
dados_rh_num <- dados_rh %>%
    select_if(is.numeric) %>%
  select(
  -c(
  Education, 
  EmployeeNumber, 
  EmployeeCount, 
  JobLevel, 
  StockOptionLevel, 
  WorkLifeBalance, 
  StandardHours
  )
  )

#Calculando correlacao. Usando spearman pois há variaveis continuas e discretas
correl_dados_rh_num <- dados_rh_num %>%
  cor(method = "spearman" )

library(echarts4r)
cor_chart_rh <- cor(correl_dados_rh_num) %>%
  e_charts() %>%
  e_correlations(
    order = "hclust",
    visual_map = FALSE
  ) %>%
  e_visual_map(
    min = -1,
    max = 1
  ) %>%
  e_tooltip()

#chamando o objeto criado
cor_chart_rh

