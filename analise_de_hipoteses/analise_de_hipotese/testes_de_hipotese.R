################################################
###           ANÁLISE DE HIPÓTESES           ###
################################################

# Fazemos análise de hipóteses para rejeitar um hipótese H0.
# A hipótese H0 é chamada de hipótese nula e sempre representará uma igualdade
# Já a hipótese alternativa H1 representa uma desigualdade (maior, menor ou diferente)

# Avaliamos o teste de hipótese pelo p-Value. 
# Se p-Value < 0,05 (5%), temos uma forte evidência CONTRA H0.
# Ou seja, se p-Value < 0,05, rejeitamos H0. 

################################################
###         CLASSIFICAÇÃO DOS TESTES         ###
################################################

# Paramétricos e Não Paramétricos

# Aplicamos testes paramétricos quando a distribuição é normal. Esses testes 
# podem fazer inferências sobre intervalos.

# Os testes não paramétricos não exigem uma distribuição normal e possuem
# resultados melhores para pequenas amostras.

# Testamos a normalidade pelo Teste SHAPIRO-WILK, onde H0 representa a normalidade
# Se p-Value < 0,05, a distribuição não é normal. 

# Ver visualmente a distribuição da coluna do dataset. Nesse caso, não parece normal
hist(iris$Sepal.Length)
# Aplicando o teste de Shapiro-Wilk. p = 0.01 < 0.05 (rejeitamos a normalidade)
shapiro.test(iris$Sepal.Length)

# Ver visualmente a distribuição da coluna do dataset. Nesse caso, parece normal
hist(iris$Sepal.Width)
# Aplicando o teste de Shapiro-Wilk. p = 0.10 > 0.05 (aceitamos a normalidade)
shapiro.test(iris$Sepal.Width)



# Pareados e Não Pareados

# Em teste pareados, as amostras analisadas devem ser dependentes, de um mesmo
# grupo ou indivíduo. O tamanho dessas amostras deve ser o mesmo. 

# Em testes não pareados, as amostras analisadas são independentes, de grupos ou 
# indivíduos diferentes. O tamanho dessas amostras pode ser diferente. 

# Não existe um teste para testar se são pareados ou não. Fazemos essa análise
# pelo contexto inserido. 
