library("sf") # pacote para trabalhar com dados vetoriais
library("terra") # pacote para trabalhar com dados raster
library("spData")
library("dplyr")

##########################################
### Manipulação de atributos vetoriais ###
##########################################

# Possui compatibilidade com a pacote dplyr
# Podemos retirar a coluna de geometria com a função st_drop_geometry()

# Subconjunto de atributos vetoriais
# Funções filter(), slice (subconjunto de linhas) e select(subconjunto de colunas)

world[1:6, ] # subconjunto de linhas por posição
world[, 1:3] # subconjunto de colunas por posição
world[1:6, 1:3] # subconjunto de linhas e colunas por posição
world[, c("name_long", "pop")] # subconjunto de colunas por nome

# Subconjunto por uma condição
small_countries = world[world$area_km2 < 10000, ]
# small_countries = world[world$area_km2 < 10000, ]

# Subconjunto de colunas
world1 = select(world, name_long, pop)
world2 = select(world, name_long:pop) # colunas entre name_long e pop inclusive
world3 = select(world, -subregion, -area_km2) # exceto subregion e area_km2
world4 = select(world, name_long, population = pop) # renomeando nome_novo = nome_antigo

# Subconjunto de linhas
slice(world, 1:6) # da linha 1 a 6 inclusive
world5 = filter(world, area_km2 < 10000) # filtrando linhas por condicoes
world6 = filter(world, lifeExp > 82)

# Usando comandos com pipes

# selecionando países da africa, mostrando nome e continente, cortando de 1 a 5
world7 = world |>
  filter(continent == "Asia") |>
  select(name_long, continent) |>
  slice(1:5)

# Agregação de atributos vetoriais

# Somando a população por continente
world_agg1 = world |>
  group_by(continent) |>
  summarize(pop = sum(pop, na.rm = TRUE))

# Somando a população, área por continente e número de países
world_agg2  = world |> 
  group_by(continent) |> 
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n())

# Exemplo de múltiplas operações
world_agg3 = world |> 
  st_drop_geometry() |> # retira a geometria para se tornar mais rápido
  select(pop, continent, area_km2) |> # apenas colunas pop, continent e area  
  group_by(continent) |> # agrupando por ocntinente
  summarize(Pop = sum(pop, na.rm = TRUE), Area = sum(area_km2), N = n()) |> # fazendo soma da pop, area e quantidade de paises
  mutate(Density = round(Pop / Area)) |> # criando coluna de densidade
  slice_max(Pop, n = 3) |> # fazendo um top 3 em relacao a populacao
  arrange(desc(N)) # ordenando por quantidade de paises desc

# Junção de atributos vetoriais

# LEFT-JOIN: preserva todos os dados do primeiro df e mescla com o segundo
# é procurado uma váriavel que tenha mesmo nome

world_coffee = left_join(world, coffee_data)

# como fazer o left-join quando não temos correspondencias nos nomes dos dfs
# left_join(df1, df2, by = join_by(coluna_df1 == coluna_df2))

# INNER - JOIN: apenas dados com correspondência em ambas tabelas
world_coffee_inner = inner_join(world, coffee_data)

# identificando linhas que não corrsponderam
setdiff(coffee_data$name_long, world$name_long)

# corrigindo o erro
drc = stringr::str_subset(world$name_long, "Dem*.+Congo")
coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc #alterando o nome
world_coffee_match = inner_join(world, coffee_data)

# Craindo atributos e removendo informações espaciais

# Criando a coluna de densidade
world_new = world # não sobrescrever o df original
world_new$pop_dens = world_new$pop / world_new$area_km2
# Poderia ser criado com mutate
world_new2 = world |> 
  mutate(pop_dens = pop / area_km2)

# Juntando colunas
# unite(nome_colunanova, quais colunas serao unidas, separador, remover colunas originais)
world_unite = world |>
  tidyr::unite("con_reg", continent:region_un, sep = " : ", remove = TRUE)

# Separando colunas
world_separate = world_unite |>
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")

# Renomeando colunas - setNames
new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")
world_new_names = world |>
  setNames(new_names)

#####################################
### Manipulação de objetos raster ###
#####################################

# Criando um objeto raster de 6x6 com valores sequenciais de 1 a 36 
# distribuídos em uma extensão espacial de -1.5 a 1.5.
elev = rast(nrows = 6, ncols = 6,
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)
plot(elev)

# Criando um raster de 6x6 com valores categóricos ("clay", "silt", "sand") 
# gerados aleatoriamente, mantendo uma ordem específica para os fatores.
grain_order = c("clay", "silt", "sand") # criando vetor
grain_char = sample(grain_order, 36, replace = TRUE) # 36 elementos do vetor order aleatoriamente
grain_fact = factor(grain_char, levels = grain_order) # transforma em fator com um nivel de order ("clay" < "silt" < "sand")
grain = rast(nrows = 6, ncols = 6, 
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)
plot(grain)

# Cópia de um raster categórico original, onde os níveis dos fatores são renomeados 
# com novos valores associados à umidade ("wet", "moist", "dry").
grain2 = grain # não sobrescrever df original
levels(grain2) = data.frame(value = c(0, 1, 2), wetness = c("wet", "moist", "dry"))
levels(grain2)
plot(grain2)

# Subconjunto de raster
# Elemento da linha 1 coluna 1 
elev[1, 1]

# Modificando valores
elev[1, 1] = 0

# Mostrar valores de um raster
elev[]

# Modificando mais de um valor por vez
elev[1, c(1, 2)] = 0 # na linha 1, elementos da coluna 1 e 2

# Modificando por matrizes
two_layers = c(grain, elev)
# Substituindo as 2 primeiras linhas do df com os valores 1 e 4
# Obs: são 6 elementos por linha, logo foram modificados 12 elementos
two_layers[1:2, ] = cbind(c(1), c(4))
two_layers[]

# Estatíticas descritivas em dados rasters

# Mínimo, máximo, quartis e número de NAs
summary(elev)
# Desvio padrão 
global(elev, sd)
# Tabela de frequência
freq(grain)

##################
### EXERCÍCIOS ###
##################

data(us_states)
data(us_states_df)

# E1. Crie um novo objeto chamado us_states_name que contenha apenas a 
# a coluna NAME do us_states. Qual é a classe do novo objeto e o que o torna geográfico?
us_states_name <- us_states["NAME"]
class(us_states_name) # Objeto do tipo sf
attributes(us_states_name) # Mostra que tem a coluna geometry
attributes(us_states_name$geometry) # Nessa coluna tem objetos geometricos

# E2. Selecione colunas do us_states que contenham dados populacionais. Obtenha 
# o mesmo resultado usando um comando diferente
us_states %>%
  select(total_pop_10, total_pop_15)

us_states %>% 
  select(starts_with("total_pop"))

us_states %>% 
  select(contains("total_pop"))

# E3. Encontre e plote todos os estados com as seguintes características:

# Pertence à região Midwest.
us_states_midwest <- us_states %>% 
  filter(REGION == "Midwest")
plot(us_states_midwest)

# Pertencem à região West, têm uma área inferior a 250.000 km2 e em 2015 uma população superior a 5.000.000 de habitantes (dica: pode ser necessário utilizar a função units::set_units()ou as.numeric()).
us_states_west_inf250_5hab <- us_states %>%
  filter(REGION == "West",
         AREA < units::set_units(250000, km^2), 
         total_pop_15 > 5000000)
plot(us_states_west_inf250_5hab)

# Pertence à região South, possui uma área maior que 150.000 km2 e uma população total em 2015 maior que 7.000.000 de habitantes.
us_states_south_sup150_7hab <- us_states %>%
  filter(REGION == "South",
         AREA > units::set_units(150000, km^2), 
         total_pop_15 > 7000000)
plot(us_states_south_sup150_7hab)

# E4. Qual era a população total em 2015 no us_statesconjunto de dados? 
sum(us_states$total_pop_15)
# Qual era a população total mínima e máxima em 2015?
min(us_states$total_pop_15)
max(us_states$total_pop_15)

# E5. Quantos estados existem em cada região?
us_states %>%
  count(REGION)

# E6. Qual foi a população total mínima e máxima em 2015 em cada região? 
us_states %>%
  group_by(REGION) %>%
  summarize(min(total_pop_15),
            max(total_pop_15))
# Qual foi a população total em 2015 em cada região?
us_states %>%
  group_by(REGION) %>%
  summarize(sum(total_pop_15))

# E7. Adicione variáveis de us_states_df para us_states, e crie um novo objeto chamado us_states_stats. 
us_states_stats = us_states |>
  left_join(us_states_df, by = c("NAME" = "state"))

# Qual função você usou e por quê? 
# R: Usei left join para manter todos os valores de us_states e incorporar os dados de us_states_df
# Qual variável é a chave em ambos os conjuntos de dados? 
# R: coluna NAME em us_states corresponde a coluna state em us_states_df
# Qual é a classe do novo objeto?
class(us_states_stats)

# E8. us_states_dftem duas linhas a mais que us_states. Como você pode encontrá-las? 
# (dica: tente usar a dplyr::anti_join())
# anti_join retorna as linhas de us_states_df que não tem correspondencia em us_states
us_states_df |>
  anti_join(st_drop_geometry(us_states), by = c("state" = "NAME"))

#E9. Qual era a densidade populacional em 2015 em cada estado? 
us_states2 = us_states |>
  mutate(pop_dens_15 = total_pop_15/AREA)
# Qual era a densidade populacional em 2010 em cada estado?
us_states2 = us_states |>
  mutate(pop_dens_10 = total_pop_10/AREA)

# E10. Quanto a densidade populacional mudou entre 2010 e 2015 em cada estado? 
# Calcule a mudança em porcentagens e mapeie-a
us_popdens_change = us_states2 |>
  mutate(pop_diff_15_10 = pop_dens_15 - pop_dens_10,
         pop_diff_15_10_porc = (pop_diff_15_10/pop_dens_10)*100)
plot(us_popdens_change["pop_diff_15_10_porc"])

# E11. Altere os nomes das colunas us_states para minúsculas. 
# (Dica: funções auxiliares - tolower()e colnames()podem ajudar.)
us_states %>% 
  setNames(tolower(colnames(us_states)))

# E12. Usando us_states e us_states_df crie um novo objeto chamado us_states_sel.
# O novo objeto deve ter apenas duas variáveis - median_income_15 e geometry. 
# Altere o nome da median_income_15 coluna para Income.
us_states_sel = us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) |>
  select(Income = median_income_15) # basta selecionar a coluna median, pois geometry é fixa

# E13. Calcule a mudança no número de residentes vivendo abaixo do nível de pobreza entre 2010 e 2015 para cada estado. 
# (Dica: veja ?us_states_df para documentação sobre as colunas de nível de pobreza.) 
us_pov_change = us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) |>
  mutate(pov_change = poverty_level_15 - poverty_level_10)
# Bônus: Calcule a mudança na porcentagem de residentes vivendo abaixo do nível de pobreza em cada estado.
us_pov_porc_change = us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) |>
  mutate(pov_porc_10 = (poverty_level_10 / total_pop_10) * 100, # porcentagem em 2010
         pov_porc_15 = (poverty_level_15 / total_pop_15) * 100) |> # porcnetagem em 2015 
  mutate(pov_porc_change = pov_porc_15 - pov_porc_10)

# E14. Qual foi o número mínimo, médio e máximo de pessoas vivendo abaixo da linha da pobreza em 2015 para cada região? 
us_states |>
  left_join(us_states_df, by = c("NAME" = "state")) %>%
  group_by(REGION) %>%
  summarize(min(poverty_level_15),
            mean(poverty_level_15),
            max(total_pop_15))
# Bônus: Qual é a região com o maior aumento de pessoas vivendo abaixo da linha da pobreza?
us_pov_change %>% 
  group_by(REGION) %>% 
  summarize(region_pov_change = sum(pov_change)) %>%
  filter(region_pov_change == max(region_pov_change))

# E15. Crie um raster do zero com nove linhas e colunas e uma resolução de 0,5 graus decimais (WGS84). 
# Preencha-o com números aleatórios. 
r = rast(nrow = 9, ncol = 9, res = 0.5,
         xmin = 0, xmax = 4.5, ymin = 0, ymax = 4.5,
         vals = rnorm(81))
# Extraia os valores das quatro células de canto.
r[c(1, 9, 73, 81)]

# E16. Qual é a classe mais comum do nosso raster de exemplo grain?
freq(grain)
