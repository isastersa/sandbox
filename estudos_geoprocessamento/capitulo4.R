library(sf) # pacote para trabalhar com dados vetoriais
library(terra) # pacote para trabalhar com dados raster
library(spData)
library(dplyr)

####################################
### Operações de dados espaciais ###
####################################

# Operações espaciais em dados vetoriais

# Subconjunto espacial
# dataset nz e nz_height = dados da Nova Zelândia

# Subconjunto de linhas
canterbury = nz |> 
  filter(Name == "Canterbury")
# Verifica quais pontos de nz_height estão dentro da geometria de canterbury
# determinada na linha acima
canterbury_height = nz_height[canterbury, ]

# Outra forma de fazer subconjunto
canterbury_height2 = nz_height |>
  st_filter(y = canterbury, .predicate = st_intersects)

# Relações topológicas
# Em geral, são binárias.

# Por padrão, o subconjunto é feito com a operação topológica st_intersects.
# st_intersect - inclui elementos que tocam, cruzam ou estão dentro do subconjunto de origem
# st_disjoint - oposto de st_intersect
# st_touches - compartilham pelo menos um ponto da fronteira, mas não o interior
# st_overlaps - possui alguma interseção, mas não é contido
# st_contains - verifica se primeiro objeto contém o segundo objeto, podendo incluir parte da fronteira
# st_contains_properly - verifica se primeiro objeto contém o segundo objeto, sem parte da fronteira
# st_within - verifica se primeiro objeto está contido no segundo objeto
# st_covers - verifica se primeiro objeto cobre o segundo objeto
# st_covered_by - verifica se o primeiro objeto é coberto pelo segundo objeto 


# Outra forma de fazer subconjunto
canterbury_height2 = nz_height |>
  st_filter(y = canterbury, .predicate = st_intersects)

# Relações de distância
# São contínuas. st_distance calcula a distância entre dois objetos

# Ponto mais alto de NZ
nz_highest = nz_height |> 
  slice_max(n = 1, order_by = elevation) # n=1 garante o retorno de apenas uma linha
# Ponto central em Canterbury
canterbury_centroid = st_centroid(canterbury)
# Distância entre o ponto mais alto e o ponto central
st_distance(nz_highest, canterbury_centroid)

# Separando as linhas com nome Canter e Otag
co = filter(nz, grepl("Canter|Otag", Name))
# Calculando a distância das três primeiras linhas de nz_height com Canter e Otag
st_distance(nz_height[1:3, ], co)

# Junção espacial

# Exemplo de conjunto de pontos
set.seed(2018) # garantindo que os resultados se mantenha
(bb = st_bbox(world)) # retorna os limites espaciais do sf world

random_df = data.frame( # gera 10 pontos aleatorios no intervalo dos limites espaciais de world
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
) 
random_points = random_df |> # converte para objeto espacial ponto com CRS
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")

# JOIN PARA OBJETOS ESPACIAIS: 
# st_join() - fará a relação entre objetos espaciais, usando st_intersects como padrão
random_joined = st_join(random_points, world["name_long"]) # Está juntando o pontos randomicos com os nomes dos paises correspondentes

# JOIN com base na distância
# st_is_within_distance = função usada para saber se um objeto está dentro de uma determinada distancia de outra geometria

z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, 
            dist = units::set_units(20, "m"))

# Agregação espacial

# O que será agregado? x, Dentro de qual fonte? by, Qual função de agregação? FUN
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

# Forma alternativa - preservando o nome das regioes
nz_agg2 = st_join(x = nz, y = nz_height) |> # juntou as duas bases
  group_by(Name) |> # agrupou por nome
  summarize(elevation = mean(elevation, na.rm = TRUE)) # tirou a media de elevation

##################
### EXERCÍCIOS ###
##################

#E1. Foi estabelecido na Seção 4.2 que Canterbury era a região da Nova Zelândia 
# contendo a maioria dos 101 pontos mais altos do país. 
# Quantos desses pontos altos a região de Canterbury contém?
canterbury = nz |> 
  filter(Name == "Canterbury")
canterbury_height = nz_height[canterbury, ]
nrow(canterbury_height)
# Bônus: plote o resultado usando a plot() para mostrar toda a Nova Zelândia, 
# canterbury destacada em amarelo, 
# pontos altos em Canterbury representados por cruzes vermelhas (dica: pch = 7)
# pontos altos em outras partes da Nova Zelândia representados por círculos azuis. 
not_canterbury = nz_height[canterbury, , op = st_disjoint]
plot(st_geometry(nz))
plot(st_geometry(canterbury), col = "yellow", add = TRUE)
plot(st_geometry(not_canterbury), pch = 1, col = "blue", add = TRUE) #pch mudar o formato
plot(st_geometry(canterbury_height), pch = 4, col = "red", add = TRUE)

#E2. Qual região tem o segundo maior número de nz_height pontos e quantos ela tem?
nz_height_contagem = aggregate(nz_height, nz, length) # agrupando nz_height de acordo com a geometria de nz e contando (lenght)
nz_height_combinado = cbind(nz, count = nz_height_contagem$elevation) # juntando as contagens do sf nz
nz_ordenado <- nz_height_combinado %>% 
  st_drop_geometry() %>% 
  select(Name, count) %>% 
  arrange(desc(count))

nz_ordenado[2,] # segundo pais na ordem

# E3. Generalizando a questão para todas as regiões: quantas das 16 regiões da Nova Zelândia contêm pontos que pertencem 
# aos 101 pontos mais altos do país? Quais regiões?
#Bônus: crie uma tabela listando essas regiões em ordem de número de pontos e seus nomes.
nz_regioes_pontos_altos <- nz_height_combinado %>% 
  st_drop_geometry() %>% 
  select(Name, count) %>% 
  arrange(desc(count)) %>%
  na.omit()
# Quantas regiões?
nrow(nz_regioes_pontos_altos)
# Quais regiões?
nz_regioes_pontos_altos$Name

# E4. Teste seu conhecimento sobre predicados espaciais descobrindo e plotando 
# como os estados dos EUA se relacionam entre si e com outros objetos espaciais.

# O ponto de partida deste exercício é criar um objeto representando o estado do Colorado nos EUA. 
# Faça isso com o comando colorado = us_states[us_states$NAME == "Colorado",] e plote o objeto resultante no contexto dos estados dos EUA.
colorado = us_states[us_states$NAME == "Colorado", ]
plot(us_states$geometry)
plot(colorado$geometry, col = "grey", add = TRUE)
#Crie um novo objeto representando todos os estados que se interceptam geograficamente com o Colorado 
# e plote o resultado (dica: a maneira mais concisa de fazer isso é com o método de subconjunto [).
intersects_with_colorado = us_states[colorado, , op = st_intersects]
plot(us_states$geometry, main = "Estados que interceptam com Colorado")
plot(intersects_with_colorado$geometry, col = "grey", add = TRUE)
#Crie outro objeto representando todos os objetos que tocam (têm um limite compartilhado com) Colorado 
# e plote o resultado.
touches_colorado = us_states[colorado, , op = st_touches]
plot(us_states$geometry, main = "Estados que tocam o Colorado")
plot(touches_colorado$geometry, col = "grey", add = TRUE)
#Bônus: crie uma linha reta do centroide do Distrito de Columbia, perto da costa leste, até o centroide da Califórnia, 
#perto da costa oeste dos EUA e identifique quais estados essa longa linha leste-oeste cruza.st_union()st_cast()
washington_to_cali = us_states |> 
  filter(grepl(pattern = "Columbia|Cali", x = NAME)) |> 
  st_centroid() |> 
  st_union() |> 
  st_cast("LINESTRING")
states_crossed = us_states[washington_to_cali, , op = st_crosses]
states_crossed$NAME
plot(us_states$geometry, main = "Estados que são cruzados pela linha Columbia - California")
plot(states_crossed$geometry, col = "grey", add = TRUE)
plot(washington_to_cali, add = TRUE)
