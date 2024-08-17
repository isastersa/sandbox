library("sf") # pacote para trabalhar com dados vetoriais
library("terra") # pacote para trabalhar com dados raster
library("spData")
library("spDataLarge", repos = "https://nowosad.r-universe.dev")

#######################
### DADOS VETORIAIS ###
#######################

# Dados vetoriais: pontos, linhas e polígonos. Bom para entidades discretas (com limites bem definidos)
# CRS geográfico ('lon/lat')
# Classe sf: combina dados tabulares não geográficos com geometrias espaciais

# Sobre dataframe world
class(world) # tipo sf pois contém colunas espaciais
names(world) # retorna as colunas
plot(world) # retorna um mapa para cada variável
summary(world) # retorna um resumo sobre cada variável

world_dfr = st_read(system.file("shapes/world.shp", package = "spData")) # retorna um df
world_tbl = read_sf(system.file("shapes/world.shp", package = "spData")) # retorna tibble

# Criação básica de mapas
plot(world[3:6]) # plotou colunas 3 a 6
plot(world["pop"]) # plotou coluna pop

world_asia = world[world$continent == "Asia", ]  # df apenas com páises da Ásia
asia = st_union(world_asia) # uniou todas as geometrias da Ásia

plot(world["pop"], reset = FALSE)
plot(asia, add = TRUE, col = "red") # adicionando uma nova camada ao gráfico já criado

india = world[world$name_long == "India", ] # df com os dados da Índia
plot(st_geometry(india), expandBB = c(0, 0.2, 0.1, 1), col = "gray", lwd = 3) # plotou com expansao e margem 
plot(st_geometry(world_asia), add = TRUE) # adiciona uma camada com todos os países da Ásia

# Tipos de geometria (sfg - simple feature geometry)

# Ponto: POINT (x y)
# Linha: sequência de pontos - LINESTRING (x1 y1, x2 y2, x3 y3, ...)
# Polígono: forma fechada, formando uma área - POLYGON ((x1 y1, x2 y2, x3 y3, x4 y4, x5 y5))
# Multiponto: conjunto de pontos - MULTIPOINT (x1 y1, x2 y2, x3 y3, x4 y4)
# Multilinha: conjunto de linhas - MULTILINESTRING ((x1 y1, x2 y2, x3 y3, x4 y4), (x5 y5, x6 y6))
# Multipolígono: conjunto de polígonos - MULTIPOLYGON (((x1 y1, x2 y2, x3 y3, x4 y4, x5 y5), (x6 y6, x7 y7, x8 y8)))
# Coleção de geometria: pode conter qualquer combinação de geometrias
# GEOMETRYCOLLECTION (MULTIPOINT (x1 y1, x2 y2, x3 y3, x4 y4), LINESTRING (x5 y5, 4x6 y6, x7 y7, x8 y8))

# Criação de geometrias

# Para criar, usamos sf_point(), sf_polygon(), ... , sf_tipodegeometria()
point1 = st_point(c(5, 2))
# Geometrias com múltiplos pontos ou segmentos conectados (multiponto e linha) criamos com matrizes
line1_matrix = rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2))
line1 = st_linestring(line1_matrix)

multipoints1_list = rbind(c(1, 2), c(3, 4), c(5, 6))
multipoints1 = st_multipoint(multipoints1_matrix)
# Geometrias mais complexas, compostas ou coleções (poligonos, multilinha, multipoligono, coleçao) criamos com listas
polygon_list1 = list(rbind(c(1, 5), c(2, 2), c(4, 1), c(4, 4), c(1, 5)))
polygon1 = st_polygon(polygon_list1)

multilinestring_list1 = list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4)))
multilinestring1 = st_multilinestring((multilinestring_list1))

# Criação de colunas com dados geométricos - sfc simple feature column

# Usamos a função st_sfc()
point_multilinestring_sfc = st_sfc(point1, multilinestring1)

# Verificar o tipo da geometria sfg e da coluna sfc
# Função st_geometry_type
st_geometry_type(point1)
st_geometry_type(point_multilinestring_sfc)

# Por padrão, as geometrias são criadas sem CRS. 
# Podemos especificar o CRS com o parâmetro crs.
# Obs: todas as geometrias em sfc devem ter mesmo CRS.
point_multilinestring_sfc_wgs = st_sfc(point1, multilinestring1,crs = "EPSG:4326")
# Verificar CRS
st_crs(point_multilinestring_sfc_wgs)

# WGS 84 está para EPSG assim como metro está para unidade de comprimento.

####################
### DADOS RASTER ###
####################

# Dados raster: células (ou pixels) de tamanhos diferentes. 
# Bom para dados contínuos.
# SpatRaster: objeto do pacote terra que contém infomações sobre uma grade de células.

# Criação de objeto SpatRaster
# Normalmente, ler um arquivo raster do disco ou de um servidor.

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath) # Cria um objeto SpatRaster
class(my_rast)

# Criação de mapas
plot(my_rast)

##################
### EXERCÍCIOS ###
##################

# E1. Use summary() na coluna de geometria do sf world.
# O que a saída nos diz sobre:
summary(world$geom)
# Seu tipo de geometria? MULTIPOLYGON
# O número de países? 177
# Seu sistema de referência de coordenadas (CRS)? EPSG: 4326


# E3. Use plot() para criar mapas da Nigéria em contexto.
# Ajuste os argumentos lwd, col e expandBB de plot().
nigeria = world[world$name_long == "Nigeria", ]
world_africa = world[world$continent == "Africa", ]
africa = st_union(world_africa)
plot(st_geometry(nigeria), expandBB = c(0.5, 0.2, 0.5, 0.2), col = "gray", lwd = 3)
plot(st_geometry(world_africa), add = TRUE)
world_cents = st_centroid(world) # criando objetos que representam o centro das geometrias
world_coords = st_coordinates(world_cents) # extraindo as coordenadas dos centroids
text(world_coords, world$iso_a2) # adição do texto ao gráfico (coordenada, texto)


# E4. Crie um SpatRaster vazio chamado my_raster com 10 colunas e 10 linhas. 
# Atribua valores aleatórios entre 0 e 10 ao novo raster e plote-o.
my_raster = rast(ncol = 10, nrow = 10,
                 vals = sample(0:10, size = 10 * 10, replace = TRUE))
plot(my_raster)

# E5. Leia o raster/nlcd.tifarquivo do pacote spDataLarge . 
# Que tipo de informação você pode obter sobre as propriedades deste arquivo?
nlcd = rast(system.file("raster/nlcd.tif", package = "spDataLarge"))
dim(nlcd) # linhas, colunas e camadas
res(nlcd) # resolução: tamanho dos pixels
ext(nlcd) # extensão: limites das coordenadas x e y
nlyr(nlcd) # número de camadas
cat(crs(nlcd)) # CRS

# E6. Verifique o CRS do raster/nlcd.tifarquivo do pacote spDataLarge . 
# Que tipo de informação você pode aprender com ele?

# Sistema de Referência de Coordenadas Projetado em Duas Dimensões: PROJCRS["NAD83 / UTM zone 12N",.
# Elipsoide GRS 1980, Datum Norte-Americano de 1983 e Meridiano de Greenwich: BASEGEOGCRS["NAD83", DATUM["North American Datum 1983", ELLIPSOID["GRS 1980", ...]], PRIMEM["Greenwich", ...]].
# Projeção de Mercator Transversal (UTM zona 12N): CONVERSION["UTM zone 12N", METHOD["Transverse Mercator", ...]].
# Eixos Relacionados a Leste e Norte com Unidades em Metros: CS[Cartesian,2], AXIS["(E)",east, ...], AXIS["(N)",north, ...].
# SRID: ID["EPSG",26912]].