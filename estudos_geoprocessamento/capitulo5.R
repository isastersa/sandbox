library(sf) # pacote para trabalhar com dados vetoriais
library(terra) # pacote para trabalhar com dados raster
library(dplyr)
library(spData)
library(spDataLarge)
library(smoothr)

#############################
### Operações Geométricas ###
#############################

# Operações Geométricas com dados vetoriais
# Operações que de alguma forma alteram  a geometria
# Tipos de operações: Simplificação, Centróides, Buffers, Transformações afins, 
#                     Recorte, Uniões Geométricas e Transformações de tipo
# Podemos modificar a geometria original com st_set_geometria(data_antigo, data_novo)

# Simplificação: generalizar objetos vetoriais (reduz a quantidade de vértice). 
# Porque fazer? Mapas com menor escala, reduzir espaço de memoria.
# Métodos para simplificação: st_simplify, ms_simplify, smoothr::smooth


# st_simplify(data, dTolerance = "x" (nível de generalização))
# dTolerance: A distância entre os pontos da geometria original e da simplificada não exceda x (em metros)
us_states_simp1 = st_simplify(us_states, dTolerance = 100000)  # 100 km
plot(st_geometry(us_states_simp1)) # com st_simplify as geometrias ficam sobrepostas/furadas, perdendo sua topologia
object.size(us_states)
object.size(us_states_simp1)

# rmapshaper::ms_simplify(data, keep = "porcentagem dos vertices mantidos", keep_shapes = TRUE "preservar a forma")
us_states_simp2 = rmapshaper::ms_simplify(us_states, keep = 0.01, keep_shapes = TRUE)
plot(st_geometry(us_states_simp2)) # topologia preservada
object.size(us_states)
object.size(us_states_simp2) # a redução em espaço é um pouco maior

# smoothr::smooth(data, method = "ksmooth"(esse método aplica uma suavização para cada ponto da borda), smoothness = "nível de suavização")
# útil para aparência estética pós vetorização de dados raster
us_states_simp3 = smoothr::smooth(us_states, method = "ksmooth", smoothness = 6)
plot(st_geometry(us_states_simp3)) # topologia também sofre alterações
object.size(us_states)
object.size(us_states_simp3) # não reduz tanto espaço quanto st_simplify e ms_simplify. smoothness não muda a redução


# Centróides: identificam o centro de objetos geográficos
# Porque fazer? Criar representação de ponto simples ou estimar distância entre polígonos
# Métodos: st_centroid, st_point_on_surface

# st_centroid
seine_centroid = st_centroid(seine)
plot(st_geometry(seine))
plot(st_geometry(seine_centroid), add = TRUE) # tem pontos fora do limite

# st_point_on_surface: garante que o ponto estará no objeto
seine_pos = st_point_on_surface(seine)
plot(st_geometry(seine))
plot(st_geometry(seine_pos), add = TRUE, col = "red") 


# Buffers: representam polígono dentro de uma distância dada.
# Porque fazer? Análise de dados geográficos
# Métodos: st_buffer

# st_buffer(data, dist = em metros)
seine_buff_5km = st_buffer(seine, dist = 5000)
plot(st_geometry(seine))
plot(st_geometry(seine_buff_5km), add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))

# Parâmetros em st_buffer: 
# nQuadSegs: Ajusta a suavidade do buffer, aumentando o número de segmentos para maior precisão.
# max_cells: Ajusta a suavidade do buffer com o mecanismo S2, aumentando o número de células.
# endCapStyle e joinStyle: Controlam a aparência das bordas e junções do buffer.
# singleSide: Define se o buffer é criado apenas em um lado ou em ambos os lados da geometria.


# Transformações afins: transformação que preserva linhas e paralelismo (não necessariamente ângulos ou comprimentos)
# Porque fazer? Alinhar dados espaciais, redimensionar para análises
# Exemplos de transformações: deslocamento(translação), escala e rotação.

# Deslocamento
nz_deslocada = st_geometry(nz) + c(0, 100000) # desloca as coordenadas y em 100 km
plot(st_geometry(nz))
plot(st_geometry(nz_deslocada), add = TRUE, col = "red")

# Escala: multiplicação por um fator que aumento (x>1) ou diminui (0<x<1)
nz_centroid_sfc = st_centroid(st_geometry(nz)) # centroides de nz
nz_scale = (st_geometry(nz) - nz_centroid_sfc) * 0.5 + nz_centroid_sfc # faz com que todos os centroides fiquem na origem, escalona por 0.5 e depois retorna ao lugar da centroide
plot(st_geometry(nz))
plot(st_geometry(nz_scale), add = TRUE, col = "red")

# Rotação: feita por matriz rotação. Gira em sentido horário.
rotacao = function(a){ # parâmetro a em graus
  r = a * pi / 180 # graus para radianos
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), nrow = 2, ncol = 2)
} 

nz_rotate = (st_geometry(nz)-nz_centroid_sfc)*rotacao(90)+nz_centroid_sfc # rotaciona cada objeto no angulo de 90
plot(st_geometry(nz))
plot(st_geometry(nz_rotate), add = TRUE, col = rgb(1, 0, 0, alpha = 0.5))

# Recorte: forma de subconjunto que envolve alterações nas geometrias
# Porque fazer? Focar na área de interesse, reduzir o tamanho dos dados, visualização limpa
# Métodos: utilizando as relações topológicas

# Criando dois círculos de raio 1
b = st_sfc(st_point(c(0, 1)), st_point(c(1, 1))) # criando dois pontos
b = st_buffer(b, dist = 1) # transformando em círculos
plot(b, border = "grey")

x = b[1] # x é o primeiro objeto (a esquerda)
y = b[2] # y é o segundo objeto 

# Interseção entre x e y
x_and_y = st_intersection(x,y)
plot(b, border = "grey")
plot(x_and_y, col = "red", add = TRUE)

# Diferença entre X e Y (X-Y)
x_diff_y = st_difference(x,y)
plot(b, border = "grey")
plot(x_diff_y, col = "red", add = TRUE)

# Diferença entre Y e X (Y-X)
y_diff_x = st_difference(y,x)
plot(b, border = "grey")
plot(y_diff_x, col = "red", add = TRUE)

# União entre X e Y (X U Y)
x_uniao_y = st_union(x,y)
plot(b, border = "grey")
plot(x_uniao_y, col = "red", add = TRUE)

# Disjunção entre X e Y 
x_disj_y = st_sym_difference(x,y)
plot(b, border = "grey")
plot(x_disj_y, col = "red", add = TRUE)


# Uniões Geométricas: Combina duas ou mais geometrias em uma nova geometria. 
# Porque fazer? Agrupamento de áreas para análise, simplificação dos dados, criação de áreas de inffluência
# Método: st_union

us_west = us_states[us_states$REGION == "West", ] # Identifica todos os estados da Região WEST
us_west_union = st_union(us_west) # Une em um objeto só os estados da Região WEST
plot(st_geometry(us_states))
plot(st_geometry(us_west_union), add = TRUE, col = "blue")

texas = us_states[us_states$NAME == "Texas", ] # Identifica o Texas
texas_union = st_union(us_west_union, texas) # Une o Texas à Região WEST
plot(st_geometry(us_states))
plot(st_geometry(texas_union), add = TRUE, col = "blue")

# Transformações de tipo
# Porque fazer? Compatibilidade de dados
# Método: st_cast(data, "NOVO_TIPO")

multipoint = st_multipoint(matrix(c(1, 3, 5, 1, 3, 1), ncol = 2))
linestring = st_cast(multipoint, "LINESTRING")
polyg = st_cast(multipoint, "POLYGON")

plot(multipoint)
plot(linestring, add = TRUE, col = rgb(1,0,0, alpha = 0.5))
plot(polyg, add = TRUE, col = rgb(0,1,0, alpha = 0.5))


# Operações geométricas com dados raster
# Tipos de operações: Deslocamento, Inversão, Espelhamento, Dimensionamento,
#                     Rotação ou Distorção de imagens

# Interseções geométricas
elev = rast(system.file("raster/elev.tif", package = "spData"))
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))
elev[clip, drop = FALSE]

# Extensão e origem 
elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2)) # aumenta os limites do raster (1 nas linhas e 2 em colunas)

plot(elev)
plot(elev_2)

elev_3 = extend(elev, elev_2) # elev foi ampliado para ter a mesma extensão de elev_2
plot(elev_3)

# Agregação e desagregação 
# Quando queremos alterar a resolução pelo fator de (des)agregação

# Agregação: diminuir a resolução - aggregate()
# agregate(data, fact = "cada celula será um nxn", fun = "funcao para calcular o valor representativo")
dem = rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg = aggregate(dem, fact = 5, fun = mean)
par(mfrow = c(1, 2))
plot(dem)
plot(dem_agg)
par(mfrow = c(1, 1)
print(dem) 
print(dem_agg) # Diminuiu a dimensão e resolução, com isso o espaço ocupado na memória

# Desagregação: aumentar a resolução - disagg()
# disagg(data, fact = n, method = "bilinear ou near"))

dem_disagg1 = disagg(dem_agg, fact = 5, method = "near")
dem_disagg2 = disagg(dem_agg, fact = 5, method = "bilinear")
par(mfrow = c(2, 2))
plot(dem_agg)
plot(dem_disagg1) # Apenas duplica a entrada com o método near
plot(dem_disagg2) # Não fica igual ao originar por é uma interpolação dos valores
plot(dem)
par(mfrow = c(1, 1))

# Reamostragem: pega valores originais e recalcula novos valores para um raster alvo com resolução e origem personalizadas
# resample(raster_original, y = raster_alvo, method = "x")
target_rast = rast(xmin = 794650, xmax = 798250, 
                   ymin = 8931750, ymax = 8935350,
                   resolution = 300, crs = "EPSG:32717")

# Parâmetro method:
# 1. near: Atribui o valor da célula mais próxima do raster original, adequada para reamostragem categórica.
dem_near = resample(dem, y = target_rast, method = "near")
par(mfrow = c(1, 2))
plot(dem)
plot(dem_near)
par(mfrow = c(1, 1))
# 2. bilinear: Atribui média ponderada das 4 mais próximas do raster original. Adequado para rasters contínuos. 
dem_bilinear = resample(dem, y = target_rast, method = "bilinear")
par(mfrow = c(1, 2))
plot(dem)
plot(dem_bilinear)
par(mfrow = c(1, 1))
# 3. cubic: Usa função polinomial de 3 ordem nas 16 células mais próximas. Rasters contínuos
dem_cubic = resample(dem, y = target_rast, method = "cubic")
par(mfrow = c(1, 2))
plot(dem)
plot(dem_cubic)
par(mfrow = c(1, 1))
# 4. cubicspline: Igual cubic, mas aplica a função em partes, resultando em maior suavidade
dem_cubicspline = resample(dem, y = target_rast, method = "cubicspline")
par(mfrow = c(1, 2))
plot(dem)
plot(dem_cubicspline)
par(mfrow = c(1, 1))
# 5. lanczos: Usa as 36 células mais próximas
dem_lanczos = resample(dem, y = target_rast, method = "lanczos")
par(mfrow = c(1, 2))
plot(dem)
plot(dem_lanczos)
par(mfrow = c(1, 1))

##################
### Exercícios ###
##################

#E1. Gere e plote versões simplificadas do nzconjunto de dados. 
#Experimente com diferentes valores de keep(variando de 0,5 a 0,00005) para ms_simplify()
# e dTolerance(de 100 a 100.000) st_simplify().
# Em que valor a forma do resultado começa a falhar para cada método, tornando a Nova Zelândia irreconhecível?
par(mfrow = c(3,2))
plot(rmapshaper::ms_simplify(st_geometry(nz), keep = 0.5))
plot(rmapshaper::ms_simplify(st_geometry(nz), keep = 0.05))
# Começa a descaracterizar
plot(rmapshaper::ms_simplify(st_geometry(nz), keep = 0.005))
# Alterações não mudam mais 
plot(rmapshaper::ms_simplify(st_geometry(nz), keep = 0.0005))
plot(rmapshaper::ms_simplify(st_geometry(nz), keep = 0.00005))
par(mfrow = c(1,1))
par(mfrow = c(3,2))
plot(st_simplify(st_geometry(nz), dTolerance = 100))
plot(st_simplify(st_geometry(nz), dTolerance = 1000))
# Começa a descaracterizar
plot(st_simplify(st_geometry(nz), dTolerance = 10000))
plot(st_simplify(st_geometry(nz), dTolerance = 100000))
plot(st_simplify(st_geometry(nz), dTolerance = 100000, preserveTopology = TRUE))
par(mfrow = c(1,1))

# Avançado: O que há de diferente no tipo de geometria dos resultados de em st_simplify()
# comparação com o tipo de geometria de ms_simplify()? Quais problemas isso cria e como isso pode ser resolvido?

# O st_simplify pode retornar MULTIPOLYGON ou POLYGON, a depender da dTolerance
# O ms_simplify retorna sempre o mesmo tipo de dados de entrada
# Isso pode criar incompatibilidade de geometrias, que pode ser resolvido pelo st_cast

# E2. No primeiro exercício do Capítulo Operações de dados espaciais, 
# foi estabelecido que a região de Canterbury tinha 70 dos 101 pontos mais altos da Nova Zelândia. 
# Usando st_buffer(), quantos pontos em nz_height estão a 100 km de Canterbury?

canterbury = nz[nz$Name == "Canterbury", ]
cant_buff = st_buffer(canterbury, 100000) # regiao de 100km = 100 000 m 
nz_cat_buff= nz_height[cant_buff, ] # quais pontos estao na regiao do buffer
nrow(nz_cat_buff) 

# E3. Encontre o centroide geográfico da Nova Zelândia. 
# Quão longe ele está do centroide geográfico de Canterbury?
cant_cent = st_centroid(canterbury) # centróide de caterbury
nz_centre = st_centroid(st_union(nz)) # centróide de nz
st_distance(cant_cent, nz_centre) # calculando a distância em metros

# E4. A maioria dos mapas-múndi tem uma orientação norte para cima. 
# Um mapa-múndi com uma orientação sul para cima pode ser criado por uma reflexão da geometria de world.
# Escreva um código para fazer isso. 
# Dica: você pode usar a rotation() deste capítulo para esta transformação. 
world_reflexao = st_geometry(world) * rotacao(180) # reflexao é girar 180 graus
plot(st_geometry(world))
plot(st_geometry(world_reflexao))
# Bônus: crie um mapa de cabeça para baixo do seu país.
brasil = world[world$name_long == "Brazil", ]
brasil_reflexao = st_geometry(brasil) * rotacao(180) # reflexao é girar 180 graus
plot(st_geometry(brasil))
plot(st_geometry(brasil_reflexao))

# E5. Calcule o comprimento das linhas de fronteira dos estados dos EUA em metros. 
# Qual estado tem a fronteira mais longa e qual tem a mais curta? 
# Dica: A st_length calcula o comprimento de uma LINESTRING ou MULTILINESTRING.

us_states9311 = st_transform(us_states, "EPSG:9311") #garantir que está em metros
us_states_bor = st_cast(us_states9311, "MULTILINESTRING") #transformando para MULTILINESTRING
us_states_bor$borders = st_length(us_states_bor) # Calculando os comprimentos
estado_mais_longo <- arrange(us_states_bor, desc(borders))
print(estado_mais_longo[1,]$NAME)
estado_mais_curto <- arrange(us_states_bor, borders)
print(estado_mais_curto[1,]$NAME)

# E7. Leia o arquivo srtm.tif no R ( srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))). 
# Este raster tem uma resolução de 0,00083 por 0,00083 graus. 
# Altere sua resolução para 0,01 por 0,01 graus usando todos os métodos disponíveis no pacote terra . 
# Visualize os resultados. Você consegue notar alguma diferença entre os resultados desses métodos de reamostragem?

srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
rast_template = rast(ext(srtm), res = 0.01)
srtm_resampl1 = resample(srtm, y = rast_template, method = "bilinear")
srtm_resampl2 = resample(srtm, y = rast_template, method = "near")
srtm_resampl3 = resample(srtm, y = rast_template, method = "cubic")
srtm_resampl4 = resample(srtm, y = rast_template, method = "cubicspline")
srtm_resampl5 = resample(srtm, y = rast_template, method = "lanczos")

srtm_resampl_all = c(srtm_resampl1, srtm_resampl2, srtm_resampl3,
                     srtm_resampl4, srtm_resampl5)
plot(srtm_resampl_all)

# Diferenças
plot(srtm_resampl_all - srtm_resampl1, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl2, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl3, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl4, range = c(-300, 300))
plot(srtm_resampl_all - srtm_resampl5, range = c(-300, 300))
