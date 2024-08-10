#########################################################
### ANÁLISE DE ÁREAS VERDES URBANAS EM BELO HORIZONTE ###
#########################################################

# Carregar pacote necessário
install.packages("sf")
library(sf)

# Carregar shapefile dos setores censitários de Minas Gerais
setores_mg <- st_read("MG_Malha_Preliminar_2022/MG_Malha_Preliminar_2022.shp")

# Filtrar para obter apenas Belo Horizonte
setores_bh <- subset(setores_mg, NM_MUN == "Belo Horizonte")

# Visualizar os dados
plot(setores_bh)

# Carregar os dados de praças e parques de Belo Horizonte
parques_bh <- read.csv("parques_municipais_bh.csv", stringsAsFactors = FALSE)
pracas_bh <- read.csv("pracas_bh.csv", sep = ";", stringsAsFactors = FALSE)


