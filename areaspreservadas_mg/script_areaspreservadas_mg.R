#########################################################
###    ANÁLISE DE ÁREAS PROTEGIDAS EM MINAS GERAIS    ###
#########################################################

# Carregar pacote necessário
library(sf)
library(tidyverse)
library(ggplot2)
library(units)

##########################
### Extração dos dados ###
##########################

# Carregar shapefile dos setores censitários de Minas Gerais
unidades_conservacao <- st_read("limites_ucs_federais_20062024_a/limites_ucs_federais_20062024_a.shp")

# Filtrar para obter apenas de Minas Gerais
unidades_conservacao_mg <- unidades_conservacao %>%
  filter(UFAbrang == "MG")

# Carregar shapefile dos setores censitários de Minas Gerais
setores_mg <- st_read("MG_Malha_Preliminar_2022/MG_Malha_Preliminar_2022.shp")

# Checar a estrutura dos dados
str(unidades_conservacao_mg)
str(setores_mg)

# Verificar a projeção dos dados
st_crs(unidades_conservacao_mg)
st_crs(setores_mg)

# Reprojetar setores_mg para o CRS de unidades_conservacao_mg
setores_mg <- st_transform(setores_mg, crs = st_crs(unidades_conservacao_mg))

# Resumo estatístico
summary(unidades_conservacao_mg)
summary(setores_mg)

# Exibir algumas estatísticas das colunas de área
summary(unidades_conservacao_mg$AreaHaAlb)  # Provavelmente hectares
summary(setores_mg$AREA_KM2)                # Quilômetros quadrados

# Verificar valores ausentes
sum(is.na(unidades_conservacao_mg))
sum(is.na(setores_mg))

# Visualizar a distribuição e tamanho das áreas protegidas
ggplot(data = unidades_conservacao_mg) +
  geom_sf(fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Distribuição das Áreas Protegidas em Minas Gerais",
       subtitle = "Tamanho das Áreas Protegidas",
       caption = "Fonte: ICMBio")


#########################################
### Limpeza e Transformação dos dados ###
#########################################

# UNIDADES_CONSERVACAO_MG

# Selecionar as colunas importantes para a análise
unidades_conservacao_mg <- unidades_conservacao_mg %>%
  select(NomeUC, CriacaoAno, AreaHaAlb, PerimM, EsferaAdm, SiglaCateg, 
         GrupoUC, BiomaIBGE, BiomaCRL, NGI, geometry)

# Renomeando atributos
# Coluna GrupoUC
unidades_conservacao_mg <- unidades_conservacao_mg %>%
  mutate(GrupoUC = case_when(
    GrupoUC == "PI" ~ "Proteção Integral",
    GrupoUC == "US" ~ "Uso Sustentável"
  ))
# Coluna SiglaCateg
unidades_conservacao_mg <- unidades_conservacao_mg %>%
  mutate(SiglaCateg = case_when(
    SiglaCateg == "PARNA" ~ "Parque Nacional",
    SiglaCateg == "ESEC" ~ "Estação Ecológica",
    SiglaCateg == "APA" ~ "Área de Proteção Ambiental",
    SiglaCateg == "FLONA" ~ "Flora Nacional",
    SiglaCateg == "RDS" ~ "Reserva de Desenvolvimento Sustentável",
    SiglaCateg == "REBIO" ~ "Reserva Biológica"
  ))

# Renomear colunas
unidades_conservacao_mg <- unidades_conservacao_mg %>%
  rename(
    Nome_Unidade = NomeUC,
    Ano_Criacao = CriacaoAno,
    Area_Ha = AreaHaAlb,
    Perimetro_M = PerimM,
    Esfera_Adm = EsferaAdm,
    Categoria = SiglaCateg,
    Grupo_UC = GrupoUC,
    Bioma_IBGE = BiomaIBGE,
    Bioma_Relativo = BiomaCRL,
    NGI_Identificador = NGI,
    geometry = geometry
  ) 

# SETORES_MG
# Agrupar por município, somar as áreas e unir as geometrias
setores_mg_unidos <- setores_mg %>%
  group_by(NM_MUN) %>%
  summarise(
    NM_MESO = unique(NM_MESO),
    total_area_ha = sum(AREA_KM2, na.rm = TRUE)*100, 
    geometry = st_union(geometry)                   
  )

# Renomear colunas
setores_mg_mun <- setores_mg_unidos %>%
  rename(
    Nome_Municipio = NM_MUN,
    Nome_Mesoregiao = NM_MESO,
    Area_Ha = total_area_ha,
    geometry = geometry
  ) 

# Visualizar a distribuição dos Setores_MG
ggplot(data = setores_mg_mun) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Distribuição dos Munícipios em Minas Gerais",
       caption = "Fonte: IBGE")

########################################
### Análise e Visualização dos Dados ###
########################################

# Em quais municípios temos UCs? Quantas?
unidades_conservacao_mg <- st_cast(unidades_conservacao_mg, "MULTIPOLYGON")
setores_mg_mun <- st_cast(setores_mg_mun, "MULTIPOLYGON")
intersecao_uc_mun <- st_intersection(unidades_conservacao_mg, setores_mg_mun)
intersecao_uc_mun <- intersecao_uc_mun %>%
  mutate(Area_Ha_Interseccao = st_area(geometry) / 10000) # Convertendo m² para ha
intersecao_uc_mun$geometry <- st_cast(intersecao_uc_mun$geometry, "MULTIPOLYGON")
str(intersecao_uc_mun)

uc_por_municipio <- intersecao_uc_mun %>%
  group_by(Nome_Municipio) %>%
  summarise(
    Numero_UCs = n()
  ) %>% 
  arrange(desc(Numero_UCs))

# Cada UC está localizada em quais municípios?
uc_cidades <- intersecao_uc_mun %>%
  group_by(Nome_Unidade) %>%
  summarise(Cidades = paste(unique(Nome_Municipio), collapse = ", "))

#Quais mesorregiões têm a maior concentração de Unidades de Conservação?
setores_mg_meso <- setores_mg_mun %>%
  group_by(Nome_Mesoregiao) %>%
  summarise(
    Area_Ha = sum(Area_Ha, na.rm = TRUE), 
    geometry = st_union(geometry)                   
  )
intersecao_uc_meso <- st_intersection(unidades_conservacao_mg, setores_mg_meso)
intersecao_uc_meso <- intersecao_uc_meso %>%
  mutate(Area_Ha_Interseccao = st_area(geometry) / 10000) # Convertendo m² para ha
intersecao_uc_meso$geometry <- st_cast(intersecao_uc_meso$geometry, "MULTIPOLYGON")
intersecao_uc_meso$UC_ID <- as.factor(intersecao_uc_meso$Nome_Unidade)

uc_por_mesoregiao <- intersecao_uc_meso %>%
  group_by(Nome_Mesoregiao) %>%
  summarise(
    Numero_UCs = n()
  ) %>% 
  arrange(desc(Numero_UCs))

ggplot() +
  geom_sf(data = setores_mg_meso, fill = "lightgrey", color = "black") +
  geom_sf(data = intersecao_uc_meso, aes(fill = UC_ID), color = "black", alpha = 0.5) +
  labs(title = "Mesorregiões de Minas Gerais com Unidades de Conservação",
       subtitle = "Cada Unidade de Conservação tem uma cor diferentes",
       fill = "Categoria") +
  theme_minimal() +
  theme(legend.position = "none")

# Cada UC está localizada em quais municípios?
uc_meso <- intersecao_uc_meso %>%
  group_by(Nome_Unidade) %>%
  summarise(Mesoregião = paste(unique(Nome_Mesoregiao), collapse = ", "))

#Qual a proporção da área total de Minas Gerais coberta por Unidades de Conservação?

area_total_mg <- sum(setores_mg_mun$Area_Ha)
area_uc_total <- sum(unidades_conservacao_mg$Area_Ha)
proporcao_uc <- area_uc_total / area_total_mg
proporcao_uc

# Quais são os municípios com maior e menor cobertura por Unidades de Conservação em termos de área?

uc_area_por_municipio <- intersecao_uc_mun %>%
  st_drop_geometry() %>% 
  group_by(Nome_Municipio) %>%
  summarise(
    Area_Total_UCs = sum(Area_Ha_Interseccao, na.rm = TRUE)
  )

area_municipio_total <- setores_mg_mun %>%
  st_drop_geometry() %>% 
  select(Nome_Municipio, Area_Ha) %>%
  distinct()

uc_area_municipio_completo <- uc_area_por_municipio %>%
  left_join(area_municipio_total, by = "Nome_Municipio") %>%
  filter(!is.na(Area_Ha)) %>%  # Filtrar apenas municípios com área definida
  mutate(
    Proporcao_UCs = (Area_Total_UCs / Area_Ha) * 100
  ) %>%
  arrange(desc(Proporcao_UCs))

maior_proporcao_uc <- uc_area_municipio_completo %>%
  filter(Proporcao_UCs == max(Proporcao_UCs, na.rm = TRUE))

menor_proporcao_uc <- uc_area_municipio_completo %>%
  filter(Proporcao_UCs == min(Proporcao_UCs, na.rm = TRUE))

print(maior_proporcao_uc)
print(menor_proporcao_uc)

#Como a criação das Unidades de Conservação evoluiu ao longo dos anos em Minas Gerais?

evolucao_uc <- unidades_conservacao_mg %>%
  mutate(Ano_Criacao = as.numeric(Ano_Criacao)) %>% 
  group_by(Ano_Criacao) %>%
  summarise(
    Numero_UCs = n(),
    Area_Total_UCs = sum(Area_Ha, na.rm = TRUE)
  ) %>%
  arrange(Ano_Criacao)

ggplot(evolucao_uc_acumulativo, aes(x = Ano_Criacao, y = Numero_UCs_Cumulativo)) +
  geom_line(color = "black") +  
  geom_point(color = "red") +  
  geom_text(aes(label = Numero_UCs_Cumulativo), vjust = -0.5, color = "black") +  
  labs(
    title = "Número Acumulativo de Unidades de Conservação Criadas por Ano em Minas Gerais",
    x = "Ano de Criação",
    y = "Número Acumulativo de UCs"
  ) +
  scale_x_continuous(
    breaks = unique(evolucao_uc_acumulativo$Ano_Criacao),  
    limits = c(min(evolucao_uc_acumulativo$Ano_Criacao) - 1, max(evolucao_uc_acumulativo$Ano_Criacao) + 1)  
  ) +
  scale_y_continuous(
    breaks = seq(0, max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo, na.rm = TRUE), by = max(1000, ceiling(max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo) / 10))),  
    limits = c(0, max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo, na.rm = TRUE) * 1.1) 
  ) +
  theme_minimal() +  # Tema minimalista para o gráfico
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Calcular a soma acumulativa das UCs por ano
evolucao_uc_acumulativo <- evolucao_uc %>%
  group_by(Ano_Criacao) %>%
  summarise(
    Numero_UCs = sum(Numero_UCs, na.rm = TRUE)
  ) %>%
  arrange(Ano_Criacao) %>%
  mutate(
    Numero_UCs_Cumulativo = cumsum(Numero_UCs)
  )

ggplot(evolucao_uc_acumulativo, aes(x = Ano_Criacao, y = Numero_UCs_Cumulativo)) +
  geom_line(color = "blue") +  # Linha azul para o gráfico
  geom_point(color = "red") +  # Pontos vermelhos para os dados
  labs(
    title = "Número Acumulativo de Unidades de Conservação Criadas por Ano em Minas Gerais",
    x = "Ano de Criação",
    y = "Número Acumulativo de UCs"
  ) +
  scale_x_continuous(
    breaks = unique(evolucao_uc_acumulativo$Ano_Criacao),  # Define os ticks apenas para os anos presentes nos dados
    limits = c(min(evolucao_uc_acumulativo$Ano_Criacao) - 1, max(evolucao_uc_acumulativo$Ano_Criacao) + 1)  # Adiciona margem para visualização
  ) +
  scale_y_continuous(
    breaks = seq(0, max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo, na.rm = TRUE), by = max(1000, ceiling(max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo) / 10))),  # Ajusta o intervalo no eixo y
    limits = c(0, max(evolucao_uc_acumulativo$Numero_UCs_Cumulativo, na.rm = TRUE) * 1.1)  # Ajusta os limites para que todos os valores estejam visíveis
  ) +
  theme_minimal() +  # Tema minimalista para o gráfico
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotaciona os labels do eixo x se necessário
    axis.text.y = element_text(vjust = 0.5)  # Ajusta o alinhamento dos rótulos do eixo y
  )

# Qual a proporção da área total de Unidades de Conservação por Categoria?
proporcao_categoria <- unidades_conservacao_mg %>%
  group_by(Categoria) %>%
  summarise(
    Area_Total = sum(Area_Ha, na.rm = TRUE)
  ) %>%
  mutate(Proporcao_Porc = Area_Total / sum(Area_Total) * 100) %>% 
  arrange(desc(Proporcao_Porc))

proporcao_categoria
ggplot(proporcao_categoria, aes(x = reorder(Categoria, Proporcao_Porc), y = Proporcao_Porc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Proporcao_Porc)), hjust = -0.2, size = 3.5) + 
  coord_flip() +
  labs(
    title = "Proporção da Área Total por Categoria de Unidade de Conservação",
    x = "Categoria",
    y = "Proporção (%)"
  ) +
  theme_minimal()


# Como é a distribuição das Unidades de Conservação por Bioma?
ggplot(data = unidades_conservacao_mg) +
  geom_sf(aes(fill = Bioma_IBGE), color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribuição das Unidades de Conservação por Bioma",
       fill = "Bioma")

# Como a área total das Unidades de Conservação varia entre os grupos de uso?
ggplot(data = unidades_conservacao_mg) +
  geom_bar(aes(x = Grupo_UC, y = Area_Ha, fill = Grupo_UC), stat = "summary", fun = "sum") +
  theme_minimal() +
  labs(title = "Área Total de Unidades de Conservação por Grupo de Uso",
       x = "Grupo de Uso",
       y = "Área Total (ha)")

# Como a área média das Unidades de Conservação varia entre os grupos de uso?
media_area <- unidades_conservacao_mg %>%
  group_by(Grupo_UC) %>%
  summarise(Media_Area_Ha = mean(Area_Ha, na.rm = TRUE))

ggplot(data = media_area) +
  geom_bar(aes(x = Grupo_UC, y = Media_Area_Ha, fill = Grupo_UC), stat = "identity") +
  theme_minimal() +
  labs(title = "Média da Área de Unidades de Conservação por Grupo de Uso",
       x = "Grupo de Uso",
       y = "Média da Área (ha)")
