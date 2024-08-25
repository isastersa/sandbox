# Análise das Áreas Preservadas em Minas Gerais

Este projeto tem como objetivo mapear e analisar as áreas preservadas em Minas Gerais, MG. O foco é entender a distribuição geográfica e o tamanho das áreas preservadas, bem como avaliar a cobertura dos diferentes tipos de unidades de conservação (UCs) no estado. 

## Descrição

O projeto inclui:

- Coleta e carregamento dos dados das áreas preservadas.

- Pré-processamento e análise espacial das unidades de conservação.

- Visualização dos resultados em mapas temáticos e interativos.

## Etapas do Projeto

### 1. Coleta de Dados

Os dados utilizados para este projeto foram coletados das seguintes fontes:
  
1. **Áreas Preservadas Nacionais:**
   - **Descrição:** Dados nacionais sobre as unidades de conservação, incluindo informações sobre nome, tipo e localização das áreas preservadas. Estes dados são fornecidos pelo ICMBio e abrangem todo o território brasileiro. Posteriormente, foi realizada a filtragem para o estado de Minas Gerais.
   - **Formato:** Shapefile
   - **Fonte:** [ICMBio - Mapa Temático e Dados Geoestatísticos das Unidades de Conservação Federais](https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais)

2. **Malha dos Setores Censitários de Minas Gerais:**
   - **Descrição:** Malha geoespacial dos setores censitários do estado de Minas Gerais.
   - **Formato:** Shapefile
   - **Fonte:** [IBGE - Instituto Brasileiro de Geografia e Estatística](https://www.ibge.gov.br/geociencias/downloads-geociencias.html?caminho=organizacao_do_territorio/malhas_territoriais/malhas_de_setores_censitarios__divisoes_intramunicipais/censo_2022_preliminar/setores/shp/UF)

### 2. Carregamento e Pré-processamento dos Dados

Nesta etapa, foram realizadas as seguintes ações:

1. **Carregamento dos Shapefiles:** 

- **Áreas Preservadas:** O shapefile com as unidades de conservação é carregado e filtrado para Minas Gerais.

- **Setores Censitários:** O shapefile dos setores censitários de Minas Gerais é carregado e reprojetado para o mesmo sistema de referência das UCs.
  
2. **Limpeza e Transformação:**

- **Unidades de Conservação:** Seleção das colunas relevantes, renomeação e recodificação de variáveis para facilitar a análise.

- **Setores Censitários:** Agrupamento por município, soma das áreas e união das geometrias.

### 3. Análise Espacial

1. **Distribuição das Áreas Preservadas:**

- Mapeamento da distribuição e tamanho das áreas protegidas em Minas Gerais.

2. **Cobertura por Municípios e Mesorregiões:**

- Identificação dos municípios e mesorregiões com maior e menor concentração de Unidades de Conservação.

- Análise da proporção da área total coberta por UCs.

3. **Evolução ao Longo dos Anos:**

- Avaliação da evolução na criação das Unidades de Conservação ao longo dos anos.

### 4. Visualização dos Dados

1. **Distribuição das UCs:** 

- Mapas temáticos mostrando a distribuição das unidades de conservação por bioma e por categoria.

2. **Área Total e Média:**

- Gráficos mostrando a área total e média das UCs por grupo de uso.

3. **Proporção por Categoria e Bioma:**

- Análise da proporção da área total por categoria de UC e a distribuição das UCs por bioma.