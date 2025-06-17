# Script utilizado para criar o mapa do Brail com os centroides de cada estudo utilizado no mestrado e com o limite historico
# da Mata Atlantica.

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(sf)
library(dplyr)
library(tmap)

# Carrega dados dos estudo
dados_gpkg <- st_read("E:/Mestrado/Tabelas_analises/05_mapas/dados_fragmentos.gpkg")

# Converter para EPSG:4674 (SIRGAS 2000). Mesma projecao dos outros objetos
dados_gpkg <- st_transform(dados_gpkg, crs = 4674)

# Carregue o shapefile limite do Brail, limite dos estados e limite da Mata Atlantica
shapefile_paises <- st_read("E:/Mestrado/Tabelas_analises/05_mapas/lml_pais_a.shp")
shapefile_estados <- st_read("E:/Mestrado/Tabelas_analises/05_mapas/lml_unidade_federacao_a.shp")
mata_atlantica <- st_read("E:/Mestrado/Tabelas_analises/05_mapas/mata_atlantica.gpkg")

# Calculo centroides estudos
# Agrupar os dados por study_id
dados_agrupados <- dados_gpkg %>%
  group_by(study_id) %>%
  summarize(centroid = st_centroid(st_union(geom)))


# Criar a coluna ID com IDs numericos formatados
dados_agrupados <- dados_agrupados %>%
  mutate(ID = sprintf("%02d", as.numeric(factor(study_id, levels = unique(study_id)))))

# Seleciona as colunas de interesse
dados_agrupados <- select(dados_agrupados, ID, centroid)

# Extrair coordenadas de latitude e longitude
dados_agrupados2 <- st_coordinates(dados_agrupados)

# Converter a matriz de coordenadas em um objeto sf
dados_agrupados <- st_sf(cbind(dados_agrupados, dados_agrupados2))

# Renomear as colunas de coordenadas
colnames(dados_agrupados)[which(names(dados_agrupados) == "X")] <- "long"
colnames(dados_agrupados)[which(names(dados_agrupados) == "Y")] <- "lat"

# Fazer o mapa 
tm_shape(mata_atlantica) +
  tm_polygons(col = "chartreuse3") +
  tm_shape(shapefile_estados) +
  tm_borders(col = "black") +
  tm_shape(dados_agrupados) +
  tm_symbols(col = "ID", shape = 21, size = 0.8, title.col = "Estudos") +
  tm_compass(position = c("center", "bottom")) +
  tm_scale_bar(position = c("center", "bottom"), text.size = 0.6) 
