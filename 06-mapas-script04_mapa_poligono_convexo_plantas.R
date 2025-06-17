 # Script utilizado para criar mapas de cada estudo utilizado no mestrado. Sao calculados e denhenhados tambem os centroides de cada fragmento amostrado e os poligonos convexos 
 # dos fragemtnos amostrados mais externos. O tamanho dos mapas e calculado com base nos poligonos utilizados no desenho do polonigono convexo de cada estudo. 

 # Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(sf)
library(dplyr)
library(readr)
library(tmap)

# Caminho para os mapas florestais de cada estudo
caminho_mapas <- "E:/Mestrado/Tabelas_analises/05_mapas/fundos/"

# Dados de cada fragmento amostrado de cada estudo
dados_gpkg <- st_read("E:/Mestrado/Tabelas_analises/05_mapas/dados_fragmentos.gpkg")

# Nesse exemplo estao sendo selecionados destudos somente com plantas para fazer os mapas
dados_gpkg <- dados_gpkg[dados_gpkg$taxon == "Plantas",]

  # Inicialize a lista para armazenar os resultados
  lista_mapas_cortados <- list()
  
  # Iterar sobre cada 'study_id' unico em 'dados_gpkg'
  for (ID in unique(dados_gpkg$study_id)) {
    
    # Filtre 'dados_gpkg' para o 'study_id' atual
    dados_filtrados <- dados_gpkg[dados_gpkg$study_id == ID, ]
    
    # Calcular os centroides dos poligonos
    centroides <- st_centroid(dados_filtrados)
    
    # Extrair coordenadas de latitude e longitude
    coordinates <- st_coordinates(centroides)
    
    # Criar as novas colunas 'lat' e 'long'
    dados_filtrados$lat <- coordinates[, "Y"]
    dados_filtrados$long <- coordinates[, "X"]
    
    # Carregar o mapa correspondente ao 'ID' e ano
    mapa_arquivo <- paste0(caminho_mapas, ID, ".gpkg")
    mapa_gpkg <- st_read(mapa_arquivo)
    
    # Calcular os centroides dos poligonos
    centroides2 <- st_centroid(mapa_gpkg)
    
    # Extrair coordenadas de latitude e longitude
    coordinates2 <- st_coordinates(centroides2)
    
    # Criar as novas colunas 'lat' e 'long' no mapa
    mapa_gpkg$lat <- coordinates2[, "Y"]
    mapa_gpkg$long <- coordinates2[, "X"]
    
    # Obter os limites do objeto 'dados_gpkg'
    bbox_dados_reproj <- st_bbox(dados_filtrados)
    
    # Extraia as coordenadas xmin, ymin, xmax, ymax
    xmin <- bbox_dados_reproj["xmin"]
    ymin <- bbox_dados_reproj["ymin"]
    xmax <- bbox_dados_reproj["xmax"]
    ymax <- bbox_dados_reproj["ymax"]
    
    # Criar um objeto sf contendo a area de corte
    bbox <- st_bbox(c(xmin, ymin, xmax, ymax))
    bbox_sf <- st_as_sfc(st_bbox(bbox), crs = st_crs(mapa_gpkg))
    
    # Realizar o corte usando st_crop
    mapa_cortado <- st_crop(mapa_gpkg, bbox_sf)
    
    # Adiciona 'study_id' ao mapa cortado
    mapa_cortado$study_id <- ID
    
    # Utilizar a funcao left_join() para juntar os data frames
    mapas_cortados_junto <- left_join(mapa_cortado, ids, by = "study_id")
    
    # Armazenar cada resultado em 'lista_mapas_cortados'
    lista_mapas_cortados[[paste0("mapas_cortados_", ID)]] <- mapas_cortados_junto
  }
  
  # Unir todos os data frames em um unico data frame
  mapas_cortados_unico <- do.call(rbind, lista_mapas_cortados)

# Calcular poligono convexo
#
hulls <- st_as_sf(dados_gpkg) %>%
 group_by(study_id) %>%
  summarise(geometry = st_combine(geom)) %>%
 st_convex_hull()

hulls <- left_join(hulls, ids, by = "study_id")

# Calcular a area de cada poligono convexo
hulls_area <- hulls %>%
  st_area()

# Converter a area para hectares usando o pacote units
hulls_area <- units::set_units(x = hulls_area, value = ha) %>%
  as_tibble() %>%
  mutate(landscape_area_ha = as.numeric(value)) %>%
  # Adicionar study_id do conjunto de dados original dos poligonos convexos
  mutate(study_id = hulls$study_id)

# Selecionar apenas as colunas relevantes (study_id, landscape_area_ha)
# Arredondo o valor das areas
hulls_area$landscape_area_km2 <- round(hulls_area$landscape_area_ha* 0.0001, 3)
#hulls_area <- left_join(hulls_area, ids, by = "study_id")
hulls <- left_join(hulls, hulls_area, by = "study_id")

# Criar mapas facetados com escalas livres usando tmap
mapa <- tm_shape(mapas_cortados_unico) +
  tm_borders(col = "grey") +
  tm_fill(col = "grey") +
  tm_facets(by = "estudo", free.scales = TRUE) +
  # Adicionar pontos aos mapas
  tm_shape(dados_gpkg) +
  tm_symbols(col = "red", size = 0.5) + # Personalize conforme necessario
  tm_facets(by = "estudo", free.scales = TRUE) + # Garantir que pontos correspondam aos mapas
  tm_scale_bar(position = c("right", "bottom"), text.size = 1, breaks = c(0, 5), color.light = "black") +
  # Adicionar camada de poligono
  tm_shape(hulls) +
  tm_polygons(col = "orange", alpha = 0.3, lwd = 1, lty = "dashed") +
  tm_facets(by = "estudo", free.scales = TRUE) +
   tm_credits(text = paste(unique(hulls$landscape_area_km2),"km²"),
             position = c("left", "top"), size = 1, col = "blue") +
   tm_layout(panel.label.size = 1.5,
          panel.label.height = 1.1)

# Salvar o grafico no formato desejado (16cm por 18cm)
tmap_save(mapa, filename = "grafico_pontos_plantas.png", width = 16, height = 18, units = "cm")
