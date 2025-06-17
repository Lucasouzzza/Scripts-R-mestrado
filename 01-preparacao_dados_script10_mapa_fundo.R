# Script destinado a realizar o recorte das paisagens de cada estudo com base nos fragmentos amostrados.
# São recortados mapas provenientes do projeto MapBiomas. Posteriormente, o recorte é reclassificado e reprojetado
# conforme os parâmetros utilizados no mestrado, unindo todos os usos da terra relacionados a florestas naturais em uma única categoria,
# enquanto as demais categorias são agrupadas em outra. A projeção adotada é a equivalente ao Albers.

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(sf)
library(raster)

#obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- gsub("/scripts_R_dez_mil", "", diretorio) #remove a ultima parte do diretorio
# Extrair a parte desejada do diretorio
diretorio_pai <- dirname(dirname(dirname(diretorio2)))

file_taxon<-paste0(diretorio2, "/taxon.txt")
taxon <- suppressWarnings(readLines(file_taxon))
file_estudo<-paste0(diretorio2, "/estudo.txt")
estudo <- suppressWarnings(readLines(file_estudo))
file_ano<-paste0(diretorio2, "/ano.txt")
ano <- suppressWarnings(readLines(file_ano))
estudo_titulo <- gsub(" ", "_", estudo)

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
# Adicionar "/2_fisher_grafico_dez_mil" ao final do caminho base
caminho_completo <- file.path(diretorio2, "00_poligonos_dez_mil")
# diretorio mapas
caminho_mapa <- file.path(diretorio_pai, "Mestrado/Mapas")
#diretorio para salvar
caminho_salvar <- file.path(diretorio2, "00_arquivos_map_biomas")

name_data <- "frag_taman_usado_dez_mil.gpkg"

file<-paste0(caminho_completo, "/",name_data)

# Ler dados dos GeoPackages
dados_gpkg <- st_read(file)
# Defina a projecao de destino desejada
nova_projecao <- "+proj=longlat +datum=WGS84 +no_defs"

# Reprojete o objeto sf
dados_reproj <- st_transform(dados_gpkg, crs = nova_projecao)

# Construa o caminho completo para o arquivo TIFF com base no ano
caminho_completo_mapa <- paste0(caminho_mapa,"/",ano, ".tif")

# Salvar arquivo finalizado
salvar_mapa <- paste0(caminho_salvar,"/","fundo_dez_mil.tif")

# Carregue o mapa TIFF
mapa <- raster(caminho_completo_mapa)

# Obtenha os limites do objeto 'dados_reproj'
bbox_dados_reproj <- st_bbox(dados_reproj)

# Extraia as coordenadas xmin, ymin, xmax, ymax
xmin <- bbox_dados_reproj["xmin"]
ymin <- bbox_dados_reproj["ymin"]
xmax <- bbox_dados_reproj["xmax"]
ymax <- bbox_dados_reproj["ymax"]

# Recorte o raster usando a extensao da area de interesse
mapa_recortado <- crop(mapa, extent(xmin, xmax, ymin, ymax))

# Crie uma matriz de reclassificacao para mapear as cores desejadas
matriz_reclassificacao <- matrix(c(2.5, 5.5, 1, 48.5, 49.5, 1, 8.5, 48, 2), ncol = 3, byrow = TRUE)

# Reclassifique o raster com base nas cores
mapa_reclassificado <- reclassify(mapa_recortado, matriz_reclassificacao)

# Substituir todos os valores iguais a 1 por NA (valor ausente)
mapa_final<-mapa_reclassificado[mapa_reclassificado != 1] <- NA

# Remova o ID=2
mapa_final <- mask(mapa_reclassificado, mapa_reclassificado == 1)

# Defina o sistema de coordenadas de destino conforme especificado
new_projection <- "+proj=aea +lat_1=10 +lat_2=-40 +lat_0=-25 +lon_0=-50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
raster_layer_reprojected <- projectRaster(mapa_final, crs = new_projection)

# Salvar
writeRaster(raster_layer_reprojected, salvar_mapa, format = "GTiff", overwrite = TRUE)
