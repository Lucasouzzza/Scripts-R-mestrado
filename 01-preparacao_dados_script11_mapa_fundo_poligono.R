# Script destinado para conversao dos mapas.tif em objetos sf e posteriormente, salva-los na pasta apropriada. 

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(stars)

#obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- gsub("/scripts_R_dez_mil", "", diretorio) #remove a ultima parte do diretorio
# Extrair a parte desejada do diretorio
diretorio_pai <- dirname(dirname(dirname(diretorio2)))

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
# Adicionar "/2_fisher_grafico_dez_mil" ao final do caminho base
caminho_completo <- file.path(diretorio2, "00_arquivos_map_biomas")
# diretorio mapas
caminho_mapa <- file.path(diretorio_pai, "EStudos/Mapas")
#diretorio para salvar
caminho_salvar <- file.path("E:/Mestrado/Tabelas_analises/05_mapas/fundos_dez_mil")

name_data <- "fundo_dez_mil.tif"
file<-paste0(caminho_completo, "/",name_data)

# Ler dados dos GeoPackages
mapa_tif <- read_stars(file)

# Converta o objeto stars para um objeto sf (Simple Features) com parametros fornecidos
sf_polygon <- stars:::st_as_sf.stars(mapa_tif, point = FALSE, merge = TRUE, connect8 = TRUE)

# Salvar arquivo finalizado
salvar_mapa <- paste0(caminho_salvar,"/",nome_pasta_atual,"_dez_mil.gpkg")

# Salvar
st_write(sf_polygon, salvar_mapa, driver = "GPKG")
