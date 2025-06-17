# Script destinado a unir todos os objetos 'frag_taman_usado_dez_mil' em um único objeto.

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(data.table)
library(readr)
library(sf)
library(fs)

#geral
# Defina o diretorio raiz
diretorio_raiz <- "C:/Users/lucas/Documents/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
#subdiretorios <- list.dirs(path = diretorio_raiz, recursive = TRUE)
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Criar um objeto vazio para armazenar os dados
dados_unidos <- NULL

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .gpkg no diretorio atual
  arquivos_gpkg <- list.files(path = subdiretorio, pattern = "^frag_taman_usado_dez_mil.*\\.gpkg$", full.names = TRUE)
  
  # Unir os arquivos .gpkg em um unico objeto sf, se houver algum
  if (length(arquivos_gpkg) > 0) {
    for (arquivo_gpkg in arquivos_gpkg) {
      # Ler o arquivo .gpkg
      dados_gpkg <- st_read(arquivo_gpkg)
      # Unir os dados ao objeto dados_unidos
      if (is.null(dados_unidos)) {
        dados_unidos <- dados_gpkg
      } else {
        dados_unidos <- rbind(dados_unidos, dados_gpkg)
      }
    }
  }
}

#cria uma coluna com ids numericos
dados_unidos$ID <- sprintf("%02d", match(dados_unidos$study_id, unique(dados_unidos$study_id)))

# Salvar os dados unidos como um arquivo GeoPackage
caminho_saida <- "C:/Users/lucas/Documents/Mestrado/Tabelas_analises/05_mapas/dados_fragmentos_dez_mil.gpkg"
st_write(dados_unidos, caminho_saida)
