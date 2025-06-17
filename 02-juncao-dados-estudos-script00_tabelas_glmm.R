# No meu mestrado, foram utilizados dados de 35 estudos. A preparação desses dados 
# (cálculo das combinações e do índice alfa de diversidade de Fisher) foi realizada individualmente para cada estudo.
# Este script tem como objetivo consolidar esses dados em três conjuntos distintos:
# dados completos, dados contendo somente espécies especialistas em habitat florestal,
# e dados contendo somente espécies generalistas de habitat.

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(data.table)
library(readr)
library(fs)

# Dados completos

# Defina o diretorio raiz
diretorio_raiz <- "E:/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Inicializar um dataframe vazio para armazenar os dados
dados_unidos <- data.table()

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .csv que comecam com "data_" no diretorio atual
  arquivos_csv <- list.files(path = subdiretorio, pattern = "^d1_data_dez_mil_.*\\.csv$", full.names = TRUE)
  
  # Unir os arquivos .csv em um unico dataframe, se houver algum
  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      # Ler o arquivo .csv e unir ao dataframe existente
      dados_unidos <- rbind(dados_unidos, fread(arquivo_csv))
    }
  }
}

# Salva tabela
write_csv(dados_unidos, file="E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_completos.csv")

