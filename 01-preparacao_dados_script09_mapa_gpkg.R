# Este script remove os fragemntos nao utilizados nas analises do arquivo gpkg para
# posteriormente ser utilizado na preparacao dos mapas

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(sf)
library(dplyr)

#obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- gsub("/scripts_R_dez_mil", "", diretorio) #remove a ultima parte do diretorio

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data<-file.path(diretorio2,"00_qgis")
caminho_completo_patchs<-file.path(diretorio2,"dados_tratados")
caminho_completo_save<-file.path(diretorio2,"00_poligonos_dez_mil")

name<- nome_pasta_atual
name_data<- paste0(name, "_selec.gpkg")
name_patch<- "fragmentos_usados_dez_mil.csv"
name_data_save<- "frag_taman_usado_dez_mil.gpkg"
file<-paste0(caminho_completo_data, "/",name_data)
patch<-paste0(caminho_completo_patchs, "/",name_patch)

file_taxon<-paste0(diretorio2, "/taxon.txt")
taxon <- suppressWarnings(readLines(file_taxon))

rm(list=setdiff(ls(), c("file","name_data_save" ,"caminho_completo_save", "nome_pasta_atual", "patch", "arquivos_script", "arquivo")))

#importar tabela origem de dados e gerar data frame
data <- st_read(file)

#importar data frame de comparacao
patch_data <-read.csv(patch)

#data frame com as linhas removidas
data2 <- subset(data, patch %in% patch_data$patch)
# Adiciona colunas com o id e taxon
data2$study_id<-nome_pasta_atual
data2$taxon<-taxon 

#seleciona colunas de interesse
data2 <- select(data2, study_id, patch, geom)

#salvar 
st_write(data2, file.path(caminho_completo_save, name_data_save), driver = "GPKG")