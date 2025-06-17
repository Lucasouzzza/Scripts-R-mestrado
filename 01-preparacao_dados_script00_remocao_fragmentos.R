# Script para remover todos os fragmentos florestais com área superior a 10.000 hectares,
# que não foram utilizados nas análises por apresentarem extensão espacial muito grande,
# tornando-se inadequados para as análises do tipo SLOSS.

# Versao do R utilizada: 4.2.3

# Pacotes utilizados
library(dplyr)
library(readr)

# Obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data<-file.path(diretorio2,"dados_brutos")
caminho_completo_save<-file.path(diretorio2,"dados_tratados")

name<- nome_pasta_atual
name_data<- paste0(name, ".csv")
name_data_save<- paste0(name, "_dez_mil.csv")
file<-paste0(caminho_completo_data, "/",name_data)

rm(list=setdiff(ls(), c("file","name_data_save" ,"caminho_completo_save", "arquivos_script", "arquivo")))

# Importar tabela origem de dados e gerar data frame
data <- read.csv(file, header = TRUE, sep = ",")

# Remover todas as areas maiores que 10000
data <- subset(data,area_ha <= 10000)

# Salvar em .csv
write.csv(data, file.path(caminho_completo_save, name_data_save), row.names = FALSE)
