# Script para remover todos os fragmentos florestais com área superior a 10.000 hectares,
# os quais não foram utilizados nas análises por apresentarem extensão espacial excessiva,
# tornando-se inadequados para a abordagem SLOSS adotada.
# 
# Além disso, alguns estudos continham mais de 23 fragmentos amostrados, o que inviabilizou
# a aplicação da metodologia utilizada nas análises. Nesses casos, foi realizada uma reamostragem
# dos fragmentos.

# Versao do R utilizada: 4.2.3

# Pacotes utilizados
library(dplyr)
library(readr)

#obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data<-file.path(diretorio2,"dados_brutos")
caminho_completo_data_save<-file.path(diretorio2,"dados_tratados")

name<- nome_pasta_atual
name_data<- paste0(name, ".csv")
name_data_save<- paste0(name, "_dez_mil.csv")
file<-paste0(caminho_completo_data, "/",name_data)

rm(list=setdiff(ls(), c("file","name_data_save" ,"caminho_completo_data", "caminho_completo_data_save", "arquivos_script", "arquivo")))

#importar tabela origem de dados e gerar data frame
data <- read.csv(file, header = TRUE, sep = ",")

#remover todas as areas maiores que 10000
data <- subset(data,area_ha <= 10000)

# subsampling

# Defina a semente para tornar os resultados reproduziveis
set.seed(127)

# Funcao para amostragem em grupos
sample_n_groups = function(grouped_df, size, replace = FALSE, weight=NULL) {
  grp_var <- grouped_df %>% 
    groups %>%
    unlist %>% 
    as.character
  random_grp <- grouped_df %>% 
    summarise() %>% 
    sample_n(size, replace, weight) 
  grouped_df %>% 
    right_join(random_grp, by=grp_var) %>% 
    group_by(across(all_of(grp_var)))
}

 #faz a reamostragem
  frag_ream <- data %>% group_by(patch) %>% sample_n_groups(23)

  #salvar em .csv
  write.csv(frag_ream, file.path(caminho_completo_data_save, name_data_save), row.names = FALSE)
