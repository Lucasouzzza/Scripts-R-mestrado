# Código para o cálculo do índice alfa de diversidade de Fisher em fragmentos florestais.

# Versao do R utilizada: 4.2.3

# Pacotes utilizados
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(vegan)
library(foreach)
library(doParallel)
library(doFuture)
library(furrr)

# Obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data<-file.path(diretorio2,"dados_tratados")

# Para setar o diretorio onde os aruivos vao ser salvos     
caminho_pasta_save <- file.path(diretorio2, "02_fisher_dez_mil")

name<- nome_pasta_atual
name_data<- paste0(name, "_dez_mil.csv")
file<-paste0(caminho_completo_data, "/",name_data)

rm(list=setdiff(ls(), c("diretorio2" ,"file", "name", "caminho_pasta_save", "arquivos_script", "arquivo")))

 # Importar tabela origem de dados e gerar data frame
 frag <- read.csv(file, header = TRUE, sep = ",")
 # Selecionar colunas e ordem de interesse
 frag <- select(frag, area_ha, patch, species, total_abund) 
 # Soma as especies repetidas
 frag <- frag %>% group_by(area_ha, patch, species) %>% summarise(total_abund = sum(total_abund), .groups = 'drop')
 # Remover os fragmentos com apenas especies unicas
 frag <- subset(frag,duplicated(patch) | duplicated(patch, fromLast=TRUE)) 
 # Tira os fragmentos com singlons
 frag <- frag  %>% group_by(patch) %>%  filter(!all(total_abund == 1))
 # Convete frag para dataframe
 frag <- as.data.frame(frag) 
 # Ordenar em ordem crescente a coluna patch
 frag <- frag[order(frag$patch),]
 # Selecionar colunas do data frame gerado
 fragselect <-(distinct(frag,area_ha, patch))
 # Muda , para . dos valores as areas
 fragselect$area_ha <- as.numeric(gsub(",", "\\.", fragselect$area_ha)) 
 # Cria um dataframe com todas as especies presentes em frag
 spe <-(distinct(frag, species)) 
 # Adiciona uma coluna com abundancia 0 para essas especies
 spe["total_abund"] <-rep(0, ) 
 
 # Definfir diferentes frags-base
 frag_base_different<-unique(fragselect[,"patch"]) 
 
 # Backend paralelo
 # Registra o backend paralelo
 registerDoFuture()
 # Calcula o numero de nucleos disponiveis, menos 1
 num_workers <- parallel::detectCores() - 1
 # Configura o plano paralelo com o numero de workers
 plan(multisession, workers = num_workers)

 # Loop para criar data frames utilizando Frag_base_diferente<-unique, remove as colunas desnecessarias e depois transforma em uma lista
 fragment.list<- foreach(a=1:length(frag_base_different), .combine = 'c') %dopar% {
   assign(paste0("fragment_",frag_base_different[a]), 
          frag[frag$patch==frag_base_different[a],][,-c(1,2)])
   fragment.list<- mget(ls(pattern = "fragment_.*"))
 }
 
 # Adiciona as especies que estao faltando. referencia: https://stackoverflow.com/questions/69758291/add-rows-to-list-of-dataframes-from-another-dataframe
 fragment.list <- imap(fragment.list, ~ .x %>% bind_rows(spe))
 # Faz ajustes na lista: verifica se tem especie repetida. referencia: https://stackoverflow.com/questions/57288991/how-to-group-and-summarise-each-data-frame-in-a-list-of-data-frames
 fragment.list<-future_map(fragment.list, ~.x %>% group_by(species) %>% summarize(total_abund = sum(total_abund)))
 
 # Formata para o formato aceito pelo pacote vegan
 fragment.list<-future_map(fragment.list, ~.x %>% spread(species, total_abund))
 
 # Encerra o backend paralelo
 plan(sequential)
 
 # Lapply para aplicar a funcao fisher.alpha em toda a lista
 fisher.list <- lapply(fragment.list, function(x)fisher.alpha(x)) 
 
 # Remover objetos que nao vao ser mais usados
 rm(fragment.list)
 
 # Criar tabela para salvar valores de Fisher
 results_fisher <- ldply (fisher.list, data.frame)
 colnames(results_fisher) <- c("fragments", "result_fisher") 
 # Remover possiveis combinacoes repetidas
 results_fisher <-unique(results_fisher)
 
 results_fisher$tam_frag_base1 <- NA  # Adicionar uma nova coluna "area_ha" ao dataframe "results_fisher" e preencher com NA
 
 for (i in 1:nrow(results_fisher)) {  # Iterar pelas linhas do dataframe "results_fisher"
   fragment_number <- as.numeric(gsub("fragment_", "", results_fisher$fragments[i]))  # Extrair o numero da linha atual no dataframe "results_fisher" removendo o prefixo "fragment_" usando a funcao gsub e convertendo para numero
   
   # Verificar se existe um valor correspondente na coluna "area_ha" do dataframe "frag"
   matching_rows <- which(frag$patch == fragment_number)  # Encontrar os indices das linhas em que ha uma correspondencia entre o valor de fragment_number e a coluna "patch" do dataframe "frag"
   
   if (length(matching_rows) > 0) {  # Verificar se ha pelo menos uma correspondencia encontrada
     area_ha_value <- frag$area_ha[matching_rows[1]]  # Obter o valor correspondente da coluna "area_ha" do dataframe "frag" usando o primeiro indice de correspondencia encontrado
     results_fisher$tam_frag_base1[i] <- area_ha_value  # Atribuir o valor correspondente a nova coluna "area_ha" no dataframe "results_fisher"
   } else {
     results_fisher$tam_frag_base1[i] <- NA  # Atribuir NA para indicar que nenhum valor correspondente foi encontrado
   }
 }

 # Salvar em .csv
 write.csv(results_fisher, file.path(caminho_pasta_save, "f01_results_fisher_dez_mil.csv"), row.names = FALSE)
 
