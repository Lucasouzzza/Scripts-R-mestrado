# No meu mestrado foi utilizado nas analises o indice alfa de diversidade de Fisher obtido da abundancia das especies. O indice alfa tem a propriedade de ser
# pouco influenciado pelo tamanho da amostra amostra (Beck e Schwanghart, 2010; Condit et al., 1996,1998; Taylor et al., 1976). Para a utilizacao desse indice, as abundancias devem estar
# em contagem e em teoria, devem seguir a distribuicao log-series, embora seja considerado robusto em relacao as violacoes do pressuposto da distribuicao log-series das
# abundancias de especies (Magurran, 2004). Mesmo assim, testamos o ajuste das abundancias a distribuicao log-series em todos os fragmentos.

#Literatura citada: 
# Beck, J., Schwanghart, W., 2010. Comparing measures of species diversity from incomplete inventories: an update. Methods in Ecology and Evolution 1, 38–44.
# Condit, R., Hubbell, S.P., Lafrankie, J. V, Sukumar, R., Manokaran, N., Foster, R.B., Ashton, P.S., 1996. Species-area and species-individual relationships for tropical trees: a comparison of
# three 50-ha plots. Journal of Ecology 549–562.
# Condit, R., Foster, R.B., Hubbell, S.P., Sukumar, R., Leigh, E.G., Manokaran, N., de Lao, S.L., LaFrankie, J. V, Ashton, P.S., others, 1998. Assessing forest diversity on small plots: calibration
# using species-individual curves from 50-ha plots. Assessing forest diversity on small plots:calibration using species-individual curves from 50-ha plots. 247–268.
# Taylor, L.R., Kempton, R.A., Woiwod, I.P., 1976. Diversity statistics and the log-series model The Journal of Animal Ecology 255–272.
# Magurran, A.E., 2004. Measuring Biological Diversity. Oxford: Blackwell.

# Versao do R utilizada: 4.2.3.

# Pacotes utilizados
#library(readr)
#library(plyr)
library(dplyr)
#library(tidyr)
#library(purrr)
#library(vegan)
library(foreach)
#library(doParallel)
library(doFuture)
library(furrr)
library(sads)

# Obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

file_taxon<-paste0(diretorio2, "/taxon.txt")
taxon <- suppressWarnings(readLines(file_taxon))

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data<-file.path(diretorio2,"dados_tratados")

name<- nome_pasta_atual
name_data<- paste0(name, "_dez_mil.csv")
file<-paste0(caminho_completo_data, "/",name_data)

caminho_completo_data_tres_maiores<-file.path(diretorio2,"03_SL_tres_maiores_dez_mil")
# Para setar o diretorio onde os aruivos vao ser salvos     
caminho_pasta2 <- file.path(diretorio2, "04_tabelas_aic_dez_mil")

name_data1<- paste0("d1_data_dez_mil_01.csv")
name_data2<- paste0("d1_data_dez_mil_02.csv")
name_data3<- paste0("d1_data_dez_mil_03.csv")

file_01<-paste0(caminho_completo_data_tres_maiores, "/",name_data1)
file_02<-paste0(caminho_completo_data_tres_maiores, "/",name_data2)
file_03<-paste0(caminho_completo_data_tres_maiores, "/",name_data3)

tryCatch({

# Carrega os arquivos
frag_01 <- read.csv(file_01, header = TRUE, sep = ",")
frag_01 <- select(frag_01, frag_base, area)
frag_01 <- frag_01[1,]
frag_02 <- read.csv(file_02, header = TRUE, sep = ",")
frag_02 <- select(frag_02, frag_base, area)
frag_02 <- frag_02[1,]
frag_03 <- read.csv(file_03, header = TRUE, sep = ",")
frag_03 <- select(frag_03, frag_base, area)
frag_03 <- frag_03[1,]

}, error = function(e) {
})

# Listar todos os objetos do ambiente de trabalho
objetos <- ls()

# Identificar os data frames que comecam com "frag_"
frag_objs <- objetos[grep("^frag_0", objetos)]

# Obter uma lista dos data frames identificados
frag_df_list <- mget(frag_objs)

# Combinar os data frames em um unico data frame
# Ou utilize library(data.table) # se preferir utilizar a funcao rbindlist()
resultado <- bind_rows(frag_df_list)

rm(list=setdiff(ls(), c("diretorio2" ,"file", "name", "resultado", "caminho_pasta2", "taxon", "arquivos_script", "arquivo")))

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
 # Converte frag para dataframe
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
 
 # Lista para armazenar os valores
 results_model_comp <- list()
 
 # Loop para percorrer os diferentes valores de 'combi'
 for (k in seq_along(fragment.list)) {
   # Fazer algo com cada valor de 'combi'
   data_sub <- subset(fragment.list[[k]]) 
   # Remove os zeros
   data_sub <- subset(data_sub, total_abund != 0)
   data_2 <- data_sub$total_abund
   fit_with_error_handling <- function(data, distribution, ...) {
     tryCatch({
       if (distribution == "lnorm") {
         fit <- fitsad(data, sad = distribution, ...)
       } else {
         fit <- fitsad(data, sad = distribution)
       }
       return(fit)
     }, error = function(e) {
       cat("Erro ao ajustar a distribuição", distribution, ": ", conditionMessage(e), "\n")
       return(NULL)
     })
   }
   
   tryCatch({
     # Ajuste de cada distribuicao com tratamento de erro
     fit_results <- list(
       ls = fit_with_error_handling(data_2, "ls"),
       ln = fit_with_error_handling(data_2, "lnorm", trunc = 0.5),
       pln = fit_with_error_handling(data_2, "poilog"),
       nbi = fit_with_error_handling(data_2, "nbinom"),
       geo = fit_with_error_handling(data_2, "geom"),
       bs = fit_with_error_handling(data_2, "bs")
     )
     
     # Construcao do objeto result apenas com as distribuicoes bem-sucedidas
     valid_results <- Filter(Negate(is.null), fit_results)
     result <- AICtab(valid_results, weights = TRUE)
     # Transforma os nomes das linhas em coluna
     result <- as.data.frame(result)
     # Extrai Fisher's alpha
     coefficientes <- coef(fit_results$ls)
     N_ind <- coefficientes[1]
     Fishers_alpha <- coefficientes[2]
     # Adiciona a tabela
     result$N <- NA
     result$Fish_alpha <- NA
     result["ls", 4] <- N_ind
     result["ls", 5] <- Fishers_alpha
     # Salva na lista com o nome da interacao
     results_model_comp[[names(fragment.list)[k]]] <- result
     
   }, error = function(e) {
   })
   
 }
 
 # Create a matrix for storing AIC values 
 N_results<-length(results_model_comp)
 AICs_ls<-matrix(data=0, nrow=N_results, ncol=5) 
 colnames(AICs_ls)<-c("frag_base", "dAIC", "AICweigth", "N_ind", "Fishers_alpha")
 
 nomes_dataframes <- names(results_model_comp)
 
 #insert site_ids
 AICs_ls[,1]<-nomes_dataframes
 
 #loop over frags 
 for (i in 1:N_results){
   
   #extract dataframe from results model comparison  
   res<-as.data.frame(results_model_comp[i])
   # extract values in table
   dAIC<-res["ls",1]
   AICweigth<-res["ls",3]
   N_ind<-res["ls",4]
   Fishers_alpha<-res["ls",5]
   #save values in table
   AICs_ls[i,2]<-dAIC
   AICs_ls[i,3]<-AICweigth
   AICs_ls[i,4]<-N_ind
   AICs_ls[i,5]<-Fishers_alpha
   
 }
 #convert columns 2 and 3 in numeric
 AICs_ls<-as.data.frame(AICs_ls)
 AICs_ls[,2]<-as.numeric(AICs_ls[,2])
 AICs_ls[,3]<-as.numeric(AICs_ls[,3])
 AICs_ls[,5]<-as.numeric(AICs_ls[,5])
 
 #ad taxon column
 AICs_ls["taxon"]<-taxon
 #ad column classification
 AICs_ls["classification"]<-"dez_mil"
 AICs_ls["study_id"]<-name
 
 # Remove o prefixo "fragment_" dos valores na coluna frag_base do data frame AICs_ls
 AICs_ls$frag_base <- sub("^fragment_", "", AICs_ls$frag_base)
 
 # Filtrar as linhas de frag$frag_base que correspondem aos valores em resultado$frag_base
 AICs_ls <- AICs_ls %>% 
   filter(frag_base %in% resultado$frag_base)
 
 #adicione a coluna com as areas
 # Adicionando a coluna "area" correspondente ao dataframe AICs_ls
 AICs_ls <- AICs_ls %>%
   mutate(frag_base_area = resultado$area[match(frag_base, resultado$frag_base)])
  
 #salvar em .csv
 write.csv(AICs_ls, file.path(caminho_pasta2, "T00_Primeiro_niveil_AIC_especies_dez_mil.csv"), row.names = FALSE)
 