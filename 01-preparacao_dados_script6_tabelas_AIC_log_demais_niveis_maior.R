# No meu mestrado foi utilizado nas analises o indice alfa de diversidade de Fisher obtido da abundancia das especies. O indice alfa tem a propriedade de ser
# pouco influenciado pelo tamanho da amostra amostra (Beck e Schwanghart, 2010; Condit et al., 1996,1998; Taylor et al., 1976). Para a utilizacao desse indice, as abundancias devem estar
# em contagem e em teoria, devem seguir a distribuicao log-series, embora seja considerado robusto em relacao as violacoes do pressuposto da distribuicao log-series das
# abundancias de especies (Magurran, 2004). Mesmo assim, testamos o ajuste das abundancias a distribuicao log-series em todas as combinacoes correspondentes ao maior fragmento.

#Literatura citada: 
# Beck, J., Schwanghart, W., 2010. Comparing measures of species diversity from incomplete inventories: an update. Methods in Ecology and Evolution 1, 38–44.
# Condit, R., Hubbell, S.P., Lafrankie, J. V, Sukumar, R., Manokaran, N., Foster, R.B., Ashton, P.S., 1996. Species-area and species-individual relationships for tropical trees: a comparison of
# three 50-ha plots. Journal of Ecology 549–562.
# Condit, R., Foster, R.B., Hubbell, S.P., Sukumar, R., Leigh, E.G., Manokaran, N., de Lao, S.L., LaFrankie, J. V, Ashton, P.S., others, 1998. Assessing forest diversity on small plots: calibration
# using species-individual curves from 50-ha plots. Assessing forest diversity on small plots:calibration using species-individual curves from 50-ha plots. 247–268.
# Taylor, L.R., Kempton, R.A., Woiwod, I.P., 1976. Diversity statistics and the log-series model The Journal of Animal Ecology 255–272.
# Magurran, A.E., 2004. Measuring Biological Diversity. Oxford: Blackwell.

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

diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

file_taxon<-paste0(diretorio2, "/taxon.txt")
taxon <- suppressWarnings(readLines(file_taxon))

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
caminho_completo_data1<-file.path(diretorio2,"02_fisher_dez_mil")
caminho_completo_data2<-file.path(diretorio2,"dados_tratados")
caminho_completo_data_tres_maiores<-file.path(diretorio2,"03_SL_tres_maiores_dez_mil")

name<- nome_pasta_atual
name_data<- paste0(name, "_dez_mil.csv") 
name_data2<- paste0("d1_data_dez_mil_01.csv") 
file<-paste0(caminho_completo_data2, "/",name_data)

# caminho_completo_data_tres_maiores<-file.path(diretorio2,"3_tres_maiores_dez_mil")
# Para setar o diretorio onde os aruivos vao ser salvos     
caminho_pasta2 <- file.path(diretorio2, "04_tabelas_aic_dez_mil")

# Obter uma lista de todos os arquivos com sufixo ".csv"
arquivos_csv <- list.files(path = caminho_completo_data1, pattern = "\\.csv$", full.names = TRUE)

# Loop para carregar e atribuir cada arquivo ao ambiente global
for (arquivo in arquivos_csv) {
  # Obter o nome do arquivo sem o caminho completo e a extensao
  nome_objeto <- sub("\\.csv$", "", basename(arquivo))
  # Ler o arquivo .csv e atribui-lo a um objeto no ambiente global
  assign(nome_objeto, read.csv(arquivo))
}
 
# Inicialize uma lista para armazenar os data frames
lista_de_dataframes <- list()

# Loop sobre os objetos no ambiente global
for (objeto_nome in ls()) {
  # Verifique se o objeto e um data frame e se nao comeca com "f01"
  if (is.data.frame(get(objeto_nome)) && !startsWith(objeto_nome, "f01")) {
    # Adicione o data frame a lista
    lista_de_dataframes[[objeto_nome]] <- get(objeto_nome)
  }
}

# Funcao para selecionar colunas que comecam com "f" em um data frame
selecionar_colunas_f <- function(df) {
  # Obter nomes das colunas que comecam com "f"
  colunas_f <- grep("^f", names(df), value = TRUE)
  # Retornar o data frame com apenas as colunas que comecam com "f"
  return(df[colunas_f])
}

# Aplicar a funcao em todos os data frames da lista
lista_de_dataframes_f <- lapply(lista_de_dataframes, selecionar_colunas_f)

file_00<-paste0(caminho_completo_data_tres_maiores, "/",name_data2)

# Tenta ler o arquivo CSV
resultado <- tryCatch(
  read.csv(file_00, header = TRUE, sep = ","),
  error = function(e) NULL
)

# Verifica se o resultado e nulo
if (is.null(resultado)) {
  # Executa o codigo para limpar o ambiente de trabalho, excluindo todas as variaveis, exceto a funcao `executar_scripts`
  rm(list=setdiff(ls(), c("arquivos_script", "arquivo")))
}
 
  resultado <- select(resultado, frag_base, area)
  # Deixa so observacaoes unicas
  resultado <- resultado[1,]

# Filtrar cada data frame dentro da lista
lista_de_dataframes_f <- lapply(lista_de_dataframes_f, function(df) {
  filter(df, frag_base %in% resultado$frag_base)
})

# Funcao para calcular o tamanho da amostra
calculate_sample_size <- function(population_size, confidence_level, margin_of_error) {
  # Calcula o valor critico da distribuicao normal padrao
  z <- qnorm(1 - (1 - confidence_level) / 2)
  
  # Calcula a proporcao da populacao com a caracteristica desejada
  p <- 0.5 # Assumindo 50% se nao ha estimativa previa
  
  # Calcula o tamanho da amostra
  sample_size <- (p * (1 - p) * z^2 * population_size) / (margin_of_error^2 * (population_size - 1) + p * (1 - p) * z^2)
  
  return(sample_size)
}

# Defina o nivel de confianca (por exemplo, 95%)
confidence <- 0.95

# Defina a margem de erro (por exemplo, 5%)
margin <- 0.05

# Lista para armazenar os tamanhos de amostra calculados para cada dataframe
sample_sizes <- numeric(length(lista_de_dataframes_f))

# Itera sobre os elementos da lista
for (i in seq_along(lista_de_dataframes_f)) {
  # Tamanho da populacao e o numero de linhas do dataframe atual
  population <- nrow(lista_de_dataframes_f[[i]])
  
  # Calcula o tamanho da amostra para o dataframe atual
  sample_sizes[i] <- calculate_sample_size(population, confidence, margin)
  
  # Verifica se o tamanho da amostra e maior que 100
  if (sample_sizes[i] > 100) {
    max_linhas <- sample_sizes[i]
    cat("Tamanho da amostra calculado para", names(lista_de_dataframes_f)[i], ":", round(sample_sizes[i]), "\n")
  } else {
    max_linhas <- population
    cat("O tamanho da amostra calculado para", names(lista_de_dataframes_f)[i], "é menor ou igual a 100, o que não atende ao critério mínimo.\n")
  }
  
  # Seleciona aleatoriamente ate max_linhas linhas do dataframe atual
  num_linhas <- min(nrow(lista_de_dataframes_f[[i]]), max_linhas)
  if (num_linhas > 0) {
    lista_de_dataframes_f[[i]] <- lista_de_dataframes_f[[i]][sample(nrow(lista_de_dataframes_f[[i]]), num_linhas), , drop = FALSE]
  }
}

# Importar tabela origem de dados e gerar data frame
frag <- read.csv(file, header = TRUE, sep = ",")

# Selecionar colunas de interesse
frag <- select(frag, patch, species, total_abund)

# Obter os nomes dos data frames na lista
nomes_dataframes <- names(lista_de_dataframes_f)

# Funcao para criar os data frames combinados e armazena-los em uma lista
criar_comb_data_frames <- function(nome_df, frag) {
  df <- lista_de_dataframes_f[[nome_df]]  # Obtem o data frame pelo nome
  comb_data_frames <- list()  # Inicializa uma lista para armazenar os data frames combinados
  
  # Itera sobre as linhas do data frame
  for (i in 1:nrow(df)) {
    
    # Valor_patch <- df[i, "frag_base"]
    # Nome do data frame combinado
    nome_data_frame_combinado <- paste0("combi_", i, "_", nome_df)
    
    # Obtem os valores das colunas "fragment_" para a linha atual
    valores_fragment <- unlist(df[i, grep("^fragment_", names(df))])
    
    # Filtra as linhas do data frame de acordo com os valores de frag$patch
    linhas_filtradas <- frag[frag$patch %in% valores_fragment, ]
    
    # Adiciona o data frame resultante a lista de data frames combinados
    comb_data_frames[[nome_data_frame_combinado]] <- linhas_filtradas
  }
  
  return(comb_data_frames)
}

# Lista para armazenar os data frames combinados
lista_comb_data_frames <- list()

# Aplica a funcao a cada nome de data frame
for (nome_df in nomes_dataframes) {
  # Cria os data frames combinados e armazena-os na lista
  lista_comb_data_frames <- c(lista_comb_data_frames, criar_comb_data_frames(nome_df, frag))
}

# Itera sobre os elementos da lista_comb_data_frames e renomeia os data frames
for (nome_df in names(lista_comb_data_frames)) {
  # Remove "results_fisher_dez_mil" do nome_df
  novo_nome_df <- gsub("_results_fisher_dez_mil", "", nome_df)
  
  # Renomeia o data frame na lista
  lista_comb_data_frames[[novo_nome_df]] <- lista_comb_data_frames[[nome_df]]
  # Remove o data frame original da lista
  lista_comb_data_frames[[nome_df]] <- NULL
}

# Backend paralelo
# Registra o backend paralelo
registerDoFuture()
# Calcula o numero de nucleos disponiveis, menos 1
num_workers <- parallel::detectCores() - 1
# Configura o plano paralelo com o numero de workers
plan(multisession, workers = num_workers)

lista_comb_data_frames<-future_map(lista_comb_data_frames, ~.x %>% 
                               group_by(species) %>% 
                               summarize(total_abund = sum(total_abund)))

# Ecerra backend paralelo
plan(sequential)

# Funcao para transformar os valores da coluna "total_abund" em vetor
transformar_em_vetor <- function(df) {
  # Seleciona apenas a coluna "total_abund" e transforma em vetor
  vetor <- df$total_abund
  return(vetor)
}

# Lista para armazenar os vetores resultantes
vetores_combinados <- list()

# Aplica a funcao a cada elemento da lista de data frames combinados
for (nome_df in names(lista_comb_data_frames)) {
  df <- lista_comb_data_frames[[nome_df]]
  # Remove a coluna "species" de cada data frame
  df <- df[, !(names(df) %in% "species"), drop = FALSE]
  vetor <- transformar_em_vetor(df)
  vetores_combinados[[nome_df]] <- vetor
}

# Remove vetores vazios da lista
vetores_combinados <- Filter(function(x) !is.null(x) && length(x) > 0, vetores_combinados)

rm(list=setdiff(ls(), c("taxon", "caminho_pasta2", "vetores_combinados", "name","resultado", "arquivos_script", "arquivo")))

# Definir a funcao fit_with_error_handling
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

fit_distributions_foreach <- function(vetores_combinados) {
  # Definir a funcao fit_distributions
  fit_distributions <- function(data_sub) {
    fit_results <- list(
      ls = fit_with_error_handling(data_sub, "ls"),
      ln = fit_with_error_handling(data_sub, "lnorm", trunc = 0.5),
      pln = fit_with_error_handling(data_sub, "poilog"),
      nbi = fit_with_error_handling(data_sub, "nbinom"),
      geo = fit_with_error_handling(data_sub, "geom"),
      bs = fit_with_error_handling(data_sub, "bs")
    )
    valid_results <- Filter(Negate(is.null), fit_results)
    return(valid_results)
  }
  
  # Backend paralelo
  # Registra o backend paralelo
  registerDoFuture()
  # Calcula o numero de nucleos disponiveis, menos 1
  num_workers <- parallel::detectCores() - 1
  # Configura o plano paralelo com o numero de workers
  plan(multisession, workers = num_workers)
  
  # Executar o loop foreach
  results_model_comp <- foreach(data_sub = vetores_combinados) %dopar% {
    valid_results <- fit_distributions(data_sub)
    return(list(valid_results))
  }
  
  # Combinar os resultados
  results_model_comp <- unlist(results_model_comp, recursive = FALSE)
  names(results_model_comp) <- names(vetores_combinados)
  
  return(results_model_comp)
}

# Chamada da funcao fit_distributions_foreach
results_model_comp <- fit_distributions_foreach(vetores_combinados)

# Encerra backend paralelo
plan(sequential)

 processed_results <- list()
 
 for (name_df in names(results_model_comp)) {
   result <- results_model_comp[[name_df]]
   result2 <- result
   result <- AICtab(result, weights = TRUE)
   result <- as.data.frame(result)
     fit_results <- result[["ls"]]
     coefficientes <- coef(result2$ls)
     N_ind <- coefficientes[1]
     Fishers_alpha <- coefficientes[2]
     result$N <- NA
     result$Fish_alpha <- NA
     tryCatch({
     result["ls", 4] <- N_ind
     result["ls", 5] <- Fishers_alpha
     }, error = function(e) {
       # Lidar com o erro
       warning("Ocorreu um erro ao atribuir valores a result.")
     })
   # Adicionar uma verificacao para o quadro de dados de resultado vazio
   if (length(result) == 0) {
     warning(paste("Quadro de dados de resultado vazio para", name_df))
     next  # Pular para a proxima iteracao
   }
   
   # Construir new_name corretamente
   new_name <- name_df
   # Atribuir o resultado a processed_results
   processed_results[[new_name]] <- result
 }
 
 # Create a matrix for storing AIC values 
 N_results<-length(processed_results)
 AICs_ls<-matrix(data=0, nrow=N_results, ncol=5) 
 colnames(AICs_ls)<-c("id", "dAIC", "AICweigth", "N_ind", "Fishers_alpha")
 
 nomes_dataframes <- names(processed_results)
 
 # Insert site_ids
 AICs_ls[,1]<-nomes_dataframes
 
 #loop over frags 
 for (i in 1:N_results){ 
   
   #extract dataframe from results model comparison  
   res<-processed_results[[i]]
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
 AICs_ls["classification"]<-"dez_mil_maior" 
 AICs_ls["study_id"]<-name
 AICs_ls["frag_base"]<-resultado$frag_base
 AICs_ls["frag_base_area"]<-resultado$area
 
 #salvar em .csv
 write.csv(AICs_ls, file.path(caminho_pasta2, "T01_demais_niveis_AIC_especies_dez_mil.csv"), row.names = FALSE) 
 