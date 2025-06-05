# Este script tem o objetivo de selecionar as combinacoes SLOSS somente dos tres maiores fragmentos florestais de cada estudo

# Versao do R utilizada: 4.2.3

# Pacotes utilizados
library(dplyr)
library(stringr)

# Obtem diretorio atual do script
diretorio <- getwd()
diretorio2 <- dirname(diretorio) # Remove a ultima parte do diretorio

file_taxon<-paste0(diretorio2, "/taxon.txt")
taxon <- suppressWarnings(readLines(file_taxon))

# Extrair o nome da pasta usando a funcao basename()
nome_pasta_atual <- basename(diretorio2)
# Adicionar "/2_fisher_grafico_dez_mil" ao final do caminho base
caminho_completo <- file.path(diretorio2, "02_fisher_dez_mil")

# Para setar o diretorio onde os aruivos vao ser salvos     
caminho_pasta <- file.path(diretorio2, "03_SL_tres_maiores_dez_mil")

# Variaveis usadas no Script
name<- nome_pasta_atual

# Obter uma lista de todos os arquivos com sufixo ".csv"
arquivos_csv <- list.files(path = caminho_completo, pattern = "\\.csv$", full.names = TRUE)

# Loop para carregar e atribuir cada arquivo ao ambiente global
for (arquivo in arquivos_csv) {
  # Obter o nome do arquivo sem o caminho completo e a extensao
  nome_objeto <- sub("\\.csv$", "", basename(arquivo))
  # Ler o arquivo .csv e atribui-lo a um objeto no ambiente global
  assign(nome_objeto, read.csv(arquivo))
}

# Remove o prefixo fragment_ do data frame f01_results_fisher
f01_results_fisher_dez_mil$fragments <- gsub("fragment_", "", f01_results_fisher_dez_mil$fragments)

# Renomeia a coluna fragments do data frame f01_results_fisher
colnames(f01_results_fisher_dez_mil)[colnames(f01_results_fisher_dez_mil) == "fragments"] <- "frag_base"

# Renomeia a coluna fragments do data frame f01_results_fisher
colnames(f01_results_fisher_dez_mil)[colnames(f01_results_fisher_dez_mil) == "tam_frag_base1"] <- "area"

# Mesclar os dataframes em uma lista
fragmentation.list <- mget(ls(pattern = "_results_fisher_dez_mil*")) 

# Funcao para renomear a coluna "sum_areas" para "areas" em um data frame
rename_column <- function(df) {
  if ("sum_areas" %in% names(df)) {
    df$area <- df$sum_areas
    df$sum_areas <- NULL
  }
  return(df)
}

# Aplicar a funcao a cada data frame na lista
fragmentation.list <- lapply(fragmentation.list, rename_column)

# Remove o data frame f01_results_fisher da lista
fragmentation_filtred.list = fragmentation.list[names(fragmentation.list) != 
                                                  "f01_results_fisher_dez_mil"]

# Primeiro, crie um vetor com todos os valores unicos da coluna "frag_base" presentes nos data frames da lista "fragmentation.list"
unique_frag_base_list <- unique(unlist(lapply(fragmentation_filtred.list, function(df) df$frag_base)))

# Em seguida, use a funcao `filter` do pacote "dplyr" para manter apenas as linhas com valores de "frag_base" presentes no vetor "unique_frag_base_list"
f01_results_fisher_dez_mil_filtered <- f01_results_fisher_dez_mil %>%
  filter(frag_base %in% unique_frag_base_list)

# Ordenar o data frame pela coluna "area" em ordem decrescente
f01_results_fisher_dez_mil_filtered <- f01_results_fisher_dez_mil_filtered[order(f01_results_fisher_dez_mil_filtered$area, decreasing = TRUE), ]

# Selecionar as tres primeiras linhas
f01_results_fisher_dez_mil_filtered <- f01_results_fisher_dez_mil_filtered[1:3, ]

# Criar coluna "fragmentation_name" com o nome de cada dataframe
fragmentation.list <- Map(cbind, fragmentation.list, fragmentation_name = 
                            names(fragmentation.list))

# Cria um vetor com a lista dos fragmentos 
fragments <- f01_results_fisher_dez_mil_filtered$frag_base

rm(list=setdiff(ls(), c("diretorio2","name","taxon","fragmentation.list","fragments", "caminho_pasta", "arquivos_script", "arquivo")))

### Removendo fishers repetidos
# Loop para percorrer cada data frame na lista e remove todas as combinacoes
#de frag base diferentes das tres selecionadas
for (i in 1:length(fragmentation.list)) {
  df1 <- fragmentation.list[[i]]  # Obtem o data frame atual
  # Remove as linhas com valores diferentes de "fragments" em "frag_base"
  df1 <- df1[df1$frag_base %in% fragments, ]
  fragmentation.list[[i]] <- df1  # Substitui o data frame atualizado na lista
}

# Selecionar colunas de interesse
dataframe.list <- lapply(fragmentation.list, function(x) x%>% 
                           select(frag_base, fragmentation_name, area, result_fisher))

# Lista para armazenar os data frames separados
separated_dataframes <- list()

# Loop para gerar os data frames separados com base nos tres fragmentos de frag base
for (frag in fragments) {
  df_list <- list()
  
  for (i in 1:length(dataframe.list)) {
    df2 <- dataframe.list[[i]]
    # Filtra as linhas com base no valor de "fragments" em "frag_base"
    filtered_df <- df2[df2$frag_base == frag, ]
    df_list[[i]] <- filtered_df
  }
  separated_dataframes[[frag]] <- df_list
}

# Loop para remover data frames vazios
for (frag in names(separated_dataframes)) {
  df_list <- separated_dataframes[[frag]]
  # Verifica se o data frame está vazio
  empty_df <- sapply(df_list, function(df3) nrow(df3) == 0)
  # Remove os data frames vazios da lista
  separated_dataframes[[frag]] <- df_list[!empty_df]
}

# Listar os nomes das listas de data frames presentes em "separated_dataframes"
lista_nomes <- names(separated_dataframes)

# Loop para renomear os data frames em cada lista
for (lst_nome in lista_nomes) {
  # Obter a lista atual
  lst <- separated_dataframes[[lst_nome]]
  # Verificar o numero total de data frames dentro da lista
  num_dataframes <- length(lst)
  # Renomear os data frames dentro da lista automaticamente
  for (i in 1:num_dataframes) {
    new_name <- paste0("frag_level_", i)
    names(lst)[[i]] <- new_name
  }
  
  # Atribuir a lista atual de volta a "separated_dataframes"
  separated_dataframes[[lst_nome]] <- lst
}

# Verificar quantos data frames existem em "separated_dataframes"
num_dataframes <- length(separated_dataframes)

# Criar os novos nomes automaticamente
novos_nomes <- paste0("d1_data_dez_mil_0", 1:num_dataframes)

# Renomear as listas usando a funcao names
names(separated_dataframes) <- novos_nomes

# Converter a coluna frag_base para character em todos os data frames da lista
separated_dataframes <- lapply(separated_dataframes, function(df_list) {
  df_list <- lapply(df_list, function(df) {
    df <- df %>% mutate(frag_base = as.character(frag_base))
  })
  return(df_list)
})

# Converter a coluna fragmentation_name para fator em todos os data frames da lista
separated_dataframes <- lapply(separated_dataframes, function(df_list) {
  df_list <- lapply(df_list, function(df) {
    df <- df %>% mutate(fragmentation_name = as.factor(fragmentation_name))
  })
  return(df_list)
})

# Unir os data frames de cada lista em separado
combined_dfs <- lapply(separated_dataframes, function(df_list) do.call(rbind, df_list))

# Funcao para remover valores repetidos em cada data frame
remove_repeated_values <- function(df) {
  df_unique <- lapply(split(df, df$fragmentation_name), function(sub_df) {
    sub_df[!duplicated(sub_df$result_fisher), ]
  })
  do.call(rbind, df_unique)
}

# Aplicar a funcao a cada data frame na lista combined_dfs
combined_dfs <- lapply(combined_dfs, remove_repeated_values)

# Renomear a coluna "fragmentation_name" para "frag_level" e remover "_results_fisher" dos valores
combined_dfs <- lapply(combined_dfs, function(df4) {
  df4 <- df4 %>% rename(frag_level = `fragmentation_name`)
  df4$frag_level <- str_remove(df4$frag_level, "_results_fisher_dez_mil")
  return(df4)
})

# Adiciona coluna id nos data frames 
# Funcao para adicionar a coluna "id" em um data frame usando o valor do objeto "name"
add_id_column <- function(df3, id) {
  df3$id <- id
  return(df3)
}

# Usando lapply para aplicar a funcao a cada data frame na lista
combined_dfs <- lapply(combined_dfs, add_id_column, id = name)

#adiciona coluna taxon nos data frames 
# Funcao para adicionar a coluna "taxon" em um data frame usando o valor do objeto "taxon"
add_taxon_column <- function(df3, taxon) {
  df3$taxon <- taxon
  return(df3)
}

# Usando lapply para aplicar a funcao a cada data frame na lista
combined_dfs <- lapply(combined_dfs, add_taxon_column, taxon = taxon)

# Adicionar uma nova coluna frag_base_id com valores numericos sequenciais
for (i in seq_along(combined_dfs)) {
  combined_dfs[[i]]$frag_base_id <- sprintf("%02d", i)
}

# Salvar arquivos     
lapply(names(combined_dfs), function(x) {
  write.csv(combined_dfs[[x]], file = paste0(caminho_pasta, "/", x, ".csv"), row.names = FALSE)
})
