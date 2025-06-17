# Script para o gráfico do teste de ajuste à distribuição log-série das abundâncias dos fragmentos SL e suas combinações.
# Foram utilizadas combinações dos três maiores fragmentos de cada estudo.

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(data.table)
library(readr)
library(fs)
library(dplyr)
library(ggplot2)
library(ggh4x)

#T00 - Primeiro nivel de fragmentacao
# Defina o diretorio raiz
diretorio_raiz <- "C:/Users/lucas/Documents/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
#subdiretorios <- list.dirs(path = diretorio_raiz, recursive = TRUE)
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Inicializar um dataframe vazio para armazenar os dados
dados_unidos <- data.table()

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .csv que comecam com "data_" no diretorio atual
 # arquivos_csv <- list.files(path = subdiretorio, pattern = "^1_tab_dez_mil_.*\\.csv$", full.names = TRUE)
  arquivos_csv <- list.files(path = subdiretorio, pattern = "^T00_Primeiro_niveil_AIC_especies_dez_mil*\\.csv$", full.names = TRUE)
  
  # Unir os arquivos .csv em um unico dataframe, se houver algum
  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      # Ler o arquivo .csv e unir ao dataframe existente
      dados_unidos <- rbind(dados_unidos, fread(arquivo_csv))
    }
  }
}

dados_unidos <- as.data.frame(dados_unidos)

t00 <- dados_unidos

# Classifique a coluna 'area' por ordem crescente dentro de cada grupo de 'study_id'
t00 <- t00 %>%
  arrange(study_id, frag_base_area) %>%
  group_by(study_id) %>%
  mutate(tipo = case_when(
    row_number() == n() ~ "maior",
    row_number() == n() - 1 ~ "segundo_maior",
    row_number() == n() - 2 ~ "terceira_maior",
    TRUE ~ NA_character_  # Caso haja mais de 3 observacoes por 'study_id'
  )) %>%
  ungroup()

#remove colunas repetidas
t00 <- t00[, -c(7)]
#cria coluna fragmentation
t00["fragmentation"] <- "f01"

t00 <- select(t00, study_id, tipo, taxon, fragmentation, dAIC)

rm(list=setdiff(ls(), c("t00")))

#t01 - demais niveis de fragmentacao- combinacaoes do maior frag
# Defina o diretorio raiz
diretorio_raiz <- "C:/Users/lucas/Documents/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
#subdiretorios <- list.dirs(path = diretorio_raiz, recursive = TRUE)
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Inicializar um dataframe vazio para armazenar os dados
dados_unidos <- data.table()

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .csv que comecam com "data_" no diretorio atual
  # arquivos_csv <- list.files(path = subdiretorio, pattern = "^1_tab_dez_mil_.*\\.csv$", full.names = TRUE)
  arquivos_csv <- list.files(path = subdiretorio, pattern = "^T01_demais_niveis_AIC_especies_dez_mil*\\.csv$", full.names = TRUE)
  
  # Unir os arquivos .csv em um unico dataframe, se houver algum
  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      # Ler o arquivo .csv e unir ao dataframe existente
      dados_unidos <- rbind(dados_unidos, fread(arquivo_csv))
    }
  }
}

dados_unidos <- as.data.frame(dados_unidos)

t01 <- dados_unidos
t01["tipo"] <- "maior"
#remove colunas repetidas
t01 <- t01[, -c(7)]

# Use gsub para manter apenas o ultimo sufixo na coluna 'id'
t01$id <- gsub(".*_", "", t01$id)
# Renomeie a coluna 'id' para 'fragmentation'
t01 <- t01 %>%
  rename(fragmentation = id)

t01 <- select(t01, study_id, tipo, taxon, fragmentation, dAIC)

rm(list=setdiff(ls(), c("t00", "t01")))

#t02 - demais niveis de fragmentacao- combinacaoes do segundo maior frag
# Defina o diretorio raiz
diretorio_raiz <- "C:/Users/lucas/Documents/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
#subdiretorios <- list.dirs(path = diretorio_raiz, recursive = TRUE)
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Inicializar um dataframe vazio para armazenar os dados
dados_unidos <- data.table()

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .csv que comecam com "data_" no diretorio atual
  # arquivos_csv <- list.files(path = subdiretorio, pattern = "^1_tab_dez_mil_.*\\.csv$", full.names = TRUE)
  arquivos_csv <- list.files(path = subdiretorio, pattern = "^T02_demais_niveis_AIC_especies_dez_mil*\\.csv$", full.names = TRUE)
  
  # Unir os arquivos .csv em um unico dataframe, se houver algum
  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      # Ler o arquivo .csv e unir ao dataframe existente
      dados_unidos <- rbind(dados_unidos, fread(arquivo_csv))
    }
  }
}

dados_unidos <- as.data.frame(dados_unidos)

t02 <- dados_unidos
t02["tipo"] <- "segundo_maior"
#remove colunas repetidas
t02 <- t02[, -c(7)]

# Use gsub para manter apenas o ultimo sufixo na coluna 'id'
t02$id <- gsub(".*_", "", t02$id)
# Renomeie a coluna 'id' para 'fragmentation'
t02 <- t02 %>%
  rename(fragmentation = id)

t02 <- select(t02, study_id, tipo, taxon, fragmentation, dAIC)

rm(list=setdiff(ls(), c("t00", "t01", "t02")))

#t03 - demais niveis de fragmentacao- combinacaoes do terceiro maior frag
# Defina o diretorio raiz
diretorio_raiz <- "C:/Users/lucas/Documents/Mestrado/Estudos/"

# Lista de todos os subdiretorios dentro do diretorio raiz
#subdiretorios <- list.dirs(path = diretorio_raiz, recursive = TRUE)
subdiretorios <- dir_ls(path = diretorio_raiz, recurse = TRUE)

# Inicializar um dataframe vazio para armazenar os dados
dados_unidos <- data.table()

# Percorrer os subdiretorios
for (subdiretorio in subdiretorios) {
  # Listar os arquivos .csv que comecam com "data_" no diretorio atual
  # arquivos_csv <- list.files(path = subdiretorio, pattern = "^1_tab_dez_mil_.*\\.csv$", full.names = TRUE)
  arquivos_csv <- list.files(path = subdiretorio, pattern = "^T03_demais_niveis_AIC_especies_dez_mil*\\.csv$", full.names = TRUE)
  
  # Unir os arquivos .csv em um unico dataframe, se houver algum
  if (length(arquivos_csv) > 0) {
    for (arquivo_csv in arquivos_csv) {
      # Ler o arquivo .csv e unir ao dataframe existente
      dados_unidos <- rbind(dados_unidos, fread(arquivo_csv))
    }
  }
}

dados_unidos <- as.data.frame(dados_unidos)

t03 <- dados_unidos
t03["tipo"] <- "terceira_maior"
#remove colunas repetidas
t03 <- t03[, -c(7)]

# Use gsub para manter apenas o ultimo sufixo na coluna 'id'
t03$id <- gsub(".*_", "", t03$id)
# Renomeie a coluna 'id' para 'fragmentation'
t03 <- t03 %>%
  rename(fragmentation = id)

t03 <- select(t03, study_id, tipo, taxon, fragmentation, dAIC)

rm(list=setdiff(ls(), c("t00", "t01", "t02", "t03")))

dados_finais <- rbind(t00, t01, t02, t03)

# Remover a letra "f" da coluna "fragmentation"
dados_finais$fragmentation <- gsub("f", "", dados_finais$fragmentation)

rm(list=setdiff(ls(), c("dados_finais")))

dados_finais <- dados_finais %>%
  mutate(tipo = case_when(
    tipo == "maior" ~ "A) Maior fragmento",
    tipo == "segundo_maior" ~ "B) Segundo maior fragmento",
    tipo == "terceira_maior" ~ "C) Terceiro maior fragmento",
    TRUE ~ tipo  # Mantem o valor original se nao houver correspondencia
  ))

#cria uma coluna com ids numericos
dados_finais$id <- match(dados_finais$study_id, unique(dados_finais$study_id))

# Use sprintf para formatar os numeros
dados_finais$id <- sprintf("%02d", dados_finais$id)

dados_finais <- select(dados_finais, id, tipo, taxon, fragmentation, dAIC)

# Calcula a media de dAIC para cada fragmentation, levando em consideracao o id
dados_finais <- dados_finais %>%
  group_by(id, fragmentation) %>%
  mutate(categoria = case_when(
    dAIC <= 2 ~ "<=2",
    dAIC > 2 & dAIC <= 4 ~ "2-4",
    dAIC > 4 & dAIC <= 6 ~ "4-6",
    dAIC > 6 ~ ">6"
  ))

# Calcular a contagem de ids por categoria
contagem_categoria <- dados_finais %>%
  group_by(fragmentation, categoria, tipo) %>%
  summarise(contagem_ids = n())

# Reordenar os niveis do fator categoria
contagem_categoria$categoria <- factor(contagem_categoria$categoria, 
                                       levels = c("<=2", "2-4", "4-6", ">6"))

# Definir cores para cada categoria
cores <- c("<=2" = "#56B4E9", "2-4" = "#009E73", "4-6" = "#E69F00", ">6" = "#D55E00")

# Criar o grafico de frequencia relativa
ggplot(contagem_categoria, aes(x = categoria, y = contagem_ids, fill = categoria)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cores) +  # Definir cores manualmente
  #facet_wrap(~fragmentation, scales = "free_y") +
  facet_nested(fragmentation  ~ tipo, scales = "free_y", render_empty = FALSE) +
  labs(x = "dAIC médio", y = "Contagem de combinações", fill = "Categoria dAIC") +
  theme_test() + 
  theme(  panel.background = element_rect(fill = NA, color = "black"),  
          panel.border = element_rect(fill = NA, color = "black"),      
          plot.background = element_rect(fill = "white"),                
          legend.key = element_blank(), 
          legend.position = "bottom",
          axis.text.x = element_text(hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 12),
          strip.text = element_text(size = 12)) 
