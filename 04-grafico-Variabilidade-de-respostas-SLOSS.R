#Este script fi utilizado para fazer o grafico para avaliar a variacao da razao do indice alfa de acordo com os niveis de fragmentacao
#e identificar possiveis padroes, estimamos a media da razao do indice alfa para cada nivel de
#fragmentacao proveniente de cada fragmento grande.

# Versao do R utilizada: 4.2.3

#######################################################################################################################################
#BIBLIOTECAS USADAS
#######################################################################################################################################
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)
#######################################################################################################################################
#PREPARACOO DOS DADOS
#######################################################################################################################################

# carrega data frame             
data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_completos.csv", header = TRUE, sep = ",") 

# Remover linhas com id "erica_hasui_4-3"
data <- data[data$id != "erica_hasui_4-3",]

# Mapear valores da coluna 'taxon' para novas categorias
data <- data %>%
  mutate(taxon = case_when(
    taxon == "plants" ~ "Plantas",
    taxon == "insects" ~ "Invertebrados",
    taxon == "amphibians" ~ "Vertebrados-não-voadores",
    taxon == "reptiles" ~ "Vertebrados-não-voadores",
    taxon == "birds" ~ "Vertebrados-voadores",
    taxon == "mammals" ~ "Vertebrados-não-voadores",
    taxon == "bats" ~ "Vertebrados-voadores",
    TRUE ~ taxon  # Mantem o valor original se nao houver correspondencia
  ))

# Criar novo dataframe data_1 com algumas transformacoes
data_1 <- data %>%
  mutate(numfrag = as.integer(sub("^f", "", frag_level))) %>%
  select(numfrag, result_fisher, id, taxon, frag_base_id, area) %>%
  rename(idestudo = id, indfisher = result_fisher, grupotax = taxon, SLid = frag_base_id) %>%
  group_by(idestudo, SLid) %>%
  mutate(modpreserv = ifelse(numfrag ==1, "SL", "SS")) %>%
  filter(n() > 1) 

# Ajustar a coluna 'id' com valores unicos
data_1$id <- match(data_1$idestudo, unique(data_1$idestudo))

# Manipulacao adicional do dataframe data_1
data_1 <- data_1 %>%
  group_by(id, SLid) %>%
  mutate(SLidarea = ifelse(numfrag == 1, area, NA), 
         SLidarea = last(na.omit(SLidarea)))

# Calcular contagem maxima por id e fragmento
contagem_max_por_id_frag <- data_1 %>%
  filter(modpreserv == "SS") %>%
  group_by(idestudo, SLid) %>%
  summarize(contagem_max = n())

# Ajustar dataframe data_2
data_2 <- data_1 %>%
  left_join(contagem_max_por_id_frag, by = c("idestudo", "SLid")) %>%
  group_by(idestudo, SLid, modpreserv) %>%
  slice(rep(1:n(), times = ifelse(modpreserv == "SL", first(contagem_max), 1))) %>%
  ungroup() %>%
  select(-contagem_max)

# Separar data_2 em subdatasets para modpreserv SL e SS
data_2_SL <- data_2[data_2$modpreserv == "SL",]
data_2_SS <- data_2[data_2$modpreserv == "SS",]

# Renomear colunas nos subdatasets SL e SS
data_2_SL <- data_2_SL %>%
  rename(SLfisher = indfisher, SLarea = SLidarea)

data_2_SS <- data_2_SS %>%
  rename(SSfisher = indfisher, SSnumfrag = numfrag, SSarea = area)

# Selecionar colunas relevantes nos subdatasets SL e SS
data_2_SL <- select(data_2_SL, id, idestudo, SLid, SLfisher)
data_2_SS <- select(data_2_SS, id, idestudo, grupotax, SLid, SSnumfrag, SSfisher)

# Combinar subdatasets SL e SS em data_2_final
data_2_final <- cbind(data_2_SL, data_2_SS)

# Remover colunas desnecessarias de data_2_final
data_2_final <- data_2_final[, -c(5, 6, 8)]

# Selecionar colunas relevantes em data_2_final e criar dataframe data_geral
data_geral <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)

# Calcular razao SS/SL e ajustar coluna 'SLid'
data_geral$SLOSSratio <- data_geral$SSfisher / data_geral$SLfisher
data_geral <- data_geral %>% mutate(SLid = paste("frag_", SLid, sep = ""))

# Selecionar colunas relevantes em data_geral e criar dataframe data_geral_selec
data_geral_selec <- select(data_geral, idestudo, grupotax, SLid, SSnumfrag, SLOSSratio)

# Calcular media da razao SS/SL por idestudo, grupotax e SLid
data_geral_mean <- data_geral_selec %>%
  group_by(idestudo, grupotax, SLid, SSnumfrag) %>%
  summarise(media_SLOSSratio = mean(SLOSSratio, na.rm = TRUE))

# Criar novas linhas em data_geral_mean para garantir que todos os SLid tenham pelo menos uma entrada SSnumfrag = 1
novas_linhas <- data_geral_mean %>%
  distinct(idestudo, SLid) %>%
  mutate(SSnumfrag = 1, media_SLOSSratio = 1)

# Combinar novas linhas com data_geral_mean
data_geral_mean <- bind_rows(data_geral_mean, novas_linhas)

# Ordenar dataframe data_geral_mean
data_geral_mean <- data_geral_mean %>%
  arrange(idestudo, SLid, SSnumfrag)

# Agrupar data_geral_mean por grupotax e atribuir um fator unico para cada SLid
data_final <- data_geral_mean %>%
  group_by(grupotax) %>%
  mutate(shape = (dense_rank(SLid)))

# Converter 'shape' em um fator
data_final$shape <- factor(data_final$shape)

data_completo <- data_final

data_completo$type <- "A) Dados completos"
# Reorganizar os niveis da variavel type
data_completo$grupotax <- factor(data_completo$grupotax, levels = c("Plantas","Invertebrados","Vertebrados-não-voadores","Vertebrados-voadores"))

# Limpar o ambiente, mantendo apenas data_geral
rm(list = setdiff(ls(), c("data_completo", "Ids")))

##########################################################################################################################
#ESPECIALISTAS
########################################################################################################################

# carrega data frame             
data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_especialista.csv", header = TRUE, sep = ",") 

# Remover linhas com id "erica_hasui_4-3"
data <- data[data$id != "erica_hasui_4-3",]

# Mapear valores da coluna 'taxon' para novas categorias
data <- data %>%
  mutate(taxon = case_when(
    taxon == "plants" ~ "Plantas",
    taxon == "insects" ~ "Invertebrados",
    taxon == "amphibians" ~ "Vertebrados-não-voadores",
    taxon == "reptiles" ~ "Vertebrados-não-voadores",
    taxon == "birds" ~ "Vertebrados-voadores",
    taxon == "mammals" ~ "Vertebrados-não-voadores",
    taxon == "bats" ~ "Vertebrados-voadores",
    TRUE ~ taxon  # Mantem o valor original se nao houver correspondencia
  ))

# Criar novo dataframe data_1 com algumas transformacoes
data_1 <- data %>%
  mutate(numfrag = as.integer(sub("^f", "", frag_level))) %>%
  select(numfrag, result_fisher, id, taxon, frag_base_id, area) %>%
  rename(idestudo = id, indfisher = result_fisher, grupotax = taxon, SLid = frag_base_id) %>%
  group_by(idestudo, SLid) %>%
  mutate(modpreserv = ifelse(numfrag ==1, "SL", "SS")) %>%
  filter(n() > 1) 

# Ajustar a coluna 'id' com valores unicos
data_1$id <- match(data_1$idestudo, unique(data_1$idestudo))

# Manipulacao adicional do dataframe data_1
data_1 <- data_1 %>%
  group_by(id, SLid) %>%
  mutate(SLidarea = ifelse(numfrag == 1, area, NA), 
         SLidarea = last(na.omit(SLidarea)))

# Calcular contagem maxima por id e fragmento
contagem_max_por_id_frag <- data_1 %>%
  filter(modpreserv == "SS") %>%
  group_by(idestudo, SLid) %>%
  summarize(contagem_max = n())

# Ajustar dataframe data_2
data_2 <- data_1 %>%
  left_join(contagem_max_por_id_frag, by = c("idestudo", "SLid")) %>%
  group_by(idestudo, SLid, modpreserv) %>%
  slice(rep(1:n(), times = ifelse(modpreserv == "SL", first(contagem_max), 1))) %>%
  ungroup() %>%
  select(-contagem_max)

# Separar data_2 em subdatasets para modpreserv SL e SS
data_2_SL <- data_2[data_2$modpreserv == "SL",]
data_2_SS <- data_2[data_2$modpreserv == "SS",]

# Renomear colunas nos subdatasets SL e SS
data_2_SL <- data_2_SL %>%
  rename(SLfisher = indfisher, SLarea = SLidarea)

data_2_SS <- data_2_SS %>%
  rename(SSfisher = indfisher, SSnumfrag = numfrag, SSarea = area)

# Selecionar colunas relevantes nos subdatasets SL e SS
data_2_SL <- select(data_2_SL, id, idestudo, SLid, SLfisher)
data_2_SS <- select(data_2_SS, id, idestudo, grupotax, SLid, SSnumfrag, SSfisher)

# Combinar subdatasets SL e SS em data_2_final
data_2_final <- cbind(data_2_SL, data_2_SS)

# Remover colunas desnecessarias de data_2_final
data_2_final <- data_2_final[, -c(5, 6, 8)]

# Selecionar colunas relevantes em data_2_final e criar dataframe data_geral
data_geral <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)

# Calcular razao SS/SL e ajustar coluna 'SLid'
data_geral$SLOSSratio <- data_geral$SSfisher / data_geral$SLfisher
data_geral <- data_geral %>% mutate(SLid = paste("frag_", SLid, sep = ""))

# Selecionar colunas relevantes em data_geral e criar dataframe data_geral_selec
data_geral_selec <- select(data_geral, idestudo, grupotax, SLid, SSnumfrag, SLOSSratio)

# Calcular media da razao SS/SL por idestudo, grupotax e SLid
data_geral_mean <- data_geral_selec %>%
  group_by(idestudo, grupotax, SLid, SSnumfrag) %>%
  summarise(media_SLOSSratio = mean(SLOSSratio, na.rm = TRUE))

# Criar novas linhas em data_geral_mean para garantir que todos os SLid tenham pelo menos uma entrada SSnumfrag = 1
novas_linhas <- data_geral_mean %>%
  distinct(idestudo, SLid) %>%
  mutate(SSnumfrag = 1, media_SLOSSratio = 1)

# Combinar novas linhas com data_geral_mean
data_geral_mean <- bind_rows(data_geral_mean, novas_linhas)

# Ordenar dataframe data_geral_mean
data_geral_mean <- data_geral_mean %>%
  arrange(idestudo, SLid, SSnumfrag)

# Agrupar data_geral_mean por grupotax e atribuir um fator unico para cada SLid
data_final <- data_geral_mean %>%
  group_by(grupotax) %>%
  mutate(shape = (dense_rank(SLid)))

# Converter 'shape' em um fator
data_final$shape <- factor(data_final$shape)

data_especialista <- data_final

data_especialista$type <- "B) Dados especialistas"
# Reorganizar os niveis da variavel type
data_especialista$grupotax <- factor(data_especialista$grupotax, levels = c("Plantas","Invertebrados","Vertebrados-não-voadores","Vertebrados-voadores"))


# Limpar o ambiente, mantendo apenas data_geral
rm(list = setdiff(ls(), c("data_completo", "data_especialista", "Ids")))

#########################################################################################################################
#GENERALISTAS
########################################################################################################################

# Carrega data frame             
data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_generalista.csv", header = TRUE, sep = ",") 

# Remover linhas com id "erica_hasui_4-3"
data <- data[data$id != "erica_hasui_4-3",]

# Mapear valores da coluna 'taxon' para novas categorias
data <- data %>%
  mutate(taxon = case_when(
    taxon == "plants" ~ "Plantas",
    taxon == "insects" ~ "Invertebrados",
    taxon == "amphibians" ~ "Vertebrados-não-voadores",
    taxon == "reptiles" ~ "Vertebrados-não-voadores",
    taxon == "birds" ~ "Vertebrados-voadores",
    taxon == "mammals" ~ "Vertebrados-não-voadores",
    taxon == "bats" ~ "Vertebrados-voadores",
    TRUE ~ taxon  # Mantem o valor original se nao houver correspondencia
  ))

# Criar novo dataframe data_1 com algumas transformacoes
data_1 <- data %>%
  mutate(numfrag = as.integer(sub("^f", "", frag_level))) %>%
  select(numfrag, result_fisher, id, taxon, frag_base_id, area) %>%
  rename(idestudo = id, indfisher = result_fisher, grupotax = taxon, SLid = frag_base_id) %>%
  group_by(idestudo, SLid) %>%
  mutate(modpreserv = ifelse(numfrag ==1, "SL", "SS")) %>%
  filter(n() > 1) 

# Ajustar a coluna 'id' com valores unicos
data_1$id <- match(data_1$idestudo, unique(data_1$idestudo))

# Manipulacao adicional do dataframe data_1
data_1 <- data_1 %>%
  group_by(id, SLid) %>%
  mutate(SLidarea = ifelse(numfrag == 1, area, NA), 
         SLidarea = last(na.omit(SLidarea)))

# Calcular contagem maxima por id e fragmento
contagem_max_por_id_frag <- data_1 %>%
  filter(modpreserv == "SS") %>%
  group_by(idestudo, SLid) %>%
  summarize(contagem_max = n())

# Ajustar dataframe data_2
data_2 <- data_1 %>%
  left_join(contagem_max_por_id_frag, by = c("idestudo", "SLid")) %>%
  group_by(idestudo, SLid, modpreserv) %>%
  slice(rep(1:n(), times = ifelse(modpreserv == "SL", first(contagem_max), 1))) %>%
  ungroup() %>%
  select(-contagem_max)

# Separar data_2 em subdatasets para modpreserv SL e SS
data_2_SL <- data_2[data_2$modpreserv == "SL",]
data_2_SS <- data_2[data_2$modpreserv == "SS",]

# Renomear colunas nos subdatasets SL e SS
data_2_SL <- data_2_SL %>%
  rename(SLfisher = indfisher, SLarea = SLidarea)

data_2_SS <- data_2_SS %>%
  rename(SSfisher = indfisher, SSnumfrag = numfrag, SSarea = area)

# Selecionar colunas relevantes nos subdatasets SL e SS
data_2_SL <- select(data_2_SL, id, idestudo, SLid, SLfisher)
data_2_SS <- select(data_2_SS, id, idestudo, grupotax, SLid, SSnumfrag, SSfisher)

# Combinar subdatasets SL e SS em data_2_final
data_2_final <- cbind(data_2_SL, data_2_SS)

# Remover colunas desnecessarias de data_2_final
data_2_final <- data_2_final[, -c(5, 6, 8)]

# Selecionar colunas relevantes em data_2_final e criar dataframe data_geral
data_geral <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)

# Calcular razao SS/SL e ajustar coluna 'SLid'
data_geral$SLOSSratio <- data_geral$SSfisher / data_geral$SLfisher
data_geral <- data_geral %>% mutate(SLid = paste("frag_", SLid, sep = ""))

# Selecionar colunas relevantes em data_geral e criar dataframe data_geral_selec
data_geral_selec <- select(data_geral, idestudo, grupotax, SLid, SSnumfrag, SLOSSratio)

# Calcular media da razao SS/SL por idestudo, grupotax e SLid
data_geral_mean <- data_geral_selec %>%
  group_by(idestudo, grupotax, SLid, SSnumfrag) %>%
  summarise(media_SLOSSratio = mean(SLOSSratio, na.rm = TRUE))

# Criar novas linhas em data_geral_mean para garantir que todos os SLid tenham pelo menos uma entrada SSnumfrag = 1
novas_linhas <- data_geral_mean %>%
  distinct(idestudo, SLid) %>%
  mutate(SSnumfrag = 1, media_SLOSSratio = 1)

# Combinar novas linhas com data_geral_mean
data_geral_mean <- bind_rows(data_geral_mean, novas_linhas)

# Ordenar dataframe data_geral_mean
data_geral_mean <- data_geral_mean %>%
  arrange(idestudo, SLid, SSnumfrag)

# Agrupar data_geral_mean por grupotax e atribuir um fator unico para cada SLid
data_final <- data_geral_mean %>%
  group_by(grupotax) %>%
  mutate(shape = (dense_rank(SLid)))

# Converter 'shape' em um fator
data_final$shape <- factor(data_final$shape)

data_generalista<- data_final

data_generalista$type <- "C) Dados generalistas"

# Reorganizar os niveis da variavel type
data_generalista$grupotax <- factor(data_generalista$grupotax, levels = c("Plantas","Invertebrados","Vertebrados-não-voadores","Vertebrados-voadores"))

# Limpar o ambiente, mantendo apenas data_geral
rm(list = setdiff(ls(), c("data_completo", "data_especialista", "data_generalista", "Ids")))

#####################################################################################################
# GRAFICO
#####################################################################################################
data_final <-  rbind(data_completo, data_especialista, data_generalista)

# Obter grupos taxonomicos unicos da coluna 'grupotax' do dataframe data_final
grupos_taxonomicos <- unique(data_final$grupotax)

# Definir cores contrastantes pre-definidas
cores_contrastantes <- c("#FF4040", "#0000FF", "#FF1493", "#7FFF00", 
                         "#006400", "#FFB90F", "#000000", "#9932CC", 
                         "#00CDCD", "#483D8B", "#8B0000", "#CDB79E")

# Iterar sobre cada grupo taxonomico
for (grupo in grupos_taxonomicos) {
  # Filtrar o dataframe para o grupo atual
  data_subset <- data_final[data_final$grupotax == grupo, ]
  
  # Obter valores unicos da coluna 'idestudo'
  unique_id <- unique(data_subset$idestudo)
  
  # Verificar o numero de valores unicos
  if (length(unique_id) < 12) {
    # Selecionar um subconjunto de cores contrastantes se houver menos de 12 valores unicos
    cores_contrastantes_subset <- cores_contrastantes[1:length(unique_id)]
  } else {
    # Caso contrario, usar todas as cores contrastantes disponiveis
    cores_contrastantes_subset <- cores_contrastantes
  }
  
  # Criar um dataframe com 'idestudo' e cores contrastantes correspondentes
  data_cores <- data.frame(idestudo = unique_id, cores_contrastantes = cores_contrastantes_subset)
  
  # Realizar um left join entre data_subset e data_cores
  data_subset <- left_join(data_subset, data_cores, by = "idestudo")
  
  # Atribuir o dataframe resultante a uma variavel com nome dinamico
  assign(paste0("data_", gsub("-", "_", grupo)), data_subset)
}

# Combinar os dataframes resultantes em um unico dataframe
data_finalizado <- rbind(data_Plantas, data_Invertebrados, data_Vertebrados_não_voadores, data_Vertebrados_voadores)

data_finalizado <- data_finalizado %>%
  mutate(SLid = case_when(
    SLid == "frag_1" ~ "Maior fragmento",
    SLid == "frag_2" ~ "Segundo maior fragmento",
    SLid == "frag_3" ~ "Terceiro maior fragmento",
    TRUE ~ SLid  # Mantem o valor original se nao houver correspondencia
  ))


# Criar o grafico com a legenda para os shapes 900 x 1200
ggplot(data_finalizado, aes(x = SSnumfrag, y = media_SLOSSratio, color = cores_contrastantes)) +
  geom_line(size = 0.8, alpha = 0.5, aes(group = interaction(idestudo, SLid))) +
  geom_point(aes(shape = shape), size = 3, alpha = 0.5) +  
  labs(x = "Níveis de fragmentação", y = "Média razão alfa", color = "Fragmentos") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    legend.position = "right",  
    legend.direction = "vertical"  
  ) +
  scale_color_identity(name = "ID") +  # Mapear cores diretamente
  facet_nested(grupotax ~ type, scales = "free_y") +
  scale_shape_manual(name = "Fragmentos", values = c(10, 17, 15), labels = c("Maior", "Segundo maior", "Terceiro maior")) +  
  theme_test() +
  theme(panel.background = element_rect(fill = NA, color = "black"),  # Cor da moldura
          panel.border = element_rect(fill = NA, color = "black"),      # Cor da moldura
          #Panel.grid.major = element_line(color = "gray", size = 0.5),  # Linhas principais do grid
          #Panel.grid.minor = element_line(color = "gray", size = 0.2),                           # Remover linhas secundarias do grid
          plot.background = element_rect(fill = "white"),                # Cor de fundo
          legend.key = element_blank(), 
         axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth=1.2) +
  guides(color = "none") 

dados_completos <- data_finalizado[data_finalizado$type=="A) Dados completos",]

# Criar o grafico com a legenda para os shapes 900 x 1200
ggplot(dados_completos, aes(x = SSnumfrag, y = media_SLOSSratio, color = cores_contrastantes)) +
  geom_line(size = 0.8, alpha = 0.5, aes(group = interaction(idestudo, SLid))) +
  geom_point(aes(shape = "16"), size = 3, alpha = 0.5) +  
  labs(x = "Níveis de fragmentação", y = "Média razão alfa", color = "Fragmentos") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    legend.position = "right",  
    legend.direction = "vertical"  
  ) +
  scale_color_identity(name = "ID") +  # Mapear cores diretamente
  facet_nested(grupotax ~ SLid, scales = "free_y") +
  # Scale_shape_manual(name = "Fragmentos", values = c(10, 17, 15), labels = c("Maior", "Segundo maior", "Terceiro maior")) +  
  theme_test() +
  theme(panel.background = element_rect(fill = NA, color = "black"),  # Cor da moldura
        panel.border = element_rect(fill = NA, color = "black"),      # Cor da moldura
        # Panel.grid.major = element_line(color = "gray", size = 0.5),  # Linhas principais do grid
        # Panel.grid.minor = element_line(color = "gray", size = 0.2),                           # Remover linhas secundarias do grid
        plot.background = element_rect(fill = "white"),                # Cor de fundo
        legend.key = element_blank(), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth=1.2) +
  guides(color = "none") 

dados_especialistas <- data_finalizado[data_finalizado$type=="B) Dados especialistas",]

# Criar o grafico com a legenda para os shapes 900 x 1200
ggplot(dados_especialistas, aes(x = SSnumfrag, y = media_SLOSSratio, color = cores_contrastantes)) +
  geom_line(size = 0.8, alpha = 0.5, aes(group = interaction(idestudo, SLid))) +
  geom_point(aes(shape = "16"), size = 3, alpha = 0.5) +  
  labs(x = "Níveis de fragmentação", y = "Média razão alfa", color = "Fragmentos") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    legend.position = "right",  
    legend.direction = "vertical"  
  ) +
  scale_color_identity(name = "ID") +  # Mapear cores diretamente
  facet_nested(grupotax ~ SLid, scales = "free_y") +
  # Scale_shape_manual(name = "Fragmentos", values = c(10, 17, 15), labels = c("Maior", "Segundo maior", "Terceiro maior")) +  
  theme_test() +
  theme(panel.background = element_rect(fill = NA, color = "black"),  # Cor da moldura
        panel.border = element_rect(fill = NA, color = "black"),      # Cor da moldura
        # Panel.grid.major = element_line(color = "gray", size = 0.5),  # Linhas principais do grid
        # Panel.grid.minor = element_line(color = "gray", size = 0.2),                           # Remover linhas secundarias do grid
        plot.background = element_rect(fill = "white"),                # Cor de fundo
        legend.key = element_blank(), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth=1.2) +
  guides(color = "none") 

dados_generalistas <- data_finalizado[data_finalizado$type=="C) Dados generalistas",]

# Criar o grafico com a legenda para os shapes 900 x 1200
ggplot(dados_generalistas, aes(x = SSnumfrag, y = media_SLOSSratio, color = cores_contrastantes)) +
  geom_line(size = 0.8, alpha = 0.5, aes(group = interaction(idestudo, SLid))) +
  geom_point(aes(shape = "16"), size = 3, alpha = 0.5) +  
  labs(x = "Níveis de fragmentação", y = "Média razão alfa", color = "Fragmentos") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    legend.position = "right",  
    legend.direction = "vertical"  
  ) +
  scale_color_identity(name = "ID") +  # Mapear cores diretamente
  facet_nested(grupotax ~ SLid, scales = "free_y") +
  # Scale_shape_manual(name = "Fragmentos", values = c(10, 17, 15), labels = c("Maior", "Segundo maior", "Terceiro maior")) +  
  theme_test() +
  theme(panel.background = element_rect(fill = NA, color = "black"),  # Cor da moldura
        panel.border = element_rect(fill = NA, color = "black"),      # Cor da moldura
        # Panel.grid.major = element_line(color = "gray", size = 0.5),  # Linhas principais do grid
        # Panel.grid.minor = element_line(color = "gray", size = 0.2),                           # Remover linhas secundarias do grid
        plot.background = element_rect(fill = "white"),                # Cor de fundo
        legend.key = element_blank(), 
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.position = "none",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 15)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth=1.2) +
  guides(color = "none") 