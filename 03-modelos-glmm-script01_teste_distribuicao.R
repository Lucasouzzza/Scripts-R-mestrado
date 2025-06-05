# Script de teste para escolha da melhor distribuicao da variavel resposta dos modelos GLMMs utilizados nas analises. Foram testadas as seguintes distribuicoes: lnorm, gamma, weibull, logis, exp, norm, unif

# Versao do R utilizada: 4.2.3

#######################################################################################################################################
# BIBLIOTECAS USADAS
#######################################################################################################################################
library(dplyr)
library(openxlsx)
library(AICcmodavg)
library(glmmTMB)
library(DHARMa)
library(ggplot2)
#######################################################################################################################################
# PREPARACAO DOS DADOS
#######################################################################################################################################

# Carrega data frame             
data <- read.csv ("D:/Mestrado/Tabelas_analises/analise_log_fisher/dez_mil/geral_lm.csv", header = TRUE, sep = ",") 

# Remove o estudo da Erica Hasui 4-3
data <- data[data$id != "erica_hasui_4-3",]

# Remove os cinco estudos com muitos fragmetos
data <- data[data$id != "cristina_banksleite_2006_1",]
data <- data[data$id != "cristina_banksleite_2006_2",]
data <- data[data$id != "cristina_banksleite_2006_3",]
data <- data[data$id != "renata_pardini_1",]
data <- data[data$id != "renata_pardini_2",]
data <- data[data$id != "renata_pardini_3",]

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

data_1 <- data %>%
  # Adiciona uma nova coluna 'numfrag' ao conjunto de dados  
  mutate(numfrag = as.integer(sub("^f", "", frag_level))) %>%
  # Seleciona as colunas especificas que serao mantidas em 'data_1'
  select(numfrag, result_fisher, id, taxon, frag_base_id, area) %>%
  # Renomeia as colunas 
  rename(idestudo = id, indfisher = result_fisher, grupotax = taxon, SLid = frag_base_id) %>%
  # Agrupa os dados pelo valor da coluna 'id_estudo'
  group_by(idestudo, SLid) %>%
  # Filtra as linhas onde 'numfrag' e igual ao primeiro ou ultimo valor de 'numfrag' dentro de cada grupo
  filter(numfrag == first(numfrag) | numfrag == last(numfrag)) %>%
  # Cria uma nova coluna 'Modpreserv' com base em uma condicao: 'base' se 'numfrag' for igual a 1, 'SL' caso contrario SS
  mutate(modpreserv = ifelse(numfrag ==1, "SL", "SS")) %>%
  # Remove todos os ids unicos (seguranca)
  filter(n() > 1) 

# Cria uma coluna com ids numericos
data_1$id <- match(data_1$idestudo, unique(data_1$idestudo))

# Cria uma coluna com areas de SLid
data_1 <- data_1 %>%
  group_by(id, SLid) %>%
  mutate(SLidarea = ifelse(numfrag == 1, area, NA), 
  SLidarea = last(na.omit(SLidarea)))

# Aqui eu igualo o numero de observaloes Sl com as observacoes SS
# Conte quantas linhas "max" existem para cada "id_estudo" e "id_frag"
contagem_max_por_id_frag <- data_1 %>%
  filter(modpreserv == "SS") %>%
  group_by(idestudo, SLid) %>%
  summarize(contagem_max = n())

# Repita as linhas "base" para cada "id_estudo" e "id_frag" com base na contagem de "max"
data_2 <- data_1 %>%
  left_join(contagem_max_por_id_frag, by = c("idestudo", "SLid")) %>%
  group_by(idestudo, SLid, modpreserv) %>%
  slice(rep(1:n(), times = ifelse(modpreserv == "SL", first(contagem_max), 1))) %>%
  ungroup() %>%
  select(-contagem_max)

# Separa o data frame em SL e SS 
data_2_SL <- data_2[data_2$modpreserv == "SL",]
data_2_SS <- data_2[data_2$modpreserv == "SS",]

# Renomeia a coluna indfisher dos dois data frames
data_2_SL <- data_2_SL %>%
  rename(SLfisher = indfisher, SLarea = SLidarea)

data_2_SS <- data_2_SS %>%
  rename(SSfisher = indfisher, SSnumfrag = numfrag, SSarea = area)

# Seleciona colunas de interesse nos dois data frames
data_2_SL <- select(data_2_SL, id, idestudo, SLid, SLfisher)
data_2_SS <- select(data_2_SS, id, idestudo, grupotax, SLid, SSnumfrag, SSfisher)

# Junta os dois data frames
data_2_final <- cbind(data_2_SL, data_2_SS)

# Remove colunas repetidas
data_2_final <- data_2_final[, -c(5, 6, 8)]

# Ordena as colunas do data frame
data_geral <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)

# Cria coluna SLOSS ratio
data_geral$SLOSSratio <- data_geral$SSfisher / data_geral$SLfisher

# Adicionando o prefixo "frag_" a coluna SLid
data_geral <- data_geral %>% 
  mutate(SLid = paste("frag_", SLid, sep = ""))

# Histograma
hist(data_geral$SLOSSratio, 10)

# Limpa Environment 
rm(list = setdiff(ls(), c("data_geral")))
  
  ##############################################################################
  # TESTE DE DISTRIBUICOES
  ##############################################################################
  
  # Carregar o pacote fitdistrplus
  library(fitdistrplus)
  
  # Seus dados
  seus_dados <- data_geral$SLOSSratio
  
  descdist(seus_dados, boot=1000)    
    
  # Lista para armazenar as distribuicoes que derem certo
  distribuicoes_corretas <- list()
  
  # Loop sobre todas as distribuicoes
  for (dist_name in c("norm", "exp", "lnorm", "weibull",
                      "gamma", "logis", "unif", "beta")) {
    tryCatch({
      # Tentar ajustar a distribuicao aos seus dados
      fit_dist <- fitdist(seus_dados, dist_name)
      
      # Se nao houver erros, armazenar a distribuicao na lista
      distribuicoes_corretas[[dist_name]] <- fit_dist
    }, error = function(e) {
      # Se ocorrer um erro, imprimir uma mensagem indicando a distribuicao e o erro
      cat("Erro ao ajustar a distribuição", dist_name, ":", conditionMessage(e), "\n")
    })
  }
  
  # Inicializar um quadro de dados vazio para armazenar os valores de AIC
  AIC_table <- data.frame(Distribuicao = character(), AIC = numeric(), stringsAsFactors = FALSE)
  
  # Loop sobre todas as distribuicoes ajustadas com sucesso
  for (dist_name in names(distribuicoes_corretas)) {
    # Acessar o valor do AIC para a distribuicao atual
    AIC_atual <- distribuicoes_corretas[[dist_name]]$aic
    
    # Adicionar os valores de AIC ao quadro de dados
    AIC_table <- rbind(AIC_table, data.frame(Distribuicao = dist_name, AIC = AIC_atual))
  }
    
  # Salva a tabela AIC em xlsx
   write.xlsx(AIC_table, file = "D:/Mestrado/Tabelas_analises/analise_log_fisher/dez_mil/tabela_AIC_gerlal.xlsx")
    
  lognormal<-distribuicoes_corretas$lnorm
  plot(lognormal)
  
  gamma<-distribuicoes_corretas$gamma
  plot(gamma)  