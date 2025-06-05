# Scrip para analise dos dados do meu mestrado utilizando modelos glmmtmb. Ajustamos os modelos com a familia de distribuicao lognormal, que foi a
# distribuicao que teve o melhor ajuste para os nossos dados

# Versao do R utilizada: 4.2.3

#######################################################################################################################################
#BIBLIOTECAS USADAS
#######################################################################################################################################
library(dplyr)
library(openxlsx)
library(AICcmodavg)
library(glmmTMB)
library(DHARMa)
library(ggplot2)

# Carrega data frame             
data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_completos.csv", header = TRUE, sep = ",") 

# Preparacao tabela para modelos
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
  
  #https://stats.stackexchange.com/questions/572642/glm-with-gamma-distribution-choosing-between-two-link-functions
  
  ##GLMM GAMMA 
  modelo_01 <- glmmTMB(SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid), data = data_geral, family = lognormal)
  modelo_02 <- glmmTMB(SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid), data = data_geral, family = lognormal)
  modelo_03 <- glmmTMB(SLOSSratio ~ SSnumfrag + (1|idestudo/SLid), data = data_geral, family = lognormal)
  modelo_04 <- glmmTMB(SLOSSratio ~ grupotax + (1|idestudo/SLid), data = data_geral, family = lognormal)
  modelo_05 <- glmmTMB(SLOSSratio ~ 1 + (1|idestudo/SLid), data = data_geral, family = lognormal)
  
  # Criar lista dos modelos para comparar com AICTab depois
  models.list<-list(modelo_01, modelo_02, modelo_03, modelo_04, modelo_05)
  
  # Criar tabela com comparacoes dos modelos 
  table_AIC<-aictab(models.list, modnames= c("modelo 01: SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid)", 
                                             "modelo 02: SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid)",
                                             "modelo 03: SLOSSratio ~ SSnumfrag + (1|idestudo/SLid)", 
                                             "modelo 04: SLOSSratio ~ grupotax + (1|idestudo/SLid)",
                                             "modelo 05: SLOSSratio ~ 1 + (1|idestudo/SLid)"))
  
  # Calcula o r2 do modelo selecionado 
  # performance::r2(modelo_01)
  # performance::r2(modelo_02)
  # performance::r2(modelo_03)
  # performance::r2(modelo_04)
  # performance::r2(modelo_05)
  
 # summary(modelo_04)
  
  # Salva a tabela AIC em xlsx
  # write.xlsx(table_AIC, file = "D:/Arquivos/Documents/Mestrado/Tabelas_analises/analise_log_fisher/dez_mil/tabela_gerlal.xlsx")

  # Grafico de diagnostico do modelo selecionado
  residuo <- simulateResiduals(fittedModel = modelo_02, n = 1000)
  plot(residuo)
  
  ####################################################################################################################################################
  # Predito do modelo
  ####################################################################################################################################################
  
  # Calcular os valores preditos
  predictos <- predict(modelo_04, newdata = data_geral, type = "response", re.form = NA, se.fit = TRUE)

  # Adiciona os preditos e erro padrao na tabela
  data_geral$predict <- predictos$fit
  data_geral$se <- predictos$se.fit
  
  data_geral$type <- "A) Dados completos"
  
  # Limpa Environment 
  rm(list = setdiff(ls(), c("data_geral")))
   
  #########################################################################################################################################################
  # ESPECIALISTAS
  ##########################################################################################################################################################
  
  # Carrega data frame             
  data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_especialista.csv", header = TRUE, sep = ",") 
    
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
  data_especialista <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)
  
  # Cria coluna SLOSS ratio
  data_especialista$SLOSSratio <- data_especialista$SSfisher / data_especialista$SLfisher
  
  # Adicionando o prefixo "frag_" a coluna SLid
  data_especialista <- data_especialista %>% 
    mutate(SLid = paste("frag_", SLid, sep = ""))
  
  # Histograma
  hist(data_especialista$SLOSSratio, 10)
  
  # Limpa Environment 
  rm(list = setdiff(ls(), c("data_geral", "data_especialista")))
    
  #https://stats.stackexchange.com/questions/572642/glm-with-gamma-distribution-choosing-between-two-link-functions
  
  ##GLMM GAMMA 
  modelo_01 <- glmmTMB(SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid), data = data_especialista, family = lognormal)
  modelo_02 <- glmmTMB(SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid), data = data_especialista, family = lognormal)
  modelo_03 <- glmmTMB(SLOSSratio ~ SSnumfrag + (1|idestudo/SLid), data = data_especialista, family = lognormal)
  modelo_04 <- glmmTMB(SLOSSratio ~ grupotax + (1|idestudo/SLid), data = data_especialista, family = lognormal)
  modelo_05 <- glmmTMB(SLOSSratio ~ 1 + (1|idestudo/SLid), data = data_especialista, family = lognormal)
  
  # Criar lista dos modelos para comparar com AICTab depois
  models.list<-list(modelo_01, modelo_02, modelo_03, modelo_04, modelo_05)
  
  # Criar tabela com comparacoes dos modelos 
  table_AIC<-aictab(models.list, modnames= c("modelo 01: SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid)", 
                                             "modelo 02: SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid)",
                                             "modelo 03: SLOSSratio ~ SSnumfrag + (1|idestudo/SLid)", 
                                             "modelo 04: SLOSSratio ~ grupotax + (1|idestudo/SLid)",
                                             "modelo 05: SLOSSratio ~ 1 + (1|idestudo/SLid)"))
  
  # Calcula o r2 do modelo selecionado 
  # performance::r2(modelo_01)
  # performance::r2(modelo_02)
  # performance::r2(modelo_03)
  # performance::r2(modelo_04)
  # performance::r2(modelo_05)
  
  # summary(modelo_04)
  
  # Salva a tabela AIC em xlsx
 # write.xlsx(table_AIC, file = "D:/Arquivos/Documents/Mestrado/Tabelas_analises/analise_log_fisher/dez_mil/tabela_especialista.xlsx")
  
  # Grafico de diagnostico do modelo selecionado
 residuo <- simulateResiduals(fittedModel = modelo_04, n = 1000)
  plotSimulatedResiduals(residuo)
  
  ####################################################################################################################################################
  # Predito do modelo
  ####################################################################################################################################################
  
  # Calcular os valores preditos
  predictos <- predict(modelo_04, newdata = data_especialista, type = "response", re.form = NA, se.fit = TRUE)
  
  # Adiciona os preditos e erro padrao na tabela
  data_especialista$predict <- predictos$fit
  data_especialista$se <- predictos$se.fit
  
  data_especialista$type <- "B) Dados especialistas"
  
  # Limpa Environment 
  rm(list = setdiff(ls(), c("data_geral", "data_especialista")))
  
  #######################################################################################################################################################
  # GENERALISTA
  #######################################################################################################################################################
  
  # Carrega data frame             
  data <- read.csv ("E:/Mestrado/Tabelas_analises/01_tabelas_fragmentos/dados_generalista.csv", header = TRUE, sep = ",") 
  
  # Remove o estudo da Erica Hasui 4-3
  data <- data[data$id != "erica_hasui_4-3",]
  
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
  
  # ordena as colunas do data frame
  data_generalista <- select(data_2_final, id, idestudo, grupotax, SLid, SLfisher,  SSfisher, SSnumfrag)
  
  # Cria coluna SLOSS ratio
  data_generalista$SLOSSratio <- data_generalista$SSfisher / data_generalista$SLfisher
  
  # Adicionando o prefixo "frag_" a coluna SLid
  data_generalista <- data_generalista %>% 
    mutate(SLid = paste("frag_", SLid, sep = ""))
  
  # Histograma
  hist(data_generalista$SLOSSratio, 10)
  
  # Limpa Environment 
  rm(list = setdiff(ls(), c("data_geral", "data_especialista", "data_generalista")))
    
  # https://stats.stackexchange.com/questions/572642/glm-with-gamma-distribution-choosing-between-two-link-functions
  
  ## GLMM GAMMA 
  modelo_01 <- glmmTMB(SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid), data = data_generalista, family = lognormal)
  modelo_02 <- glmmTMB(SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid), data = data_generalista, family = lognormal)
  modelo_03 <- glmmTMB(SLOSSratio ~ SSnumfrag + (1|idestudo/SLid), data = data_generalista, family = lognormal)
  modelo_04 <- glmmTMB(SLOSSratio ~ grupotax + (1|idestudo/SLid), data = data_generalista, family = lognormal)
  modelo_05 <- glmmTMB(SLOSSratio ~ 1 + (1|idestudo/SLid), data = data_generalista, family = lognormal)
  
  # Criar lista dos modelos para comparar com AICTab depois
  models.list<-list(modelo_01, modelo_02, modelo_03, modelo_04, modelo_05)
  
  # Criar tabela com comparacoes dos modelos 
  table_AIC<-aictab(models.list, modnames= c("modelo 01: SLOSSratio ~ SSnumfrag * grupotax + (1|idestudo/SLid)", 
                                             "modelo 02: SLOSSratio ~ SSnumfrag + grupotax + (1|idestudo/SLid)",
                                             "modelo 03: SLOSSratio ~ SSnumfrag + (1|idestudo/SLid)", 
                                             "modelo 04: SLOSSratio ~ grupotax + (1|idestudo/SLid)",
                                             "modelo 05: SLOSSratio ~ 1 + (1|idestudo/SLid)"))
  
  # Calcula o r2 do modelo selecionado 
  # performance::r2(modelo_01)
  # performance::r2(modelo_02)
  # performance::r2(modelo_03)
  # performance::r2(modelo_04)
  # performance::r2(modelo_05)
  
  # summary(modelo_04)
  
  # Salva a tabela AIC em xlsx
  # write.xlsx(table_AIC, file = "D:/Arquivos/Documents/Mestrado/Tabelas_analises/analise_log_fisher/dez_mil/tabela_generalista.xlsx")
  
  # Grafico de diagnostico do modelo selecionado
  residuo <- simulateResiduals(fittedModel = modelo_04, n = 1000)
  plotSimulatedResiduals(residuo)
  
  ####################################################################################################################################################
  # Predito do modelo
  ####################################################################################################################################################
  
  # Calcular os valores preditos
  predictos <- predict(modelo_04, newdata = data_generalista, type = "response", re.form = NA, se.fit = TRUE)
  
  # Adiciona os preditos e erro padrao na tabela
  data_generalista$predict <- predictos$fit
  data_generalista$se <- predictos$se.fit
  
  data_generalista$type <- "C) Dados generalistas"
  
  # Limpa Environment 
  rm(list = setdiff(ls(), c("data_geral", "data_especialista", "data_generalista")))
    
  ######################################################################################################################################################
  # FAZ O GRAFICO
  ######################################################################################################################################################
  
  # Agrupa os data frames em um unico
  data_final <- rbind(data_geral, data_especialista, data_generalista)
  
  # Reorganizar os niveis da variavel type
  data_final$type <- factor(data_final$type, levels = c("A) Dados completos", "B) Dados especialistas", "C) Dados generalistas"))
    
  # Calcule as contagens manualmente
  count_data <- data_final %>%
    group_by(grupotax, type) %>%
    summarise(count = n())
  
  # Junte as contagens ao data frame original
  data_final <- left_join(data_final, count_data, by = c("grupotax", "type"))
  
  # Criar o grafico
  ggplot(data_final, aes(x = grupotax, y = predict, color = grupotax)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = predict-(1.96*se), ymax = predict+(1.96*se)), 
                  width = .1, linewidth = 1.5) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1.3) +  # Linha tracejada vermelha em y = 1
    
    geom_text(aes(label = count,
                  y = predict + (1.96 * se) + 0.1),
              size = 3.5,
              position = position_dodge(width = 0.8),
              vjust = 0.5,
              color = "black") + 
    
    labs(x = "Táxons",
         y = "Razão alfa (± 95% CI)") +
    facet_wrap(~ type) + 
    theme_test() +
    theme(  panel.background = element_rect(fill = NA, color = "black"),  # Cor da moldura
            panel.border = element_rect(fill = NA, color = "black"),      # Cor da moldura
            #panel.grid.major = element_line(color = "gray", size = 0.5),  # Linhas principais do grid
            #panel.grid.minor = element_line(color = "gray", size = 0.2),                           # Remover linhas secundárias do grid
            plot.background = element_rect(fill = "white"),                # Cor de fundo
            legend.key = element_blank(),
            axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            plot.title = element_text(size = 15),
            strip.text = element_text(size = 15)) +
    guides(color = "none") 