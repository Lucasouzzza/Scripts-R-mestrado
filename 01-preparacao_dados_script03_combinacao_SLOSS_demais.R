# Este codigo tem como objetivo, criar combinacoes do maior fragmento em um paisagem com os demais, em varios niveis de 
# fragmentacao(e.g. entre um grande e dois fragmentos, um grande entre tres fragmentos etc, conforme a metodologia de Gavish 
# et al 2012) Em seguida e calculado ao indice alfa de diversidade de fisher das especues econtradas nessas combinacoes e do 
# fragmentomaior. Sao selecionados somente as combiancoes do nivel de fragmentacao mais elevado

# Artigo de referencia: Gavish, Y., Ziv, Y., Rosenzweig, M.L., 2012. Decoupling Fragmentation from Habitat 
# Loss for Spiders in Patchy Agricultural Landscapes.Conservation Biology 26, 150–159.

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
caminho_pasta <- file.path(diretorio2, "02_fisher_dez_mil")

name<- nome_pasta_atual
name_data<- paste0(name, "_dez_mil.csv")
file<-paste0(caminho_completo_data, "/",name_data)

rm(list=setdiff(ls(), c("diretorio2" ,"file", "name", "caminho_pasta", "arquivos_script", "arquivo")))

tryCatch({
  
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
 
 combinations<-2:100
 # Cria vetor vazio para armazenar as tabelas
 tables.list = list()

 for(c in 1:length(combinations)){ 
    
    # Variavel referente ao numero de repeticoes
    r<-combinations[c]
    
    # Criar possiveis combinacoes
    combsite <- lapply(r, function(x) combn(fragselect$patch, x)) 
    # Transformar em dataframe
    combsite<-as.data.frame(combsite[1])
 
    # Transpose   
    combsite<-as.data.frame(t(combsite))  
    # Variavel das letras (cria uma sequencia de letras utilizando a variavel s)
    l<-1:r 
    # Renomeia as colunas combsite2 utilizando variavel das letras
    colnames(combsite)<- paste0("fragment_", l) 
 
    # Corresponde os valores de IDs a tabela de pesquisa (=fragselect) e retorne areakm2. Armazenar em "novo".
    myfunction <- function(x){  
    fragselect$area_ha[match(x, fragselect$patch)]
    }

    # Cria nome para as colunas area utilizando a variavel z
    col<- paste0("area_", l) 
 
    # Loop para adicionar as colunas com valores das areas
    for(d in 1:length(col)){  
        combsite[,col[d]] = myfunction(combsite[,d])
        }

    # Faz a soma das areas
    combsite <- combsite %>% mutate(count = combsite %>% select(starts_with("area_")) %>% rowSums())
    # Renomeia a coluna da soma das areas
    combsite <- rename(combsite, sum_areas = count) 
    # Cria colunas mais 5% e faz os calculos
    fragselect["plus_5%"] <- fragselect$area_ha + (fragselect$area_ha/100*5) 
    # Cria coluna menos 5% e faz os calculos
    fragselect["minus_5%"] <- fragselect$area_ha - (fragselect$area_ha/100*5) 
    # Classificar fragselect de maior para menor
    fragselect1 <- fragselect[order(-fragselect$area_ha), ] 
    # Criar tabela para resultados fragmenacao
    fragme_table <- data.frame(matrix(ncol = 4+r+r, nrow=0)) 
    fragments <- paste0("fragment_", l)
    areas <- paste0("area_", l)
    colnames(fragme_table) <- c("frag_base", "area_base", fragments, areas, "sum_areas", "result_fragmentation") 
    # Definir numero de repeticoes=numero de fragmentos distinctos (menos o menor fragmento, porque nao pode ter dois que somam para essa area)
    num_reps <- dim(fragselect1)[1] 

    for(e in 1:num_reps){
        # Para o fragmento segundo maior (= segunda linha em fragselect), excluindo todas as combinacoes com o maior fragmento
        frag_ID <- fragselect1[e,2] 
        # Selecionar o fragmento na primeira linha da tabela fragselect
        tam_frag_base <- fragselect1[e,1]
        # Substitui todos os valores iguais ao frag_ID por NA 
        combsite[combsite == frag_ID] <- NA 
        # Exclui todas as linhas que tenham pelo menos um NA
        combsite <- na.omit(combsite) 
        # Checar em cada linha se o valor somado das areas das dois frags cai al dentro de +/- 5% daquele fragmento
        larger <- fragselect1[e,3]  
        small <- fragselect1[e,4]
        # Checa para cada combinacao se cai al dentro dos valores maior e menor
        combsite["result_fragmentation"] <- ifelse(combsite$sum_areas <= larger & combsite$sum_areas >= small, "OK", "No") 
        # Checar se ha combinacoes com OK
        number_OK <- table(combsite$result_fragmentation) 
        number_OK1 <- dim(number_OK)
  
          if(number_OK1>1){
             # Guardar linhas com "OK" numa tabela
             result_fragmentation <- combsite[combsite$result_fragmentation == "OK",] 
             # Adicionar o fragmento base com tamanho
             num_rows <- dim(result_fragmentation)[1] 
             frag_base <- rep(frag_ID, num_rows)
             tam_frag_base1 <- rep(tam_frag_base, num_rows)
             # Juntar
             result_fragmentation <- cbind(frag_base, tam_frag_base1, result_fragmentation) 
             # Adicionar no fragme_table
             fragme_table <- rbind(fragme_table, result_fragmentation) 
             }
         } 

    frag_base_different <- fragme_table[['frag_base']]

    # Backend paralelo
    # Registra o backend paralelo
    registerDoFuture()
    # Calcula o numero de nucleos disponiveis, menos 1
    num_workers <- parallel::detectCores() - 1
    # Configura o plano paralelo com o numero de workers
    plan(multisession, workers = num_workers)

    # Loop para criar data frames utilizando frag_base_different, remove as colunas desnecessarias e depois transforma em uma lista
    comb.list <- foreach(f = 1:length(frag_base_different), .combine = 'c') %dopar% {
                # Filtrar o data frame com base nas colunas geradas
                assign(paste0("comb_", f),
                filter(frag, patch %in% fragme_table[f, 3:(2 + r)])
                [,-c(1,2)])
                # Obter a lista de data frames
                comb.list <- mget(ls(pattern = "comb_.*"))
    }

    # Adiciona as especies que estao faltando. referencia: https://stackoverflow.com/questions/69758291/add-rows-to-list-of-dataframes-from-another-dataframe
    comb.list <- imap(comb.list, ~ .x %>% bind_rows(spe))

    # Faz ajustes na lista: verifica se tem especie repetida. referencia: https://stackoverflow.com/questions/57288991/how-to-group-and-summarise-each-data-frame-in-a-list-of-data-frames
    comb.list <- future_map(comb.list, ~.x %>% group_by(species) %>% summarize(total_abund = sum(total_abund)))
    # Formata para o formato aceito pelo pacote vegan
    comb.list <- future_map(comb.list, ~.x %>% spread(species, total_abund))

    # Ecerra backend paralelo
    plan(sequential)

    # Lapply para aplicar a funcao fisher.alpha em toda a lista
    fisher.list <- lapply(comb.list, function(x)fisher.alpha(x)) 

    # Criar tabela para salvar valores de Fisher
    results_fisher <- ldply (fisher.list, data.frame)
    colnames(results_fisher) <- c("fragment","result_fisher")

    # Remover possiveis combinacoes repetidas
    results_fisher <- unique(results_fisher)
    # Resultado final
    fragme_table_level <- fragme_table
    fragme_table_level["result_fisher"] <- results_fisher$result_fisher
 
    # Remove linhas iguais *seguranca
    fragme_table_level <- fragme_table_level %>% distinct()
 
    # Salvar em tables_list
    tables.list[[c]] = fragme_table_level

    # Subtrair
    compri <-length(tables.list)
    compri<- compri+1
    compri<-sprintf("%02d", 2:compri)

    # Rename dataframes with list
    names(tables.list) <- paste0('f', compri, '_results_fisher_dez_mil')

 }

}, error = function(e) {
  # Tratar o erro aqui, por exemplo, exibir uma mensagem de erro ou realizar alguma acao alternativa.
  cat("Erro no processamento de:",name , conditionMessage(e), "\n")
})
 
 # Salvar     
 lapply(names(tables.list), function(x) {
   write.csv(tables.list[[x]], file = paste0(caminho_pasta, "/", x, ".csv"), row.names = FALSE)
 })
 