# Script para traduzir a coluna taxon para o ingles

# Versao do R utilizada: 4.2.3

# Carregar pacotes necessarios
library(data.table)
library(readr)
library(sf)
library(fs)
library(dplyr)

dados_gpkg <- st_read("C:/Users/lucas/Documents/Mestrado/Tabelas_analises/05_mapas/dados_fragmentos_dez_mil.gpkg")

dados_gpkg <- dados_gpkg %>%
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

# Renomeando a coluna studyid para idestudo
dados_gpkg <- rename(dados_gpkg, idestudo = study_id)

# Salvar os dados unidos como um arquivo GeoPackage
caminho_saida <- "C:/Users/lucas/Documents/Mestrado/Tabelas_analises/05_mapas/dados_fragmentos.gpkg"
st_write(dados_gpkg, caminho_saida)
