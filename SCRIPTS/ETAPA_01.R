#limpar variaveis
rm(list=ls())

#limpando console
cat("\014")

# Lista de pacotes necessários
pacotes <- c("BrazilMet", "dplyr", "httr", "readr", "sf", 
             "vctrs", "writexl", "lubridate", "naniar", 
             "VIM", "ggplot2", "zoo", "terra")

# Verifica e instala apenas os pacotes que não estão instalados
pacotes_instalar <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if (length(pacotes_instalar)) install.packages(pacotes_instalar)

# Carrega os pacotes
lapply(pacotes, library, character.only = TRUE)

estacoes <-BrazilMet::see_stations_info()

# Criar a coluna 'geometry' no formato POINT (Longitude Latitude)
estacoes <- st_as_sf(estacoes, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# Ver o dataframe atualizado
print(estacoes)

plot(estacoes$geometry)

# Carregar o shapefile da região específica
shapefile <- "DADOS/SERGIPE/BUFFER.shp" ##Inserir caminho e nome do arquivo .shp##
regiao <- st_read(paste0(shapefile)) %>% st_transform(4326)
plot(regiao$geometry)

# Filtrar as estações dentro da região do shapefile
estacoes_filtradas <- st_intersection(estacoes, regiao)
plot(estacoes_filtradas$geometry)

# Diretório base onde estão as pastas de cada ano
base_dir <- "DADOS/INMET"

# Inicializar uma lista para armazenar os arquivos encontrados para cada estação
todos_arquivos <- list()

# Iterar sobre cada código de estação na coluna OMM do dataframe estacoes_filtradas
for (i in seq_along(estacoes_filtradas$OMM)) {
  # Obter o código da estação atual
  codigo_estacao <- estacoes_filtradas$OMM[i]
  
  # Listar todos os arquivos CSV que contêm o código da estação especificada
  arquivos <- list.files(base_dir, pattern = paste0("_", codigo_estacao, "_"), full.names = TRUE, recursive = TRUE)
  
  # Armazenar os arquivos encontrados na lista todos_arquivos
  todos_arquivos[[codigo_estacao]] <- arquivos
  
  # Mostrar os arquivos encontrados para cada estação
  if (length(arquivos) > 0) {
   cat("Arquivos encontrados para a estação", codigo_estacao, ":\n")
   cat(arquivos, "\n\n")
  } else {
    cat("Nenhum arquivo encontrado para a estação", codigo_estacao, ".\n\n")
  }
}

#SALVAR DATAFRAME DE CADA ESTAÇÃO

# Loop para processar cada estação
for (estacao in names(todos_arquivos)) {
  
  arquivos_estacao <- todos_arquivos[[estacao]]
  
  print(paste("Processando estação:", estacao))
  
  lista_arquivos <- list()
  
  for (arquivo in arquivos_estacao) {
    
    dados <- tryCatch({
      read.csv(arquivo, header = FALSE, sep = ";", skip = 9)[, 1:3]  # Ler apenas as 3 primeiras colunas
    }, error = function(e) {
      message(paste("Erro ao ler o arquivo:", arquivo))
      return(NULL)
    })
    
    if (!is.null(dados)) {
      
      # Detectar o ano da primeira entrada para ajustar o formato de DATA e HORA
      ano_inicial <- substr(dados$V1[1], 1, 4)
      
      # Renomear colunas
      colnames(dados) <- c("DATA", "HORA", "P_TOTAL_mm")
      
      # Ajustar a conversão de DATA e HORA para 2019 em diante
      if (ano_inicial >= "2019") {
        # Ajuste para 2019 ou posterior (formato YYYY/MM/DD para DATA e HHMM UTC para HORA)
        dados$DATA <- as.Date(dados$DATA, format = "%Y/%m/%d")
        dados$HORA <- gsub(" UTC", "", dados$HORA)  # Remover "UTC"
        dados$HORA <- sub("([0-9]{2})([0-9]{2})", "\\1:\\2", dados$HORA)  # Inserir ":"
        dados$HORA <- hms(paste0(dados$HORA, ":00"))  # Converter para hms
      } else {
        # Ajuste para anos anteriores a 2019 (formato YYYY-MM-DD para DATA e HH:MM para HORA)
        dados$DATA <- as.Date(dados$DATA, format = "%Y-%m-%d")
        dados$HORA <- hms(paste0(dados$HORA, ":00"))
      }
      
      # Substituir vírgulas e tratar -9999
      dados$P_TOTAL_mm <- gsub(",", ".", dados$P_TOTAL_mm)
      dados$P_TOTAL_mm[dados$P_TOTAL_mm == -9999] <- NA
      dados$P_TOTAL_mm <- as.numeric(dados$P_TOTAL_mm)
      
      # Adicionar o dataframe processado à lista
      lista_arquivos[[length(lista_arquivos) + 1]] <- dados
    }
  }
  
  df_estacao <- do.call(rbind, lista_arquivos)
  
  # Salvar o dataframe
  assign(estacao, df_estacao)
  
}

#########################################################################
######################## SALVAR RDAta - Etapa 01 ########################
#########################################################################

save.image("RData/RData_etapa1.RData")
