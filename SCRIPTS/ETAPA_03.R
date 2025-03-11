load("RData/RData_etapa2.RData")

# Lista para armazenar os dataframes
lista_dados_CHIRPS <- list()

# Loop para percorrer cada estação meteorológica
for (i in 1:nrow(estacoes_filtradas)) {
  codigo_estacao <- estacoes_filtradas$OMM[i]  # Código da estação OMM
  
  # Criar um dataframe vazio para essa estação
  df_estacao <- data.frame(mes = character(), precip_total_mensal_CHIRPS = numeric(), stringsAsFactors = FALSE)
  
  # Loop para percorrer os arquivos .tif
  arquivos_tif <- list.files("DADOS/CHIRPS", pattern = "\\.tif$", full.names = TRUE)
  
  for (arquivo in arquivos_tif) {
    # Extrair o ano e mês do nome do arquivo
    nome_arquivo <- basename(arquivo)
    ano_mes <- sub("chirps-v2.0\\.(\\d{4})\\.(\\d{2})\\.tif", "\\1-\\2", nome_arquivo)
    
    # Carregar o raster e extrair o valor de precipitação para a estação
    raster_atual <- raster(arquivo)
    valor_precip <- terra::extract(raster_atual, estacoes_filtradas[i, ])
    
    # Adicionar a linha ao dataframe da estação
    df_estacao <- rbind(df_estacao, data.frame(mes = ano_mes, precip_total_mensal_CHIRPS = valor_precip))
  }
  
  # Adicionar o dataframe na lista, usando o código da estação como nome
  lista_dados_CHIRPS[[codigo_estacao]] <- df_estacao
}

# Verificar a estrutura de um dataframe
str(lista_dados_CHIRPS[[1]])  # Exemplo para verificar o primeiro dataframe

#########################################################################
######################## SALVAR RDAta - Etapa 03 ########################
#########################################################################

save.image("RData/RData_etapa3.RData")