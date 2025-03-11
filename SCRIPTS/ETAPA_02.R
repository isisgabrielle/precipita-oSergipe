load("RData/RData_etapa1.RData")

# Função para agregar a precipitação horária em precipitação acumulada mensal
agregar_precipitacao_mensal <- function(df_estacao) {
  df_estacao$DATA <- as.Date(df_estacao$DATA)  # Certificar que a coluna DATA está no formato de data
  df_estacao$mes <- format(df_estacao$DATA, "%Y-%m")  # Extrair o mês e o ano
  
  # Agrupar por mês e calcular a precipitação acumulada mensal
  df_mensal <- df_estacao %>%
    group_by(mes) %>%
    summarise(
      precip_total_mensal = sum(P_TOTAL_mm, na.rm = TRUE),
      total_horas = n(),
      horas_na = sum(is.na(P_TOTAL_mm))
    )
  
  return(df_mensal)
}

# Função para aplicar o critério de qualidade nos dados mensais
limpar_dados_precipitacao_mensal <- function(df_mensal) {
  # Calcular a média e o desvio padrão da precipitação acumulada mensal
  media_mensal <- mean(df_mensal$precip_total_mensal, na.rm = TRUE)
  desvio_mensal <- sd(df_mensal$precip_total_mensal, na.rm = TRUE)
  
  # Definir o intervalo de aceitação (média ± 3,5 desvios padrão)
  limite_inferior <- media_mensal - 3.5 * desvio_mensal
  limite_superior <- media_mensal + 3.5 * desvio_mensal
  
  # Aplicar o critério de qualidade: marcar como NA os meses fora do intervalo
  df_mensal <- df_mensal %>%
    mutate(
      precip_total_mensal = ifelse(precip_total_mensal < limite_inferior | precip_total_mensal > limite_superior, NA, precip_total_mensal)
    )
  
  # Excluir meses com mais de 25% de dados ausentes
  df_mensal <- df_mensal %>%
    filter(horas_na / total_horas <= 0.25)
  
  return(df_mensal)
}

# Função principal para aplicar em cada estação
processar_dados_estacoes <- function(codigo_OMM) {
  # Nome do dataframe da estação com base no código OMM
  nome_df_estacao <- paste0(codigo_OMM)
  
  # Carregar o dataframe da estação (substitua por onde seus dataframes estão armazenados)
  df_estacao <- get(nome_df_estacao)
  
  # Passo 1: Agregar precipitação horária em precipitação mensal
  df_mensal <- agregar_precipitacao_mensal(df_estacao)
  
  # Passo 2: Aplicar o critério de qualidade nos dados mensais
  df_limpo <- limpar_dados_precipitacao_mensal(df_mensal)
  
  return(df_limpo)
}

# Aplicar a função para cada código OMM presente no dataframe estacoes_filtradas
lista_dados_limpos <- lapply(estacoes_filtradas$OMM, processar_dados_estacoes)

# Nomear cada elemento da lista com o respectivo código OMM
names(lista_dados_limpos) <- estacoes_filtradas$OMM


#########################################################################
######################## SALVAR RDAta - Etapa 02 ########################
#########################################################################

save.image("RData/RData_etapa2.RData")


