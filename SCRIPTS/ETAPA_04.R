load("RData/RData_etapa3.RData")

# Criar uma lista para armazenar os resultados
resultados_metricas <- data.frame(codigo_OMM = character(),
                                  ME = numeric(), MAE = numeric(), RMSE = numeric(),
                                  PBIAS = numeric(), NSE = numeric(),
                                  r = numeric(), R2 = numeric(), KGE = numeric(),
                                  stringsAsFactors = FALSE)

# Função para calcular KGE
KGE_calculator <- function(obs, sim) {
  return(hydroGOF::KGE(sim, obs))
}

# Loop para calcular as métricas para cada estação
for (codigo in names(lista_dados_limpos)) {
  
  # Verificar se a estação está nas duas listas
  if (codigo %in% names(lista_dados_CHIRPS)) {
    
    # Extrair os dados para a estação
    dados_limpos <- lista_dados_limpos[[codigo]]
    dados_chirps <- lista_dados_CHIRPS[[codigo]]
    
    # Unir os dois dataframes pelo mês
    dados_combinados <- merge(dados_limpos, dados_chirps, by = "mes", suffixes = c("_limpos", "_CHIRPS"))
    
    # Calcular as métricas
    obs <- as.numeric(dados_combinados$precip_total_mensal)
    sim <- as.numeric(dados_combinados$precip_total_mensal_CHIRPS)
    
    ME_val <- mean(obs - sim, na.rm = TRUE)
    MAE_val <- mae(sim, obs, na.rm = TRUE)
    RMSE_val <- rmse(sim, obs, na.rm = TRUE)
    PBIAS_val <- pbias(sim, obs, na.rm = TRUE)
    NSE_val <- NSE(sim, obs, na.rm = TRUE)
    r_val <- cor(obs, sim, use = "complete.obs")
    R2_val <- r_val^2
    KGE_val <- KGE_calculator(obs, sim)
    
    # Adicionar os resultados ao dataframe
    resultados_metricas <- rbind(resultados_metricas, 
                                 data.frame(codigo_OMM = codigo, ME = ME_val, MAE = MAE_val, RMSE = RMSE_val, 
                                            PBIAS = PBIAS_val, NSE = NSE_val, r = r_val, R2 = R2_val, KGE = KGE_val))
  }
}

# Exibir o resultado
print(resultados_metricas)

########## GRÀFICOS ###########

# Loop para criar um gráfico para cada estação
for (codigo in names(lista_dados_limpos)) {
  
  # Verificar se a estação está nas duas listas
  if (codigo %in% names(lista_dados_CHIRPS)) {
    
    # Extrair os dados para a estação
    dados_limpos <- lista_dados_limpos[[codigo]]
    dados_chirps <- lista_dados_CHIRPS[[codigo]]
    
    # Unir os dois dataframes pelo mês
    dados_combinados <- merge(dados_limpos, dados_chirps, by = "mes", suffixes = c("_limpos", "_CHIRPS"))
    
    # Criar o gráfico
    p <- ggplot(dados_combinados, aes(x = mes, group = 1)) +
      geom_line(aes(y = precip_total_mensal, color = "Estação"), size = 1) +
      geom_line(aes(y = precip_total_mensal_CHIRPS, color = "CHIRPS"), size = 1, linetype = "dashed") +
      labs(title = paste("Estação:", codigo),
           x = "Mês/Ano",
           y = "Precipitação Total Mensal (mm)") +
      scale_color_manual(values = c("Estação" = "blue", "CHIRPS" = "red")) +
      theme_minimal() +
      theme(axis.text.x = element_blank(), # Remove os rótulos do eixo X
            axis.ticks.x = element_blank()) # Remove os ticks do eixo X    
    # Exibir o gráfico
    print(p)
  }
}




#########################################################################
######################## SALVAR RDAta - Etapa 04 ########################
#########################################################################

save.image("RData/RData_etapa4.RData")
