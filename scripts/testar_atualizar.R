tab_evolut <- tab_init
for(i in unique(rodadas$Rodada)){
  
  rodada_atual <- rodadas[rodadas$Rodada == i,] 
  cat("Rodada: " %+% silver$bold$underline(i))
  cat("\n")
  for (j in 1:nrow(rodada_atual)){
    
    rodada_tmp <- rodada_atual[j, ]
    time_casa <- rodada_tmp$Casa
    time_fora <- rodada_tmp$Visitante
    
    resultado_partida <- 
      simular_partida(tab_classificacao = tab_evolut,
                                         time_casa = time_casa,
                                         time_fora = time_fora)
    
    
    # cat(red$bold$underline(time_casa) %+% " X " %+% red$bold$underline(time_fora))
    # cat("\n")
    # cat("----\n")
    # cat(resultado_partida)
    # cat("\n---\n")
    # cat("\n")
    
    tab_evolut <- atualizar_classificacao(tab_evolut,
                                        resultado_partida,
                                        time_casa,
                                        time_fora)
    
  }
  # Imprimir a classificação após cada rodada (opcional)
  cat("Classificação após a rodada ", i, ":\n")
  print(tab_evolut)
  cat("\n")
  cat(blue("--- FIM DA RODADA ---"))
  cat("\n")
}
