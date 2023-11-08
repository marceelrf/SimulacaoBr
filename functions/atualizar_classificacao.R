# Função para atualizar a tabela de classificação
atualizar_classificacao <- function(tab_classificacao, resultado_partida, time_casa, time_fora) {
  tab_classificacao <- tab_classificacao %>%
    mutate(
      GP = ifelse(Time == time_casa, GP + resultado_partida[1], GP),
      GC = ifelse(Time == time_casa, GC + resultado_partida[2], GC),
      SG = GP - GC,
      V = ifelse(Time == time_casa && resultado_partida[3] == "V", V + 1, V),
      E = ifelse(Time == time_casa && resultado_partida[3] == "E", E + 1, E),
      D = ifelse(Time == time_casa && resultado_partida[3] == "D", D + 1, D),
      P = 3 * V + E,
      Pos = rank(-P, ties.method = "min")
    )
  
  # Atualizar para o time visitante
  tab_classificacao <- tab_classificacao %>%
    mutate(
      GP = ifelse(Time == time_fora, GP + resultado_partida[2], GP),
      GC = ifelse(Time == time_fora, GC + resultado_partida[1], GC),
      SG = GP - GC,
      V = ifelse(Time == time_fora && resultado_partida[3] == "D", V + 1, V),
      E = ifelse(Time == time_fora && resultado_partida[3] == "E", E + 1, E),
      D = ifelse(Time == time_fora && resultado_partida[3] == "V", D + 1, D),
      P = 3 * V + E,
      Pos = rank(-P, ties.method = "min")
    )
  
  return(tab_classificacao)
}