# Função para atualizar a tabela de classificação
atualizar_classificacao <- function(tab_classificacao, resultado_partida, time_casa, time_fora) {

  
  tab_classificacao <- 
    tab_classificacao %>% 
    mutate(
    GP = case_when(
      Time == time_casa ~ GP + as.numeric(resultado_partida[1]),
      Time == time_fora ~ GP + as.numeric(resultado_partida[2]),
      TRUE~GP),
    GC = case_when(
      Time == time_casa ~ GC + as.numeric(resultado_partida[2]),
      Time == time_fora ~ GC + as.numeric(resultado_partida[1]),
      TRUE~GC),
    V = case_when(
      Time == time_casa & resultado_partida[3] == "V" ~ V + 1,
      Time == time_fora & resultado_partida[3] == "D" ~ V + 1,
      TRUE ~ V
    ),
    E = case_when(
      Time == time_casa & resultado_partida[3] == "E" ~ E + 1,
      Time == time_fora & resultado_partida[3] == "E" ~ E + 1,
      TRUE~E,
    ),
    D = case_when(
      Time == time_casa & resultado_partida[3] == "D" ~ D + 1,
      Time == time_fora & resultado_partida[3] == "V" ~ D + 1,
      TRUE ~ D
    ),
    SG = GP - GC,
    P = 3 * V + E,
    J = case_when(
      Time == time_casa | Time == time_fora ~ J + 1,
      TRUE ~ J
    )
  )
  
  # Ordenar a tabela de classificação com base nos critérios (Maior P, Maior V, Maior SG, Maior GP)
  tab_classificacao <- tab_classificacao %>%
    arrange(desc(P), desc(V), desc(SG), desc(GP))
  
  # Reatribuir a posição (rank) na tabela de classificação
  tab_classificacao <- tibble::rowid_to_column(
    select(tab_classificacao,-Pos), "Pos")
  
  return(tab_classificacao)
}
