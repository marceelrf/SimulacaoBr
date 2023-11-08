# Função para simular uma partida com distribuição de Poisson
simular_partida <- function(tab_classificacao, time_casa, time_fora) {
    
  
  lambda_gols_marcados_casa <- tab_classificacao$GP[tab_classificacao$Time == time_casa]/tab_classificacao$J[tab_classificacao$Time == time_casa]
  #lambda_gols_sofridos_casa <- tab_classificacao$GC[tab_classificacao$Time == time_casa]/tab_classificacao$J[tab_classificacao$Time == time_casa]
  lambda_gols_marcados_fora <- tab_classificacao$GP[tab_classificacao$Time == time_fora]/tab_classificacao$J[tab_classificacao$Time == time_fora]
  #lambda_gols_sofridos_fora <- tab_classificacao$GC[tab_classificacao$Time == time_fora]/tab_classificacao$J[tab_classificacao$Time == time_fora]
  
  gols_time1 <- rpois(1, lambda_gols_marcados_casa)
  gols_time2 <- rpois(1, lambda_gols_sofridos_fora)
  resultado <- ifelse(gols_time1 > gols_time2, "V", ifelse(gols_time1 < gols_time2, "D", "E"))
  
  return(c(gols_time1, gols_time2, resultado))
}
