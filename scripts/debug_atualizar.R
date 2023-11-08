(resultado_partida <- 
  simular_partida(tab_classificacao = tab_init,
                  time_casa = "Fortaleza",
                  time_fora = "Botafogo"))

(tab_init2 <- atualizar_classificacao(tab_init,
                                     resultado_partida,
                                     "Fortaleza",
                                     "Botafogo"))
