tab_init$GP[tab_init$Time == "Fortaleza"]/tab_init$J[tab_init$Time == "Fortaleza"]

# Exemplo de como usar a função simular_partida
resultado_partida <- simular_partida(tab_classificacao = tab_init,
                                     time_casa = "Fortaleza",time_fora =  "Botafogo")
cat("Gols Time A: ", resultado_partida[1], "\n")
cat("Gols Time B: ", resultado_partida[2], "\n")
cat("Resultado: ", resultado_partida[3], "\n")
