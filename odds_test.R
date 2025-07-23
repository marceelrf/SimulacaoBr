Odds_tmp <- Results_list %>% 
  map_dfr(.f = \(x) x %>% 
        filter(Rodada == 30,Casa == "Fortaleza") %>% 
        mutate(Resultado = case_when(
          Casa_gols > Fora_gols ~ Casa,
          Casa_gols < Fora_gols ~ Fora,
          TRUE ~ "Empate"
        )),
      .progress = TRUE
      )

rev(c(1.96, 3.02, 4.46)) * table(Odds_tmp$Resultado)/1000
