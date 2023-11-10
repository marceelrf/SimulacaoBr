Date_sim <- Sys.Date()
nSims <- 1000
Results_list <- list() # Armazenar os resultados
FinalClass_list <- list() # Armazenar as classificações finais

tab_init <- read_csv2(file = "data/tabelaBr20231109.csv",
                      locale  = readr::locale(
                        encoding = "latin1"
                      ))

rodadas <- read_csv2(file = "data/rodadasFinais20231109.csv",
                     locale  = readr::locale(
                       encoding = "latin1"
                     ))


# Simulação ---------------------------------------------------------------

tictoc::tic()
for (s in 1:nSims) {
  
  tab_evolut <- tab_init
  tab_res <- NULL
  for(i in unique(rodadas$Rodada)){
    
    rodada_atual <- rodadas[rodadas$Rodada == i,] 
    # cat("Rodada: " %+% silver$bold$underline(i))
    # cat("\n")
    for (j in 1:nrow(rodada_atual)){
      
      rodada_tmp <- rodada_atual[j, ]
      time_casa <- rodada_tmp$Casa
      time_fora <- rodada_tmp$Visitante
      
      resultado_partida <- 
        simular_partida(tab_classificacao = tab_evolut,
                        time_casa = time_casa,
                        time_fora = time_fora)
      
      res_tmp <- glue("Rodada {i}: {time_casa} {resultado_partida[1]} x {resultado_partida[2]} {time_fora}")
      tab_res <- rbind(tab_res,res_tmp)
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
    # cat("Classificação após a rodada ", i, ":\n")
    # print(tab_evolut)
    cat("\n")
    cat(blue("--- FIM DA RODADA ",i ," ---"))
    cat("\n")
  }
  
  Results_list[[s]] <- tab_res
  FinalClass_list[[s]] <- tab_evolut
  
  cat(magenta$bold$italic("--- FINAL DA SIMULAÇÃO",s," ---\n"))
}
tictoc::toc()

source("scripts/pubs.R")
source("scripts/pubs_Objetivos.R")