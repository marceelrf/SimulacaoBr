Rebaixados_corte <- function(x) {
  
  x %>% 
    filter(Pos == 17) %>% 
    pull(P)
}

Campeao_corte <- function(x) {
  
  x %>% 
    filter(Pos == 2) %>% 
    pull(P)
}

Lib_corte <- function(x) {
  
  x %>% 
    filter(Pos == 7) %>% 
    pull(P)
}

Lib_corte_dir <- function(x) {
  
  x %>% 
    filter(Pos == 5) %>% 
    pull(P)
}

CheckarTimeRebaixado <- function(x, time){
  tmp <- x %>% 
    filter(Pos >= 17) %>% 
    mutate(Reb = case_when(
      Time == time ~ "Sim",
      TRUE ~ "Não"
      )
    ) %>% 
    pull(Reb)
  

  ifelse("Sim" %in% tmp, "Sim", "Não")
}
CheckarTimeCampeao <- function(x, time){
  tmp <- x %>% 
    filter(Pos == 1) %>% 
    mutate(Camp = case_when(
      Time == time ~ "Sim",
      TRUE ~ "Não"
    )
    ) %>% 
    pull(Camp)
  
  
  ifelse("Sim" %in% tmp, "Sim", "Não")
}

TimeCampeao <- function(x) {
  x %>% 
    filter(Pos == 1) %>% 
    pull(Time)
}

CheckarTimeCampeao_prop <- function(x, time){
  tmp <- x %>% 
    filter(Pos == 1) %>% 
    mutate(Camp = case_when(
      Time == time ~ "Sim",
      TRUE ~ "Não"
      )
    ) %>% 
    pull(Camp)
  
  
  ifelse("Sim" %in% tmp, "Sim", "Não")
}
# Apply -------------------------------------------------------------------


map(FinalClass_list,Rebaixados_corte) %>% 
  unlist() %>% 
  table()
map(FinalClass_list,Campeao_corte) %>% 
  unlist()
map(FinalClass_list, ~CheckarTimeRebaixado(.x,"Goiás")) %>% 
  unlist() %>% 
  table()
map(FinalClass_list, ~CheckarTimeCampeao(.x,"Palmeiras")) %>% 
  unlist() %>% 
  table()
map(FinalClass_list, ~CheckarTimeCampeao(.x,"Botafogo")) %>% 
  unlist() %>% 
  table()
map(FinalClass_list, ~CheckarTimeCampeao(.x,"Bragantino")) %>% 
  unlist() %>% 
  table()
map(FinalClass_list, TimeCampeao) %>% 
  unlist() %>% 
  table()/300

map(FinalClass_list,Campeao_corte) %>% 
  unlist() %>% 
  table()


Tema_tmp <- map(FinalClass_list, ~CheckarTimeRebaixado(.x,"Bahia")) %>% 
  unlist()


fn_Reb <- function(team){
  Time_tmp <- map(FinalClass_list, ~CheckarTimeRebaixado(.x,team)) %>% 
    unlist()
  
  sum(Time_tmp == "Sim")/length(Time_tmp)
}

fn_Camp <- function(team){
  Time_tmp <- map(FinalClass_list, ~CheckarTimeCampeao(.x,team)) %>% 
    unlist()
  
  sum(Time_tmp == "Sim")/length(Time_tmp)
}

