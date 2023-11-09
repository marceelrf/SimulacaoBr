# Extrair os dados --------------------------------------------------------

campeao_vec <- 
  map(FinalClass_list,Campeao_corte) %>% 
  unlist()

libertadores_dir_vec <-
  map(FinalClass_list,Lib_corte_dir) %>% 
  unlist()

libertadores_vec <-
  map(FinalClass_list,Lib_corte) %>% 
  unlist()

rebaixamento_vec <-
  map(FinalClass_list,Rebaixados_corte) %>% 
  unlist()

# Table -------------------------------------------------------------------


Objetivos <- tibble(Campeao = campeao_vec,
       Lib_dir = libertadores_dir_vec,
       Lib = libertadores_vec,
       Reb = rebaixamento_vec) %>% 
  pivot_longer(cols = everything(),names_to = "vars",values_to = "Pontos") %>% 
  mutate(vars = factor(vars,levels = c("Campeao","Lib_dir",
                                     "Lib","Reb"))) 


(Objetivos_summary <- 
  Objetivos %>% 
  group_by(vars) %>% 
  summarise(min = min(Pontos),
            mean = mean(Pontos),
            median = median(Pontos),
            max = max(Pontos),
            
            pontos_data = list(Pontos),
            .groups = "drop"))
(
plot_Sum <- Objetivos_summary %>% 
  mutate(dumb = min,
         dumb2 = max,
         icon = case_when(
           vars == "Campeao" ~ "trophy",
           vars == "Lib_dir" ~ "earth-americas",
           vars == "Lib" ~ "plane",
           TRUE ~ "down-long"
         )) %>% 
  mutate(vars = case_when(
    vars == "Campeao" ~ "Campeão",
    vars == "Lib_dir" ~ "Libertadores direto",
    vars == "Lib" ~ "Libertadores",
    TRUE ~ "Rebaixamento"
  )) %>%
  select(-pontos_data,-min,-max) %>% 
  gt() %>% 
  gtExtras::gt_plt_dumbbell(col1 = dumb,col2 = dumb2,
                            palette = c("#046A38","firebrick4","grey50")) %>%
  fmt_number(columns = median, decimals = 0) %>% 
  fmt_number(columns = mean, decimals = 2) %>% 
  gtExtras::gt_fa_column(icon,palette = c("trophy" = "#f1c232",
                                          "earth-americas" = "#034040",
                                          "plane" = "#8e7cc3",
                                          "down-long" = "#f44336")) %>% 
  gt::cols_move(icon,after = vars) %>% 
  cols_label(vars = "",
             icon = "",
             mean = md("**Média**"),
             median = md("**50% dos casos**"),
             dumb = md("**Min**-**Máx**")) %>% 
  tab_style(
    style = cell_text(align = "center",
                      v_align = "middle"),
    locations = cells_column_labels()
    ) %>% 
  tab_header(title = md(glue("**Pontos necessários** : {Sys.Date()}")),
             subtitle = "@marceelrf") %>% 
  tab_source_note(md(glue("*Número de simulações: {nSims}*"))) %>% 
  tab_style(
    style = cell_text(
      size = "small",
      align = "right",
      v_align = "bottom"),
    locations = cells_source_notes())
)

gt::gtsave(plot_Sum,
           filename = glue("Output/Resumos_{nSims}_{Sys.Date()}.png"), vwidth = 1500)


# Objetivos_summary %>% 
#   mutate(dumb = max) %>% 
#   select(-pontos_data) %>% 
#   gt() %>% 
#   gtExtras::gt_plt_bullet(column = dumb,target = median,
#                           width = 20) %>%
#   fmt_number(columns = min:max, decimals = 0)
# 
# gtExtras::gt_plt_summary(tibble(Campeao = campeao_vec,
#                       Lib = libertadores_vec,
#                       Lib_dir = libertadores_dir_vec,
#                       Reb = rebaixamento_vec))
