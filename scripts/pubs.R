(
  Time_logo <- c("https://ssl.gstatic.com/onebox/media/sports/logos/xE2RajzsCEoen1wz8g8rhg_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/9LkdBR4L5plovKM8eIy7nQ_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/q9fhEsgpuyRq58OgmSndcQ_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/nIdbR6qIUDyZUBO9vojSPw_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/KLDWYp-H8CAOT9H_JgizRg_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/lMyw2zn1Z4cdkaxKJWnsQw_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/tCMSqgXVHROpdCpQhzTo1g_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/LaFlBADLmsjHfGTehCYtuA_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/Tcv9X__nIh-6wFNJPMwIXQ_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/j6U8Rgt_6yyf0Egs9nREXw_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/orE554NToSkH6nuwofe7Yg_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/fCMxMMDF2AZPU7LzYKSlig_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/me10ephzRxdj45zVq1Risg_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/gb8bo2x00XsbvsVp9nGniA_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/Ku-73v_TW9kpex-IEGb0ZA_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/OWVFKuHrQuf4q2Wk0hEmSA_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/7spurne-xDt2p6C0imYYNA_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/VHdNOT6wWOw_vJ38GMjMzg_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/4w2Z97Hf9CSOqICK3a8AxQ_96x96.png",
                 "https://ssl.gstatic.com/onebox/media/sports/logos/hHwT8LwRmYCAGxQ-STLxYA_96x96.png")
)

fn_Camp <-function(team){
  Time_tmp <- map(FinalClass_list, ~CheckarTimeCampeao(.x,team)) %>% 
    unlist()
  
  sum(Time_tmp == "Sim")/length(Time_tmp)
}

fn_Reb <- function(team){
  Time_tmp <- map(FinalClass_list, ~CheckarTimeRebaixado(.x,team)) %>% 
    unlist()
  
  sum(Time_tmp == "Sim")/length(Time_tmp)
}


Tab_pub <- tibble(Time = unique(rodadas$Casa)) %>% 
  arrange(Time) %>% 
  mutate(Logo = Time_logo) %>% 
  group_by(Time) %>% 
  mutate(
         Rebaixado = fn_Reb(Time),
         Campeao = fn_Camp(Time)
         )

(
publish <- Tab_pub %>%
    mutate(across(where(is.numeric),
                  .fns = \(x) x)) %>%
    ungroup() %>%
    mutate(Logo = Time_logo) %>% 
    gt() %>%
    tab_header(title = md(glue("**Brasileirão Séria A : {Date_sim}**")),
             subtitle = "@marceelrf") %>%
    fmt_percent(columns = c(Rebaixado,Campeao)) %>%
    gtExtras::gt_img_rows(columns = Logo, height = 20) %>%
    gt::cols_move(Time,Logo) %>% 
    cols_label(Logo = "",
             Rebaixado= md("**Rebaixamento (%)**"),
             Campeao = md("**Campeão (%)**"),
             Time = md("**Time**")) %>% 
    tab_source_note(md(glue("*Número de simulações: {nSims}*"))) %>% 
    tab_style(
      style = cell_text(
        size = "small",
        align = "right",
        v_align = "bottom"),
      locations = cells_source_notes())
)
gt::gtsave(publish,
           filename = glue("Output/Sim_{nSims}_{Date_sim}.png"), vwidth = 1500)
gt::gtsave(publish,
           filename = glue("Output/Sim_{nSims}_{Date_sim}.html"))
gt::gtsave(publish,
           filename = glue("Output/Sim_{nSims}_{Date_sim}.pdf"))
