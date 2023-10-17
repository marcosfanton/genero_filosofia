####Pacotes####
library(tidyverse)
library(here)
library(gt) # Tabelas
library(see) # Palesta cores retrôs no gráfico
library(ggtext) # Texto em gráficos
library(ggrepel) # Labels em gráficos

# Banco (ver genero_limpeza.R) 
dados <- read.csv("dados/bancos/catalogo9121_raw.csv") #n: 1117944

# Transformação de variáveis de interesse em variáveis categóricas
fatores <- c("nm_grande_area_conhecimento", 
             "nm_area_avaliacao", 
             "nm_grau_academico",
             "nm_entidade_ensino",
             "nm_regiao", 
             "sg_uf_ies", 
             "g_orientador", 
             "g_discente", 
             "g_oridis")
dados <- dados  |> 
  mutate(across(all_of(fatores), as.factor))

# Tabela 1 | Grandes Áreas ####
# Cálculo por Grande Área#### **NOTA: CRIAR FUNÇÃO***
dados_areas <- dados |> 
  group_by(nm_grande_area_conhecimento) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) 

# Cálculo por orientador
dados_areas_go <- dados |> 
  group_by(nm_grande_area_conhecimento, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_areas_go <- dados_areas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_areas_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_areas_gd <- dados_areas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por orientador-orientando
dados_areas_god <- dados |> 
  group_by(nm_grande_area_conhecimento, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_areas_god <- dados_areas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento grandes áreas####
lista_grande_area <- list(dados_areas, 
                          dados_areas_go, 
                          dados_areas_gd, 
                          dados_areas_god)

tab_grande_area <- purrr::reduce(lista_grande_area, 
                                 left_join, 
                                 by = "nm_grande_area_conhecimento") |> 
  rename("areas" = "nm_grande_area_conhecimento")

tab_grande_area <- tab_grande_area  |> 
  bind_rows(
    tab_grande_area  |> 
      summarise(across(contains("total"), sum)) |> 
      mutate(areas = "Total")
  )

# TABELA 1 | Grandes Áreas ####

tab1 <- tab_grande_area |> 
  gt(rowname_col = "areas") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Discente n(%)",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Discente n(%)",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "H",
    total_o_Female = "M",
    total_d_Female = "M",
    total_d_Male = "H",
    total_od_FF = "M/M",
    total_od_FM = "M/H",
    total_od_MF = "H/M",
    total_od_MM = "H/H"
  ) |> 
  tab_header(
    title = "Tabela 1. Descrição do gênero de orientadores e discentes das teses e dissertações defendidas no Brasil de acordo com as Grandes Áreas da CAPES (1991-2021)"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") |> 
  opt_table_font(
    font = "Times New Roman") |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  )
#Salvar
gtsave(tab1, 
       "tab1_grande-area.docx", 
       path = "figs",
       vwidth = 1400,
       vheight = 1700)

# Tabela 2 | Ciências Humanas ####
dados_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "Ciências Humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao))  

# Cálculo Total
dados_humanas_total <- dados_humanas |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
dados_humanas_go <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_humanas_go <- dados_humanas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_humanas_gd <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_humanas_gd <- dados_humanas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por Orientador-Discente
dados_humanas_god <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_humanas_god <- dados_humanas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento Humanas ####
lista_humanas <- list(dados_humanas_total, 
                      dados_humanas_go, 
                      dados_humanas_gd, 
                      dados_humanas_god)

tab_humanas <- purrr::reduce(lista_humanas, 
                             left_join, 
                             by = "nm_area_avaliacao")

# TABELA 2 | Ciências Humanas####                                                             
tab2 <- tab_humanas |> 
  gt(rowname_col = "nm_area_avaliacao") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Estudante n(%)",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Estudante n(%)",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "H",
    total_o_Female = "M",
    total_d_Female = "M",
    total_d_Male = "H",
    total_od_FF = "M/M",
    total_od_FM = "M/H",
    total_od_MF = "H/M",
    total_od_MM = "H/H"
  ) |>  
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") |> 
  opt_table_font(
    font = "Times New Roman") 

#Salvar
gtsave(tab2, 
       "tab2_humanas.docx", 
       path = "figs",
       vwidth = 1400,
       vheight = 1700)

# GRÁFICO 01 | Grandes áreas-Orientadoras-Tempo ####
graf1 <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  filter(g_orientador == "Female")

# Salvar tabela para referência 
graf1 |>
  readr::write_csv("dados/graf1_orientadores.csv")

graf1 |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 2.5) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0,70), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(title = "",
       caption = "", 
       x = "Ano",
       y = "%") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=25),
        text = element_text(size = 36, family = "Times New Roman")) +
  guides(color = guide_legend(ncol = 4))

ggsave(
  "figs/graf1.png",
  bg = "white",
  width = 16,
  height = 12,
  dpi = 300,
  plot = last_plot())

# GRÁFICO 02 | Grandes áreas-Discentes-Tempo ####
graf2 <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  filter(g_discente == "Female")

# Salvar tabela para referência 
graf2 |>
  readr::write_csv("dados/graf2_discentes.csv")


graf2 |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 2.5) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0,80), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(title = "",
       caption = "", 
       x = "Ano",
       y = "%") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=25),
        text = element_text(size = 36, family = "Times New Roman")) +
  guides(color = guide_legend(ncol = 4))

ggsave(
  "figs/figs_tiff/graf2.tiff",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# GRÁFICO 03 | Orientadora vs Estudante#### 

# Cálculo por orientador
graf3_go <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) |> 
  filter(g_orientador == "Female")

# Cálculo por discente
graf3_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  filter(g_discente == "Female") 

graf3_gogd <- left_join(graf3_go, 
                        graf3_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

# Salvar tabela para referência 
graf3_gogd |>
  readr::write_csv("dados/graf3.csv")

# Gráfico 
graf3_gogd |> ggplot(aes(x = frequencia_o, 
                        y = frequencia_d)) +
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4.5) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "Mulheres orientadoras (%)",
       y = "Mulheres estudantes (%)",
       color = "Grande Área") +
  ggrepel::geom_text_repel(aes(label = nm_area_avaliacao,
                               color = nm_grande_area_conhecimento),
                           show.legend = FALSE,
                           min.segment.length = .7,
                           box.padding = 0.3,
                           size = 5,
                           nudge_x = 0.1,
                           nudge_y = 1.6) +
  theme(legend.title = element_blank(),
        legend.position = c(.85, .38),
        text = element_text(size = 30, family = "Times New Roman"),
        legend.title.align = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=8))) 
  
# Salvar gráfico
ggsave(
  "figs/graf3.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# Tabela 3 | 10 piores áreas - Estudantes#### 

# Cálculo por estudante das 10 piores áreas
piores_areas <- dados |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  ungroup() |> 
  filter(g_discente == "Female")  |> 
  filter(nm_area_avaliacao != "Materiais") |> 
  slice_min(frequencia_d, n = 10) |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao))

# Lista de áreas para filtrar os dados
lista_piores <- levels(piores_areas$nm_area_avaliacao)

# Cálculo Total
piores_total <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por docente
piores_go <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

piores_go <- piores_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
piores_gd <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

piores_gd <- piores_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d)) 

# Cálculo por Orientador-Discente
piores_god <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

piores_god <- piores_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento piores ####
piores_df <- list(piores_total,
                  piores_go,
                  piores_gd,
                  piores_god)

tab_piores <- purrr::reduce(piores_df, 
                            left_join, 
                            by = "nm_area_avaliacao") |> 
  arrange(frequencia_d_Female) |>  
  rename("Áreas de Avaliação" = "nm_area_avaliacao")

# TABELA 3 | 10 piores áreas####
tab3 <- tab_piores |> 
  gt(rowname_col = "nm_area_avaliacao") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Estudante n(%)",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Estudante n(%)",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "H",
    total_o_Female = "M",
    total_d_Female = "M",
    total_d_Male = "H",
    total_od_FF = "M/M",
    total_od_FM = "M/H",
    total_od_MF = "H/M",
    total_od_MM = "H/H"
  ) |>  
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") |> 
  opt_table_font(
    font = "Times New Roman") 

#Salvar
gtsave(tab3, 
       "tab3_piores.docx", 
       path = "figs",
       vwidth = 1400,
       vheight = 1700)

# Gráfico 04 | 10 piores áreas - Orientador####
# Tabela piores-ano-orientador
piores_evol_o <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Avaliação aumento-diminuição em dados percentuais
piores_tendencia_o <- piores_evol_o  |> 
  filter(g_orientador == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção 
piores_evol_o <- piores_evol_o  |> 
  left_join(piores_tendencia_o, by = "nm_area_avaliacao")

# Salvar tabela para referência 
piores_evol_o |>
  readr::write_csv("dados/graf4.csv")


# GRÁFICO 04 | 10 piores - Orientador####
piores_evol_o |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1.5, alpha = 0.2) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2.5) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
                   data = filter(piores_evol_o, an_base == 2021 & g_orientador == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 5,
                   nudge_x = 0.2) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 40), position = "right") +
  labs(x = "",
       y = "%",
       color = "Área de Avaliação") +
  scale_color_metro_d("full")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=25),
        text = element_text(size = 36, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')

# Salvar gráfico
ggsave(
  "figs/graf4.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# Gráfico 05 | 10 piores áreas- Estudante####
# Tabela piores-ano-estudante
piores_evol_d <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Avaliação aumento-diminuição em dados percentuais
piores_tendencia_d <- piores_evol_d  |> 
  filter(g_discente == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção 
piores_evol_d <- piores_evol_d  |> 
  left_join(piores_tendencia_d, by = "nm_area_avaliacao")

# Salvar tabela para referência 
piores_evol_d |>
  readr::write_csv("dados/graf5.csv")

# GRÁFICO 05 | 10 piores - Estudante####

piores_evol_d |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1.5, alpha = 0.2) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2.5) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
                   data = filter(piores_evol_d, an_base == 2021 & g_discente == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 5,
                   nudge_x = 0.2) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 50), position = "right") +
  labs(x = "",
       y = "%",
       color = "Área de Avaliação") +
  scale_color_metro_d("full")+
  theme_classic()+
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=25),
        text = element_text(size = 36, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')

# Salvar gráfico
ggsave(
  "figs/graf5.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# SUPLEMENTAR####
# TOTAL GÊNEROS####
# Cálculo por orientador
dados_go <- dados |> 
  group_by(g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por discente
dados_gd <- dados |> 
  group_by(g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador-orientando
dados_god <- dados |> 
  group_by(g_oridis) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Grandes áreas-Estudantes-Tempo ####
evo_areas_d <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))
