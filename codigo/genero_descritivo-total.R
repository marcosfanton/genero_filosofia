####Pacotes####
library(tidyverse)
library(here)
library(gt) # Tabelas
library(see) # Cores retrôs no gráfico
library(ggtext) # Texto em gráficos
library(ggrepel) # Labels em gráficos
library(ggforce)
library(ggridges)
library(extrafont)
library(ggtext)
library(ggstream)
library(stringi)
library(ggsci) # Paleta cores
library(geobr)
library(sf)

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
graf1_go <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

graf1_go |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 2.5) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(limits = c(0,70), position = "right") +
  scale_color_metro_d()+
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
  "figs/graf1.tiff",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# GRÁFICO 02 | Orientadora vs Estudante#### 

# Cálculo por orientador
graf2_go <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) |> 
  filter(g_orientador == "Female")

# Cálculo por discente
graf2_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  filter(g_discente == "Female") 

graf2_gogd <- left_join(graf2_go, 
                        graf2_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

graf2_gogd |> ggplot(aes(x = frequencia_o, 
                        y = frequencia_d)) +
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4.5) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_metro_d()+
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
  "figs/graf2.tiff",
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

# TABELA 3 | 10 piores ####
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

# Gráfico 03 | 10 piores - Orientador####
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

# GRÁFICO 03 | 10 piores - Orientador####

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
  "figs/figs_tiff/graf3.tiff",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# Gráfico 04 | 10 piores - Estudante####
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

# GRÁFICO 04 | 10 piores - Estudante####

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
  "figs/figs_tiff/graf4.tiff",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())








#***************** ANTIGO*********************************############################


# Tabela 3 | Universidades####
# Organização e extração dos 10 piores cursos
# Cálculo Total
dados_ies <- dadosfi |> 
  group_by(nm_entidade_ensino) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  slice_max(total, n = 15)

# Lista para filtrar os dados
lista_ies <- levels(dados_ies$nm_entidade_ensino)

# Cálculo por discente
dados_ies_d <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  ungroup()

dados_ies_d <- dados_ies_d |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d)) 

# Cálculo por orientador
dados_ies_o <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_ies_o <- dados_ies_o |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o)) 

# Cálculo por Orientador-Discente
dados_ies_god <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_ies_god <- dados_ies_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 3 | Agrupamento em um dataframe ####
lista_ies_df <- list(dados_ies, 
                     dados_ies_o, 
                     dados_ies_d, 
                     dados_ies_god)

tab_ies <- purrr::reduce(lista_ies_df, 
                         left_join, 
                         by = "nm_entidade_ensino") |> 
  mutate(nm_entidade_ensino = stringr::str_to_title(nm_entidade_ensino)) |> 
  mutate_all(~replace_na(.,0)) |> 
  rename("IES" = "nm_entidade_ensino") 

# TABELA 3 ####
tab_ies |> 
  gt() |> 
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2}%)") |>
  tab_spanner(   # Títulos
    label = "Orientador(a)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Discente",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Orientador(a)/Discente",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "Homem",
    total_o_Female = "Mulher",
    total_d_Female = "Mulher",
    total_d_Male = "Homem",
    total_od_FF = "Mulher/Mulher",
    total_od_FM = "Mulher/Homem",
    total_od_MF = "Homem/Mulher",
    total_od_MM = "Homem/Homem",
  ) |> 
  tab_header(
    title = "Tabela 3. Descrição do gênero de orientadores e discentes das teses e dissertações defendidas nas 15 IES mais produtivas no Brasil entre 1991-2021") |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# ******FILOSOFIA******#### 
# Uso do catálogo específico da Filosofia | n: 12525
dadosfi <- readr::read_csv("dados/catalogo.csv") |> 
  filter(an_base >= 1991) |> # Exclusão - 172 observações (n: 12353)
  drop_na(g_oridis) # Exclusão - 408 (n: 11945)

# Transforma variáveis de interesse em categóricas
fatores <- c("nm_grau_academico",
             "nm_entidade_ensino",
             "nm_regiao", 
             "sg_uf_ies", 
             "g_orientador", 
             "g_discente", 
             "g_oridis")

dadosfi <- dadosfi  |> 
  mutate(across(all_of(fatores), as.factor))

# Gráfico 4 |  Filosofia Evolução | Grau Acadêmico | Linha ####
evol_fi_total <-  dadosfi |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  mutate(nm_grau_academico = stri_trans_totitle(nm_grau_academico))

evol_fi_total |> 
  ggplot(aes(x = an_base, 
             y = n)) +
  geom_line(aes(color = nm_grau_academico), linewidth = 2)+
  stat_summary(aes(color = "Total"), fun = sum, geom ='line', linewidth = 2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  scale_color_d3() +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação em Filosofia",
       subtitle = "Teses e Dissertações defendidas entre 1991-2021 | n: 11945 trabalhos",
       caption = "Elaboração: Os autores | Dados: CAPES", 
       x = "",
       y = "",
       color = "Grau Acadêmico") +
  theme(legend.position = "top") +
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

# Gráfico 04 | Filosofia | Relação Professor vs Discente | Barra ####
dadosfi |> 
  ggplot(aes(x = an_base, 
             fill = factor(g_oridis, levels = c("FF", "FM", "MF", "MM")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  theme_classic() +
  scale_fill_d3() +
  scale_x_continuous(limits = c(1990, 2021), expand = c(0, 0)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(legend.position = "top") 

#Gráfico | Total | PROFESSOR - LINHA #### 
dadosfi |> 
  ggplot(aes(x = an_base, color = nm_grau_academico)) +
  geom_line(stat = "count", linewidth = 1.2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 3)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação",
       subtitle = " Trabalhos orientados por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1991 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off') 

#Gráfico Total por Gênero do ALUNO - BARRA
dados |> 
  drop_na(g_discente) |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1987, 2021), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35))

#Gráficos por GRANDE ÁREA####
#Gráfico Grande área por pesquisador
nomes_grandearea <- c("ciencias agrarias" = "Ciências Agrárias",
                      "ciencias biologicas" = "Ciências Biológicas",
                      "ciencias da saude" = "Ciências da Saúde",
                      "ciencias exatas e da terra" = "Ciências Exatas e da Terra",
                      "ciencias humanas" = "Ciências Humanas",
                      "ciencias sociais aplicadas" = "Ciências Sociais Aplicadas",
                      "engenharias" = "Engenharias",
                      "linguistica letras e artes" = "Linguística, Letras e Artes",
                      "multidisciplinar" = "Multidisciplinar")

# Gráfico de grande área PROFESSOR - LINHA
dados |> 
  ggplot(aes(x = an_base, group = g_orientador, color = g_orientador)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação por Grande Área",
       subtitle = "Trabalhos orientados por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1990 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 35)) + 
  coord_cartesian(clip = 'off') 

# Gráfico Grande Área por Gênero do PROFESSOR - BARRA
dados |> 
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1991, 2022)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) 

# Gráfico Grande área por ALUNO - LINHA
dados |> 
  ggplot(aes(x = an_base, group = g_discente, color = g_discente)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2022), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação por Grande Área",
       subtitle = "Trabalhos defendidos por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1991 e 2021",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off') 


#Gráfico Grande Área por Gênero do ALUNO - BARRA
dados |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Grande Área",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) 

#10 piores de desigualdade para orientador####

#Gráfico PIORES ÁREAS  por Gênero do PROFESSOR - BARRA
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "10 Áreas de Avaliação com Alta Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:30pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:30pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao, ncol = 2) 

#Gráfico PIORES ÁREAS por gênero do ALUNO - LINHA
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "10 Áreas de Avaliação com Alta Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos defendidos por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao, ncol = 2) 

#Gráfico HUMANAS por Gênero do PROFESSOR - BARRA
dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |>  
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Ciências Humanas",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0.02, 0.02)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao) 


# Gráfico | Relação Professor-Aluno####
# Total
dados |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# Relação Professor-Aluno Grandes Áreas
dados |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15),
        text = element_text(size = 30)) +
  facet_wrap(~nm_grande_area_conhecimento) 

# Relação Professor-Aluno Humanidades
dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |>  
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels=NULL, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        text = element_text(size = 25),
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15)) +
  facet_wrap(~nm_area_avaliacao) 

# Relação Professor-Aluno 10 Piores
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1990-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels=NULL, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        text = element_text(size = 25),
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15)) +
  facet_wrap(~nm_area_avaliacao) 


# Cálculo Razão de Prevalência #### 
dadosfil <- read.csv("dados/catalogo.csv") |> # Uso do banco da filosofia
  drop_na(g_oridis) # Eliminação de 

matriz <- dadosfil |>  
  filter(between(an_base, 2011,2021)) |> 
  tabyl(g_orientador, g_discente) |> 
  adorn_totals(c("row", "col"))

dat.v <- matrix(c(470,1537,936,4156), ncol =2)

resultado <- epi.2by2(dat = dat.v, method = "cross.sectional",
                      conf.level = 0.95, units = 100, outcome = "as.columns")


# Gráfico | Evol Prop. Orientadora | Grandes áreas####
dados_evol_go <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_go |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1.2, alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 9)) +
  labs(title = "Evolução da proporção de trabalhos orientados por mulheres na Pós-Graduação por Grande Área",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

# Gráfico | Evol Prop. Discente | Humanas####
dados_evol_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "Ciências Humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao)) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_humanas |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  labs(title = "Evolução da proporção de trabalhos defendidos por mulheres na Pós-Graduação nas Ciências Humanas",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "%",
       color = "Ciências Humanas") +
  scale_color_d3() +
  theme_classic() +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

# Gráfico 03.A | Tendência Prop. Docente | 10 piores####
piores_go <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Avaliação aumento-diminuição em dados percentuais
avaliacao_go <- piores_go  |> 
  filter(g_orientador == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção no dataframe
piores_go <- piores_go  |> 
  left_join(avaliacao_go, by = "nm_area_avaliacao")

# Gráfico
piores_go |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 2, alpha = 0.3) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
                   data = filter(piores_go, an_base == 2021 & g_orientador == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 4,
                   nudge_x = 0.2) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 30), position = "right") +
  labs(title = "Gráfico 03.A. Tendência da prevalência de trabalhos orientados por mulheres (1991-2021)",
       subtitle = "Com a variação percentual entre 1991 e 2021",
       x = "",
       y = "%",
       color = "Área de Avaliação") +
  scale_color_d3() +
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.position = "top") + 
  coord_cartesian(clip = 'off')

# Gráfico 03.B | Tendência Prop. Discente | 10 piores####
piores_gd <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Avaliação aumento-diminuição em dados percentuais
avaliacao_gd <- piores_gd  |> 
  filter(g_discente == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção no dataframe
piores_gd <- piores_gd  |> 
  left_join(avaliacao_gd, by = "nm_area_avaliacao")

piores_gd |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 2, alpha = 0.3) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
                   data = filter(piores_gd, an_base == 2021 & g_discente == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 4,
                   nudge_x = 0.1) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 60), position = "right") +
  labs(title = "Gráfico 03.B. Tendência da prevalência de trabalhos defendidos por mulheres (1991-2021)",
       subtitle = "Com a variação percentual entre 1991 e 2021",
       x = "",
       y = "%",
       color = "Área de Avaliação") +
  scale_color_d3() +
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.position = "top") + 
  coord_cartesian(clip = 'off')

ggsave(
  "figs/genero_graf03A_piorgo.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 900,
  plot = last_plot())

# Gráfico 01 | Prevalência mulher Docente vs Discente | Áreas e Grande Área #### 
# Cálculo por orientador
dados_go <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) |> 
  filter(g_orientador == "Female")

# Cálculo por discente
dados_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  filter(g_discente == "Female") 

dados_god <- left_join(dados_go, 
                       dados_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

dados_god |> ggplot(aes(x = frequencia_o, 
                        y = frequencia_d)) +
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4.5) +
  ggrepel::geom_text_repel(aes(label = nm_area_avaliacao,
                               color = nm_grande_area_conhecimento),
                           show.legend = FALSE,
                           min.segment.length = .7,
                           box.padding = 0.3,
                           size = 5,
                           nudge_x = 0.1,
                           nudge_y = 1.6) +
  labs(title = "Gráfico 01. Prevalência de mulheres orientadoras e mulheres discentes entre as áreas de avaliação da CAPES (1991-2021)", 
       x = "Prevalência de mulheres orientadoras (%)",
       y = "Prevalência de mulheres discentes (%)",
       color = "Grande Área") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_d3() +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme_light() +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = c(.85, .38),
        text = element_text(size = 15),
        legend.title.align = 0.25,
        legend.background = element_rect(color = "black", 
                                         linewidth = 0.5, 
                                         linetype = "solid")) 
ggsave(
  "figs/genero_graf04_goridis.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 900,
  plot = last_plot())


##*****************WASTED**********************************#########
# Gráfico | Evol Prop. Discente | Grandes áreas####
dados_evol_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_discente) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_gd |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  labs(title = "Evolução da proporção de trabalhos defendidos por mulheres na Pós-Graduação por Grande Área",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

#RASCUNHOS####
