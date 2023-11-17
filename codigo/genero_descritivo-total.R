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
# Função para criar Tabelas 1 e 2####
tabfun <- function(dados, var_group1, var_group2) {
  dados |> 
    group_by({{var_group1}}, {{var_group2}}) |> 
    summarize(total = n()) |> 
    mutate(frequencia = round(total / sum(total) * 100, 2)) |>
    ungroup() |> 
    rename(!!paste("total", as.character(substitute(var_group2)), sep = "_") := total,
           !!paste("frequencia", as.character(substitute(var_group2)), sep = "_") := frequencia) |> 
    pivot_wider(names_from = {{var_group2}},
                values_from = matches("total|frequencia"))
}

# Cálculo por Grande Área
dados_areas <- dados |> 
  group_by(nm_grande_area_conhecimento) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) 

# Cálculo por orientador/estudantes
dados_areas_go <- tabfun(dados, nm_grande_area_conhecimento, g_orientador)
dados_areas_gd <- tabfun(dados, nm_grande_area_conhecimento, g_discente)
dados_areas_god <- tabfun(dados, nm_grande_area_conhecimento, g_oridis)

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
      summarise(across(contains(c("total", "frequencia")), sum))) |> 
      mutate(areas = coalesce(areas, "Total"))


# TABELA 1 | Grandes Áreas ####
tab1 <- tab_grande_area |> 
  gt(rowname_col = "areas") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_discente_Male, frequencia_g_discente_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_discente_Female, frequencia_g_discente_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Male, frequencia_g_orientador_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Female, frequencia_g_orientador_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_oridis_FF, frequencia_g_oridis_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_FM, frequencia_g_oridis_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MF, frequencia_g_oridis_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MM, frequencia_g_oridis_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Discente n(%)",
    columns = c(total_g_discente_Male, total_g_discente_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_g_orientador_Male, total_g_orientador_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Discente n(%)",
    columns = c(total_g_oridis_FF, total_g_oridis_FM, total_g_oridis_MF,total_g_oridis_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_g_orientador_Male = "H",
    total_g_orientador_Female = "M",
    total_g_discente_Female = "M",
    total_g_discente_Male = "H",
    total_g_oridis_FF = "M/M",
    total_g_oridis_FM = "M/H",
    total_g_oridis_MF = "H/M",
    total_g_oridis_MM = "H/H"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |> 
  tab_header(
    title = "Tabela 1: Descrição do gênero de orientadores e estudantes de teses e dissertações defendidas no Brasil de acordo com as Grandes Áreas da CAPES (1991-2021)",
    subtitle = NULL
  ) 

#Salvar
gtsave(tab1, 
       "tab1_grande-area.docx", 
       path = "dados")

# Tabela 2 | Ciências Humanas ####
dados_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "Ciências Humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao))  

# Cálculo Total
dados_humanas_total <- dados_humanas |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador/estudantes
dados_humanas_go <- tabfun(dados_humanas, nm_area_avaliacao, g_orientador)
dados_humanas_gd <- tabfun(dados_humanas, nm_area_avaliacao, g_discente)
dados_humanas_god <- tabfun(dados_humanas, nm_area_avaliacao, g_oridis)

# Agrupamento Humanas 
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
    columns = c(total_g_discente_Male, frequencia_g_discente_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_discente_Female, frequencia_g_discente_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Male, frequencia_g_orientador_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Female, frequencia_g_orientador_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_oridis_FF, frequencia_g_oridis_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_FM, frequencia_g_oridis_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MF, frequencia_g_oridis_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MM, frequencia_g_oridis_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Discente n(%)",
    columns = c(total_g_discente_Male, total_g_discente_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_g_orientador_Male, total_g_orientador_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Discente n(%)",
    columns = c(total_g_oridis_FF, total_g_oridis_FM, total_g_oridis_MF,total_g_oridis_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_g_orientador_Male = "H",
    total_g_orientador_Female = "M",
    total_g_discente_Female = "M",
    total_g_discente_Male = "H",
    total_g_oridis_FF = "M/M",
    total_g_oridis_FM = "M/H",
    total_g_oridis_MF = "H/M",
    total_g_oridis_MM = "H/H"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |> 
  tab_header(
    title = "Tabela 2: Descrição do gênero de orientadores e estudantes de teses e dissertações defendidas no Brasil nas Ciências Humanas (1991-2021)",
    subtitle = NULL
  ) 

#Salvar
gtsave(tab2, 
       "tab2_humanas.docx", 
       path = "dados")

# GRÁFICO 01 | Grandes áreas-Orientadoras-Tempo ####
graphfun <- function(dados, var_group1, var_group2, var_group3) {
  dados |> 
    group_by({{var_group1}}, {{var_group2}},{{var_group3}}) |> 
    summarize(total = n()) |> 
    mutate(frequencia = round(total / sum(total) * 100, 2)) |>
    ungroup() 
}
# Tabela Orientadora-Tempo
graf1 <- graphfun(dados, nm_grande_area_conhecimento, an_base, g_orientador)

# Avaliação 
variacao_graf1 <- graf1 |> 
  filter(g_orientador == "Female")  |> 
  group_by(nm_grande_area_conhecimento)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção
graf1 <- graf1  |> 
  left_join(variacao_graf1, by = "nm_grande_area_conhecimento")

# Salvar tabela para referência 
graf1 |>
  readr::write_csv("dados/graf1_orientadores.csv")

graf1 |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              linewidth = 1.6) +
  geom_label_repel(aes(label = paste0(scales::comma_format(decimal.mark = ",")(variacao), "%")),
                   data = filter(graf1, 
                                 an_base == 2021 & g_orientador == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 3,
                   nudge_x = 0.4) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0,70), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "",
       y = "%",
       color = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size = 18, family = "Times New Roman")) + 
  guides(color = guide_legend(ncol = 4)) +
  coord_cartesian(clip = 'off')

ggsave(
  "figs/graf1.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# GRÁFICO 02 | Grandes áreas-Discentes-Tempo ####
# Tabela Orientadora-Tempo
graf2 <- graphfun(dados, nm_grande_area_conhecimento, an_base, g_discente)

# Avaliação 
variacao_graf2 <- graf2 |> 
  filter(g_discente == "Female")  |> 
  group_by(nm_grande_area_conhecimento)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção
graf2 <- graf2  |> 
  left_join(variacao_graf2, by = "nm_grande_area_conhecimento")

# Salvar tabela para referência 
graf2 |>
  readr::write_csv("dados/graf2_discentes.csv")

# Gráfico 
graf2 |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              linewidth = 1.6) +
  geom_label_repel(aes(label = paste0(scales::comma_format(decimal.mark = ",")(variacao), "%")),
                   data = filter(graf2, 
                                 an_base == 2021 & g_discente == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 3,
                   nudge_x = 0.4) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0,80), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "",
       y = "%",
       color = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size = 18, family = "Times New Roman")) + 
  guides(color = guide_legend(ncol = 4)) +
  coord_cartesian(clip = 'off')

ggsave(
  "figs/graf2.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# GRÁFICO 03 | Orientadora vs Estudante#### 

# Cálculo por orientador
graf3_go <- graphfun(dados, 
                     nm_grande_area_conhecimento, 
                     nm_area_avaliacao, 
                     g_orientador) |> 
  filter(g_orientador == "Female")

# Cálculo por discente
graf3_gd <- graphfun(dados, 
                     nm_grande_area_conhecimento, 
                     nm_area_avaliacao, 
                     g_discente) |> 
  filter(g_discente == "Female")

graf3_gogd <- left_join(graf3_go, 
                        graf3_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

# Salvar tabela para referência 
graf3_gogd |>
  readr::write_csv("dados/graf3.csv")

# Gráfico 
graf3_gogd |> 
  ggplot(aes(x = frequencia.x,  # Orientadoras
                        y = frequencia.y)) + # Autoras
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "Mulheres orientadoras (%)",
       y = "Mulheres autoras (%)",
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
        legend.position = c(.85, .45),
        text = element_text(size = 20, family = "Times New Roman"),
        legend.title.align = 0.25) 
  
# Salvar gráfico
ggsave(
  "figs/graf3.png",
  bg = "white",
  width = 10,
  height = 10,
  dpi = 1200,
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

# Banco com os piores
lista_piores <- levels(piores_areas$nm_area_avaliacao)
dados_piores <- dados |> filter(nm_area_avaliacao %in% lista_piores)

# Cálculo Total
piores_total <- dados_piores |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por relações orientadora-autora
piores_go <- tabfun(dados_piores, nm_area_avaliacao, g_orientador)
piores_gd <- tabfun(dados_piores, nm_area_avaliacao, g_discente)
piores_god <- tabfun(dados_piores, nm_area_avaliacao, g_oridis)

# Agrupamento piores ####
piores_df <- list(piores_total,
                  piores_go,
                  piores_gd,
                  piores_god)

tab_piores <- purrr::reduce(piores_df, 
                            left_join, 
                            by = "nm_area_avaliacao") |> 
  arrange(frequencia_g_discente_Female) |>  
  rename("Áreas de Avaliação" = "nm_area_avaliacao")

# TABELA 3 | 10 piores áreas####
tab3 <- tab_piores |> 
  gt(rowname_col = "nm_area_avaliacao") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_discente_Male, frequencia_g_discente_Male), # Discentes Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_discente_Female, frequencia_g_discente_Female), # Discentes Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Male, frequencia_g_orientador_Male), # Orientadores Homens
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_orientador_Female, frequencia_g_orientador_Female), # Orientadoras Mulheres
    pattern = "{1} ({2})") |> 
  cols_merge(
    columns = c(total_g_oridis_FF, frequencia_g_oridis_FF), # Mulher-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_FM, frequencia_g_oridis_FM), # Mulher-Homem
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MF, frequencia_g_oridis_MF), # Homem-Mulher
    pattern = "{1} ({2})") |>
  cols_merge(
    columns = c(total_g_oridis_MM, frequencia_g_oridis_MM), # Homem-Homem
    pattern = "{1} ({2})") |>
  tab_spanner(
    label = "Discente n(%)",
    columns = c(total_g_discente_Male, total_g_discente_Female)) |> 
  tab_spanner(   # Títulos
    label = "Orientador(a) n(%)",  
    columns = c(total_g_orientador_Male, total_g_orientador_Female)) |>
  tab_spanner(
    label = "Orientador(a)/Discente n(%)",
    columns = c(total_g_oridis_FF, total_g_oridis_FM, total_g_oridis_MF,total_g_oridis_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_g_orientador_Male = "H",
    total_g_orientador_Female = "M",
    total_g_discente_Female = "M",
    total_g_discente_Male = "H",
    total_g_oridis_FF = "M/M",
    total_g_oridis_FM = "M/H",
    total_g_oridis_MF = "H/M",
    total_g_oridis_MM = "H/H"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  ) |> 
  tab_header(
    title = "Tabela 3: Descrição do gênero de orientadores e estudantes de teses e dissertações defendidas no Brasil nas dez áreas com menor proporção de estudantes mulheres (1991-2021)"
  )  

#Salvar
gtsave(tab3, 
       "tab3_piores.docx", 
       path = "dados")

# Gráfico 04 | 10 piores áreas - Orientador####
# Tabela piores-ano-orientador
graf4 <- graphfun(dados_piores, nm_area_avaliacao, an_base, g_orientador)

# Avaliação 
variacao_graf4 <- graf4  |> 
  filter(g_orientador == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção 
graf4 <- graf4  |> 
  left_join(variacao_graf4, by = "nm_area_avaliacao") |> 
  mutate(posicao = "Orientadoras") |> 
  rename(genero = g_orientador)

# Salvar tabela para referência 
graf4 |>
  readr::write_csv("dados/graf4_orientadores.csv")


# GRÁFICO 04 | 10 piores - Orientador####
graf4 |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              linewidth = 1.6) +
  geom_label_repel(aes(label = paste0(scales::comma_format(decimal.mark = ",")(variacao), "%")),
                   data = filter(graf4, 
                                 an_base == 2021 & g_orientador == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 3,
                   nudge_x = 0.4) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0,40), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "",
       y = "%",
       color = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size = 18, family = "Times New Roman")) + 
  guides(color = guide_legend(ncol = 4)) +
  coord_cartesian(clip = 'off')

# Salvar gráfico
ggsave(
  "figs/graf4.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# Gráfico 05 | 10 piores áreas- Estudante####
# Tabela piores-ano-autora
graf5 <- graphfun(dados_piores, nm_area_avaliacao, an_base, g_discente)

# Avaliação 
variacao_graf5 <- graf5  |> 
  filter(g_discente == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção 
graf5 <- graf5  |> 
  left_join(variacao_graf5, by = "nm_area_avaliacao") |> 
  mutate(posicao = "Autoras") |> 
  rename(genero = g_discente)

# Salvar tabela para referência 
graf5 |>
  readr::write_csv("dados/graf5_discentes.csv")

# GRÁFICO 05 | 10 piores - Orientador####
graf5 |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1, alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              linewidth = 1.6) +
  geom_label_repel(aes(label = paste0(scales::comma_format(decimal.mark = ",")(variacao), "%")),
                   data = filter(graf5, 
                                 an_base == 2021 & g_discente == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 3,
                   nudge_x = 0.4) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0,50), position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "",
       y = "%",
       color = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size = 18, family = "Times New Roman")) + 
  guides(color = guide_legend(ncol = 4)) +
  coord_cartesian(clip = 'off')

# Salvar gráfico
ggsave(
  "figs/graf5.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())


# GRÁFICO 04+5 | 10 piores - Orientadora - Autora####
graf45 <- bind_rows(graf4, graf5) 

graf45 |> 
  filter(genero == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1, alpha = 0.2) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              linewidth = 1.6) +
  geom_label_repel(aes(label = paste0(scales::comma_format(decimal.mark = ",")(variacao), "%")),
                   data = filter(graf45, 
                                 an_base == 2021 & genero == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 3,
                   nudge_x = 0.4) +
  facet_wrap(~posicao, ncol =1, scales = "free")+
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  scale_color_metro_d("full")+
  theme_classic() +
  labs(x = "",
       y = "%",
       color = "") +
  theme(legend.position = "top",
        legend.text=element_text(size=14),
        text = element_text(size = 18, family = "Times New Roman")) + 
  guides(color = guide_legend(ncol = 4)) +
  coord_cartesian(clip = 'off')

# Salvar gráfico
ggsave(
  "figs/graf45.png",
  bg = "white",
  width = 9,
  height = 10,
  dpi = 1200,
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
