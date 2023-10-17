####Pacotes####
library(tidyverse)
library(here)
library(gt) # Tabelas
library(see) # Cores retrôs no gráfico
library(ggtext) # Texto em gráficos
library(ggrepel) # Labels em gráficos
library(stringi) # Manipulação de texto


# Banco  ####
#O pacote genderBR não consegue atribuir gênero a alguns nomes de orientadores. 
#Para não enviesar a amostra (excluir todos trabalhos orientados por determinador professor),
#iremos atribuir o gênero manualmente para as observações que não foram definidas.
#Isso foi feito exclusivamente para a área de filosofia. 
#O banco origina-se de outro projeto, que pode ser encontrado no seguinte endereço:
#https://github.com/marcosfanton/stm_filobr - Ver codigo/stm_limpeza.R 

dadosfi <- read.csv("dados/catalogo.csv") |>  #n: 12525
  filter(an_base >= 1991) |> # Exclusão - 172 observações (n: 12353)
  drop_na(g_oridis)  # Exclusão - 408 (n: 11945)
  

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

# GRÁFICO 05 | Filosofia Evolução ####
evol_fi <-  dadosfi |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  mutate(nm_grau_academico = stri_trans_totitle(nm_grau_academico))
evol_fi |> 
  ggplot(aes(x = an_base, 
             y = n)) +
  geom_line(aes(color = nm_grau_academico), linewidth = 2)+
  stat_summary(aes(color = "Total"), fun = sum, geom ='line', linewidth = 2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  scale_color_metro_d()+
  labs(x = "",
       y = "") +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=25),
        text = element_text(size = 36, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/figs_tiff/graf5.tiff",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# GRÁFICO 06 | Relação Professor-Aluno####
dadosfi |> 
  mutate(g_oridis = recode(g_oridis,
                           "FF" = "M/M",
                           "FM" = "M/H",
                           "MF" = "H/M",
                           "MM" = "H/H")) |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  theme_classic() +
  scale_fill_metro_d() +
  labs(x = "",
       y = "",
       fill = "Professor/Estudante") +
  scale_x_continuous(limits = c(1990, 2021)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(legend.position = "top",
        legend.text=element_text(size=36),
        text = element_text(size = 36, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/graf6.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())

# Tabela 4 | Piores Universidades####
# Cálculo por estudantes das 15 piores Universidades
piores_ies <- dadosfi |> 
  group_by(nm_entidade_ensino) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  slice_max(total, n = 15) # Selecionar apenas os 15 piores. 

# Lista de áreas para filtrar os dados
lista_ies <- levels(piores_ies$nm_entidade_ensino)

# Cálculo por docente
piores_ies_go <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) 

piores_ies_go <- piores_ies_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por estudante
piores_ies_gd <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) 

piores_ies_gd <- piores_ies_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por orientador-estudante
piores_ies_god <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2)) 

piores_ies_god <- piores_ies_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento piores ####
piores_df <- list(piores_ies,
                  piores_ies_go,
                  piores_ies_gd,
                  piores_ies_god)

tab_piores_ies <- purrr::reduce(piores_df, 
                            left_join, 
                            by = "nm_entidade_ensino") |> 
  arrange(desc(total)) 

# Salvar tabela com todas IFES 
tab_piores_ies |>
  readr::write_csv("dados/tab4_total.csv")

# TABELA 4 | 10 piores ####
tab4 <- tab_piores_ies |> 
  gt(rowname_col = "nm_entidade_ensino") |>
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
    font = "Times New Roman") |> 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "0"
  )

#Salvar
gtsave(tab4, 
       "tab4_piores-ies.docx", 
       path = "figs",
       vwidth = 1400,
       vheight = 1700)
