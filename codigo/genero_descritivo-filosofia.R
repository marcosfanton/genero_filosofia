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

# GRÁFICO 06 | Relação Professor-Aluno####
# Tabela para referência
graf6 <- dadosfi |> 
  group_by(an_base, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))
graf7 <- graf7 |> 
  pivot_wider(
  names_from = g_oridis,
  values_from = c(total_od, frequencia_od))

# Salvar tabela com todas IFES 
graf6 |>
  readr::write_csv("dados/graf6_goridis.csv")

# Gráfico 
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
        legend.text=element_text(size=12),
        text = element_text(size = 20, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/graf6_ifes.png",
  bg = "white",
  width = 8,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# Tabela 4 | Piores Universidades####
# Cálculo por estudantes das 15 piores Universidades
piores_ies <- dadosfi |> 
  group_by(nm_entidade_ensino) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  slice_max(total, n = 15) # Selecionar apenas as 15 piores 

# Lista das IFES e filtragem
lista_ies <- levels(piores_ies$nm_entidade_ensino)
dadosfi_piores <- dadosfi |> filter(nm_entidade_ensino %in% lista_ies)

# Função para criar Tabelas 4####
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

# Cálculo por docente
pior_go <- tabfun(dadosfi_piores, nm_entidade_ensino, g_orientador)
pior_gd <- tabfun(dadosfi_piores, nm_entidade_ensino, g_discente)
pior_god <- tabfun(dadosfi_piores, nm_entidade_ensino, g_oridis)

# Junção
piores_df <- list(piores_ies,
                  pior_go,
                  pior_gd,
                  pior_god)

tab_piores_ies <- purrr::reduce(piores_df, 
                            left_join, 
                            by = "nm_entidade_ensino") |> 
  arrange(desc(total)) 

# Salvar tabela com todas IFES 
tab_piores_ies |>
  readr::write_csv("dados/tab4_ifes.csv")

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

# Razão de prevalência####


#Matriz 2x2 para cálculo de razão de prevalência
matriz <- dados |>  
  filter(nm_area_avaliacao == "Filosofia") |> 
  drop_na(g_discente, g_orientador) |> 
  tabyl(g_orientador, g_discente) |> 
  adorn_totals(c("row", "col"))

matriz1 <- matriz  |> 
  remove_rownames()  |> 
  column_to_rownames(var = "g_orientador")

epi.2by2(dat = matriz1, method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")
