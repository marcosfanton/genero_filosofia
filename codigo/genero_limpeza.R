####Pacotes####
library(tidyverse)
library(here) 
library(stringi) # Limpeza de texto
library(textclean) # Limpeza de texto
library(genderBR) # Dicionário de gênero com base no primeiro nome

# Banco da Plataforma CAPES-DADOS ABERTOS
# Tema: Avaliação da Pós-Graduação strictu sensu
# https://dadosabertos.capes.gov.br/organization/diretoria-de-avaliacao
# Grupo: Catálogo de Teses e Dissertações - Brasil 
# Todos os bancos foram baixados (em .csv) e armazenados em uma pasta única
# Ao todo, são 32 arquivos com cerca de 145gb. 
# SessionInfo::
# R version 4.1.2 (2021-11-01) - macOS 14.0 Platform: aarch64-apple-darwin20 (64-bit)

# Unificação dos bancos de 1991-2021####
# Bancos de 1991-2012
banco9112 <- purrr::map_dfr(list.files(path = "dados/bancos/capes9112", 
                                       pattern = "dados", # abre todos arquivo com a referida string
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1", 
                                                   decimal_mark = ",", 
                                                   grouping_mark = "."),
                            show_col_types = FALSE)
# Bancos de 2013-2021
banco1321 <- purrr::map_dfr(list.files(path = "dados/bancos/capes1321", 
                                       pattern = "capes", 
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1", 
                                                   decimal_mark = ",", 
                                                   grouping_mark = "."),
                            na = c("NI", "NA"), 
                            show_col_types = FALSE) 

# Variáveis de interesse dos Bancos de 1991-2012 
# Ver Metadados do Catálogo
vars9112 <- c("AnoBase",
              "NomeIes",
              "GrandeAreaDescricao",
              "AreaAvaliacao",
              "Nivel", 
              "Autor",
              "Orientador_1",
              "Regiao",
              "Uf")

# Variáveis dos Bancos de 2013-2021 - Ver Metadados do Catálogo
vars1321 <- c("AN_BASE",
              "NM_ENTIDADE_ENSINO",
              "NM_GRANDE_AREA_CONHECIMENTO",
              "NM_AREA_AVALIACAO",
              "NM_SUBTIPO_PRODUCAO", 
              "NM_GRAU_ACADEMICO",
              "NM_DISCENTE",
              "NM_ORIENTADOR",
              "NM_REGIAO",
              "SG_UF_IES")

# Junção dos bancos com as 13 variáveis escolhidas + 1991####
catalogo9121 <- dplyr::bind_rows(
  banco9112 |> dplyr::select(all_of(vars9112)) |> 
    dplyr::rename_with(.cols = all_of(vars9112), 
                       ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), # Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  dplyr::select(all_of(vars1321)) 

# Limpeza do texto e padronização de variáveis####
catalogo9121 <- catalogo9121  |> 
  dplyr::mutate(SG_UF_IES = as.factor(SG_UF_IES),
                dplyr::across(where(is.character), 
                       ~ stringr::str_squish(str_to_title(., locale = "pt_BR"))), # Padroniza todo texto em caixa alta na primeira letra
                NM_GRANDE_AREA_CONHECIMENTO = recode(NM_GRANDE_AREA_CONHECIMENTO,
                                                     "Lingüística, Letras E Artes" = "Linguística, Letras E Artes"),
                NM_AREA_AVALIACAO = stringr::str_replace_all(NM_AREA_AVALIACAO, c(Ii = "II", IIi = "III", Iv = "IV")), # Mantém o nome correto
                NM_AREA_AVALIACAO = recode(NM_AREA_AVALIACAO, # Recodificação das áreas de avaliação 
                                           "Filosofia / Teologia:subcomissão Filosofia" = "Filosofia", 
                                           "Filosofia/Teologia:subcomissão Filosofia" = "Filosofia",
                                           "Filosofia/Teologia:subcomissão Teologia" = "Teologia",
                                           "Ciências Da Religião E Teologia" = "Teologia",
                                           "Administração Pública E De Empresas, Ciências Contábeis E Turismo" = "Administração, Ciências Contábeis E Turismo",
                                           "Letras / Linguística" = "Linguística E Literatura",
                                           "Ciência Política E Relações Internacionais" = "Ciência Política/RI",
                                           "Matemática / Probabilidade E Estatística" = "Matemática e Estatística",
                                           "Antropologia / Arqueologia" = "Antropologia"),
                NM_GRAU_ACADEMICO = case_when( # Atribui titulação com base na variável nm_subtipo_producao
                  AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "Mestrado" ~ "Mestrado",
                  AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "Doutorado" ~ "Doutorado",
                  AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "Profissionalizante" ~ "Mestrado Profissional",
                  TRUE ~ as.character(NM_GRAU_ACADEMICO))
  ) |> 
  dplyr::rename_all(tolower)

# Variáveis derivadas####
# Gênero de orientadores, orientandos e orientador-orientando com o pacote @GenderBR####
catalogo9121 <- catalogo9121  |>  
  dplyr::mutate(
    g_orientador = genderBR::get_gender(nm_orientador),
    g_discente = genderBR::get_gender(nm_discente),
    g_oridis = factor(case_when(
      g_orientador == "Male" & g_discente == "Male" ~ "MM",
      g_orientador == "Male" & g_discente == "Female" ~ "MF",
      g_orientador == "Female" & g_discente == "Male" ~ "FM",
      g_orientador == "Female" & g_discente == "Female" ~ "FF")
    ))
# N = 1374373 (1991-2021)
# Exclui NA's de variáveis da análise
catalogo9121 <- catalogo9121 |> 
  dplyr::filter(nm_grau_academico != "Doutorado Profissional") |>  # Exclusão de 39 observações (n = 1374334)
  dplyr::filter(nm_grande_area_conhecimento != "") |>  # Exclui 2 observação com fator em branco (1374332)
  tidyr::drop_na(g_oridis) |> # Exclui NAs de g_orientador e g_discente (-256370) (1117944) --> 81.34% 
  tidyr::drop_na(an_base) # Exclui 1 observação sem ano.

# Banco limpo####
# N = 1117961
# Salvar arquivo RAW 
catalogo9121 |>
  readr::write_csv("dados/bancos/catalogo9121_raw.csv")
