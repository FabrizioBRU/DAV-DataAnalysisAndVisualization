# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 6: GERAÇÃO DE TABELAS DE ANÁLISE
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 (Atualize com a data de hoje)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. CARGA DE PACOTES E DADOS
# ------------------------------------------------
library(tidyverse)
library(broom) 

# Lendo e preparando os dados
df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")
df_analise <- df %>%
  mutate(Curso = factor(Curso))


# 2. TABELA 1: Desempenho vs. Semestres (por Curso)
# ------------------------------------------------
print("--- Tabela 1: Correlação Desempenho vs. Semestres (por Curso) ---")

tabela_desempenho_semestres <- df_analise %>%
  group_by(Curso) %>% 
  summarise(
    Correlacao_R = cor(Desempenho.academico, Semestres.cursados, method = "pearson"),
    P_Valor = cor.test(Desempenho.academico, Semestres.cursados)$p.value,
    .groups = 'drop' 
  ) %>%
  mutate(
    Correlacao_R = round(Correlacao_R, 4),
    P_Valor = round(P_Valor, 3)
  )

print(tabela_desempenho_semestres)


# 3. TABELA 2: Estatísticas Descritivas do Desempenho (por Curso)
# ------------------------------------------------
print("--- Tabela 2: Estatísticas Descritivas do Desempenho Acadêmico (por Curso) ---")

tabela_descritiva_desempenho <- df_analise %>%
  group_by(Curso) %>%
  summarise(
    Contagem = n(), # Número de alunos
    Media = mean(Desempenho.academico),
    Mediana = median(Desempenho.academico),
    Desvio_Padrao = sd(Desempenho.academico),
    Minimo = min(Desempenho.academico),
    Q1 = quantile(Desempenho.academico, 0.25),
    Q3 = quantile(Desempenho.academico, 0.75),
    Maximo = max(Desempenho.academico),
    .groups = 'drop'
  ) %>%
  mutate(Curso = factor(Curso, levels = c("Administracao", "Arquitetura", 
                                          "Direito", "Economia", "Medicina"))) %>%
  arrange(Curso) %>% 
  mutate(across(where(is.numeric), ~ round(., 2)))

print(tabela_descritiva_desempenho)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# 4. TABELA 3: Correlação GERAL Desempenho vs. Semestres
# (Esta é a tabela para o Gráfico 13)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
print("--- Tabela 3: Correlação GERAL Desempenho vs. Semestres ---")

# Correlação simples entre os dois vetores
cor_test_geral <- cor.test(df_analise$Desempenho.academico, df_analise$Semestres.cursados)

tabela_cor_geral <- tibble(
  Variaveis = "Desempenho vs. Semestres",
  Correlacao_R = cor_test_geral$estimate,
  P_Valor = cor_test_geral$p.value
) %>%
  # Arredonda os números para melhor leitura
  mutate(
    Correlacao_R = round(Correlacao_R, 4),
    P_Valor = round(P_Valor, 3)
  )

print(tabela_cor_geral)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 6
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---