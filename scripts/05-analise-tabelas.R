# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 6: GERAÇÃO DE TABELAS DE ANÁLISE
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 (Atualize com a data de hoje)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. CARGA DE PACOTES E DADOS
# ------------------------------------------------
library(tidyverse)
# install.packages("broom") # Rode uma vez se não tiver
library(broom) # Essencial para "arrumar" saídas de modelo

# Lendo e preparando os dados
df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")
df_analise <- df %>%
  mutate(Curso = factor(Curso))


# 2. TABELA: Desempenho vs. Semestres (por Curso)
# ------------------------------------------------
# O Gráfico 14 plota uma linha de regressão (lm) para cada curso.
# Vamos calcular os dados estatísticos (correlação e p-valor)
# para essa relação em cada curso.

print("--- Tabela: Correlação Desempenho vs. Semestres (por Curso) ---")

tabela_desempenho_semestres <- df_analise %>%
  group_by(Curso) %>% # Agrupa por curso
  # Para cada curso, calcula a correlação (R) e o p-valor
  summarise(
    Correlacao_R = cor(Desempenho.academico, Semestres.cursados, method = "pearson"),
    P_Valor = cor.test(Desempenho.academico, Semestres.cursados)$p.value
  ) %>%
  # Arredonda os números para facilitar a leitura
  mutate(
    Correlacao_R = round(Correlacao_R, 4),
    P_Valor = round(P_Valor, 3)
  )

# Imprime a tabela final no console
print(tabela_desempenho_semestres)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 6
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---