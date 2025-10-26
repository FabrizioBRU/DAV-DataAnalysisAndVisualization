# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 4: ANÁLISE INFERENCIAL (REGRESSÃO MÚLTIPLA)
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 (Atualize com a data de hoje)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. CARGA DE PACOTES E DADOS
# ------------------------------------------------
library(tidyverse)

# Vamos precisar do ggfortify para bons gráficos de diagnóstico
# install.packages("ggfortify") # Rode esta linha UMA VEZ se não o tiver
library(ggfortify)

# Lendo os dados brutos
df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")

# Preparando os dados para o modelo
df_modelo <- df %>%
  mutate(
    Sexo_label = factor(Sexo, 
                        levels = c(0, 1), 
                        labels = c("Masculino", "Feminino")),
    Curso = factor(Curso)
  ) %>%
  # Vamos selecionar apenas as colunas que usaremos no modelo
  select(Satisfacao, Desempenho.academico, Curso, Sexo_label)


# 2. CONSTRUÇÃO DO MODELO DE REGRESSÃO MÚLTIPLA
# ------------------------------------------------
# Queremos prever a Satisfacao usando as outras variáveis.
# A sintaxe lm(Y ~ X1 + X2 + X3, data) significa:
# "Modele a Satisfacao em função do Desempenho, do Curso e do Sexo"

print("--- Rodando o Modelo de Regressão Múltipla ---")

modelo_satisfacao <- lm(Satisfacao ~ Desempenho.academico + Curso + Sexo_label, 
                        data = df_modelo)

# 3. INTERPRETAÇÃO DO MODELO (O RESULTADO PRINCIPAL!)
# ------------------------------------------------
# A função summary() é a saída mais importante da regressão.
# É ela que nos diz quais variáveis são (ou não) significantes.

print("--- Sumário do Modelo de Regressão Múltipla ---")
summary(modelo_satisfacao)


# 4. ANÁLISE DOS PRESSUPOSTOS (DIAGNÓSTICO DO MODELO)
# ------------------------------------------------
# Um modelo de regressão só é válido se seus "erros" (resíduos)
# seguirem certos padrões (ex: normalidade, homocedasticidade).
# Vamos gerar um painel de gráficos de diagnóstico.

print("--- Gerando Gráficos de Diagnóstico do Modelo ---")

# O autoplot() do ggfortify cria os 4 gráficos de diagnóstico essenciais
graficos_diagnostico <- autoplot(modelo_satisfacao, 
                                 which = 1:4, # Queremos os gráficos 1, 2, 3 e 4
                                 ncol = 2)     # Organizados em 2 colunas

print(graficos_diagnostico)

# Salvando os gráficos de diagnóstico
ggsave("./plots/10_diagnostico_regressao.png", 
       plot = graficos_diagnostico, 
       width = 10, height = 8) # Dando mais espaço para os 4 gráficos

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 4
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---