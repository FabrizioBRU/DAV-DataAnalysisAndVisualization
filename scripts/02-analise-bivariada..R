# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 3: ANÁLISE BIVARIADA (Buscando Relações)
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 (Atualize com a data de hoje)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. CARGA DE PACOTES E DADOS
# ------------------------------------------------
# Vamos carregar os pacotes e os dados novamente
# (Boas práticas: todo script deve ser independente)
library(tidyverse)

# Lendo os dados brutos
df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")

# Preparando os dados (transformando em fatores, como na Fase 2)
df_analise <- df %>%
  mutate(
    Sexo_label = factor(Sexo, 
                        levels = c(0, 1), 
                        labels = c("Masculino", "Feminino")),
    Curso = factor(Curso),
    Nivel.de.satisfacao = factor(Nivel.de.satisfacao, 
                                 levels = c("Baixa", "Media", "Alta"))
  )

# Vamos também carregar o pacote 'ggpubr' para adicionar correlações
# install.packages("ggpubr") # Rode esta linha UMA VEZ se não o tiver
library(ggpubr)


# 2. ANÁLISE: Satisfação (Numérica) vs. Desempenho (Numérica)
# ------------------------------------------------
print("--- Gráfico: Satisfação vs. Desempenho Acadêmico ---")

# Usamos um gráfico de dispersão (scatter plot)
grafico_sat_desempenho <- ggplot(df_analise, aes(x = Desempenho.academico, y = Satisfacao)) +
  geom_point(alpha = 0.6, color = "#0072B2") + # Pontos com transparência
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Linha de tendência (Regressão Linear Simples)
  # Adiciona o coeficiente de correlação (R) e o p-valor
  stat_cor(method = "pearson", label.x = 7, label.y = 45) +
  labs(title = "Relação entre Satisfação e Desempenho Acadêmico",
       x = "Desempenho Acadêmico (Nota)",
       y = "Satisfação (Nota 0-100)") +
  theme_minimal()

print(grafico_sat_desempenho)
ggsave("./plots/06_dispersao_satisfacao_desempenho.png", plot = grafico_sat_desempenho)


# 3. ANÁLISE: Satisfação (Numérica) vs. Curso (Categórica)
# ------------------------------------------------
print("--- Gráfico: Satisfação por Curso ---")

# Usamos Boxplots para comparar as distribuições de satisfação entre os cursos
grafico_sat_curso <- ggplot(df_analise, aes(x = Curso, y = Satisfacao, fill = Curso)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2, show.legend = FALSE) + # Adiciona os pontos reais
  labs(title = "Distribuição da Satisfação por Curso",
       x = "Curso",
       y = "Satisfação (Nota 0-100)") +
  theme_minimal() +
  coord_flip() # Vira o gráfico para melhor leitura dos cursos

print(grafico_sat_curso)
ggsave("./plots/07_boxplot_satisfacao_curso.png", plot = grafico_sat_curso)


# 4. ANÁLISE: Satisfação (Numérica) vs. Sexo (Categórica)
# ------------------------------------------------
print("--- Gráfico: Satisfação por Sexo ---")

grafico_sat_sexo <- ggplot(df_analise, aes(x = Sexo_label, y = Satisfacao, fill = Sexo_label)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2, show.legend = FALSE) +
  labs(title = "Distribuição da Satisfação por Sexo",
       x = "Sexo",
       y = "Satisfação (Nota 0-100)") +
  theme_minimal()

print(grafico_sat_sexo)
ggsave("./plots/08_boxplot_satisfacao_sexo.png", plot = grafico_sat_sexo)


# 5. ANÁLISE: Satisfação (Numérica) vs. Semestres (Numérica/Discreta)
# ------------------------------------------------
print("--- Gráfico: Satisfação vs. Semestres Cursados ---")

grafico_sat_semestres <- ggplot(df_analise, aes(x = Semestres.cursados, y = Satisfacao)) +
  geom_point(alpha = 0.6, color = "#D55E00") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + # Linha de tendência
  stat_cor(method = "pearson", label.x = 7, label.y = 45) +
  labs(title = "Relação entre Satisfação e Semestres Cursados",
       x = "Semestres Cursados",
       y = "Satisfação (Nota 0-100)") +
  theme_minimal()

print(grafico_sat_semestres)
ggsave("./plots/09_dispersao_satisfacao_semestres.png", plot = grafico_sat_semestres)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 3
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---