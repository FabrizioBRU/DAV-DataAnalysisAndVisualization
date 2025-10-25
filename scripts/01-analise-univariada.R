# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 1: CARGA E PREPARAÇÃO DOS DADOS
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. INSTALAÇÃO E CARGA DE PACOTES
# ------------------------------------------------
# O 'tidyverse' é um super-pacote que inclui o ggplot2 (para gráficos)
# e o dplyr (para manipulação de dados).

# install.packages("tidyverse") # << IMPORTANTE: RODE ESTA LINHA UMA VEZ

# Após instalar, carregamos o pacote na memória para usá-lo:
library(tidyverse)

# 2. CARGA DOS DADOS
# ------------------------------------------------
# O caminho "./data/satisfacao.csv" significa:
# "a partir da pasta raiz do meu projeto, entre na pasta 'data' e pegue o arquivo 'satisfacao.csv'".
#
# Usamos read.csv2() porque seus dados usam:
# - Ponto e vírgula (;) como separador de colunas.
# - Vírgula (,) como separador decimal (ex: 7,40).

df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FASE 2: ANÁLISE UNIVARIADA (GRÁFICOS)
# Autor: Fabrizio Bruzetti
# Data: 25/10/2025 (Atualize com a data de hoje)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Nota: O 'tidyverse' (que já carregamos) inclui o 'ggplot2' para gráficos.
# A função ggsave() salva o último gráfico gerado na pasta 'plots'.

# 0. PREPARAÇÃO DOS DADOS PARA GRÁFICOS
# ------------------------------------------------
# Vamos criar uma cópia do dataframe 'df' para fazer modificações seguras.
# Também vamos transformar as variáveis categóricas (como Sexo e Nivel)
# em "fatores" (factors), que é como o R entende categorias.

df_analise <- df %>%
  mutate(
    # Converte 'Sexo' (0, 1) em labels de texto para os gráficos
    Sexo_label = factor(Sexo, 
                        levels = c(0, 1), 
                        labels = c("Masculino", "Feminino")),
    
    # Converte 'Curso' em fator
    Curso = factor(Curso),
    
    # Converte 'Nivel.de.satisfacao' em fator e REORDENA
    # (para que "Baixa" venha antes de "Média" e "Alta")
    Nivel.de.satisfacao = factor(Nivel.de.satisfacao, 
                                 levels = c("Baixa", "Media", "Alta"))
  )

# Vamos verificar a nova estrutura
str(df_analise)


# 1. Variável: Sexo (Categórica)
# ------------------------------------------------
print("--- Gerando Gráfico: Sexo ---")

grafico_sexo <- ggplot(df_analise, aes(x = Sexo_label, fill = Sexo_label)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) + # 'show.legend = FALSE' remove a legenda
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Formata eixo Y como %
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 1)), 
            stat = "count", vjust = -0.5) + # Adiciona os rótulos de %
  labs(title = "Distribuição de Estudantes por Sexo",
       x = "Sexo",
       y = "Proporção (%)") +
  theme_minimal() # Estilo de gráfico limpo

# Exibe o gráfico no RStudio
print(grafico_sexo)

# Salvando o gráfico na pasta 'plots'
ggsave("./plots/01_distribuicao_sexo.png", plot = grafico_sexo)


# 2. Variável: Curso (Categórica)
# ------------------------------------------------
print("--- Gerando Gráfico: Curso ---")

grafico_curso <- ggplot(df_analise, aes(x = Curso, fill = Curso)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 1)), 
            stat = "count", vjust = -0.5, size = 3) +
  labs(title = "Distribuição de Estudantes por Curso",
       x = "Curso",
       y = "Proporção (%)") +
  theme_minimal()

print(grafico_curso)
ggsave("./plots/02_distribuicao_curso.png", plot = grafico_curso)


# 3. Variável: Desempenho academico (Numérica)
# ------------------------------------------------
print("--- Gerando Gráfico: Desempenho Acadêmico ---")

# Vamos calcular a média para adicionar ao gráfico
media_desempenho <- mean(df_analise$Desempenho.academico, na.rm = TRUE)

grafico_desempenho <- ggplot(df_analise, aes(x = Desempenho.academico)) +
  geom_histogram(binwidth = 0.5, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = media_desempenho), 
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_desempenho * 1.05, y = 50, # Posição do texto da média
           label = paste("Média =", round(media_desempenho, 2)), color = "red") +
  labs(title = "Distribuição do Desempenho Acadêmico",
       x = "Desempenho (Nota)",
       y = "Frequência (Contagem)") +
  theme_minimal()

print(grafico_desempenho)
ggsave("./plots/03_histograma_desempenho.png", plot = grafico_desempenho)


# 4. Variável: Satisfacao (Numérica) - Nossa Variável-Alvo
# ------------------------------------------------
print("--- Gerando Gráfico: Satisfação ---")

media_satisfacao <- mean(df_analise$Satisfacao, na.rm = TRUE)

grafico_satisfacao <- ggplot(df_analise, aes(x = Satisfacao)) +
  geom_histogram(binwidth = 5, fill = "#009E73", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = media_satisfacao), 
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_satisfacao * 0.95, y = 60,
           label = paste("Média =", round(media_satisfacao, 2)), color = "red") +
  labs(title = "Distribuição da Satisfação com a Universidade",
       x = "Satisfação (Nota 0-100)",
       y = "Frequência (Contagem)") +
  theme_minimal()

print(grafico_satisfacao)
ggsave("./plots/04_histograma_satisfacao.png", plot = grafico_satisfacao)


# 5. Variável: Nivel de satisfacao (Categórica Ordinal)
# ------------------------------------------------
print("--- Gerando Gráfico: Nível de Satisfação ---")

grafico_nivel_satisfacao <- ggplot(df_analise, aes(x = Nivel.de.satisfacao, fill = Nivel.de.satisfacao)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..), accuracy = 1)), 
            stat = "count", vjust = -0.5) +
  labs(title = "Distribuição por Nível de Satisfação",
       x = "Nível de Satisfação",
       y = "Proporção (%)") +
  theme_minimal()

print(grafico_nivel_satisfacao)
ggsave("./plots/05_distribuicao_nivel_satisfacao.png", plot = grafico_nivel_satisfacao)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 2
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# 3. VERIFICAÇÃO INICIAL
# ------------------------------------------------
# Vamos inspecionar o objeto 'df' (DataFrame) que criamos.

# Mostra a estrutura dos dados (tipos de colunas)
# Queremos ver 'Curso' como "character" (texto) e 'Satisfacao' como "numeric".
print("--- Estrutura (str) ---")
str(df)

# Mostra as primeiras 6 linhas
print("--- Primeiras linhas (head) ---")
head(df)

# Mostra um sumário estatístico rápido de todas as colunas
print("--- Sumário (summary) ---")
summary(df)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 1
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---