# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# TRABALHO: ANÁLISE DE SATISFAÇÃO DA UNIVERSIDADE
# FASE 5: ANÁLISE EXPLORATÓRIA ADICIONAL (GRÁFICOS DE CONTEXTO)
# (Versão com ordenação de curso e cor do Gráfico 13 corrigida)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# 1. CARGA DE PACOTES E DADOS
# ------------------------------------------------
library(tidyverse)
library(ggpubr) # Para o stat_cor()

# Lendo os dados brutos
df <- read.csv2("./data/satisfacao.csv", header = TRUE, sep = ";")

# --- DEFINIÇÃO DA PALETA DE CORES CONSISTENTE (CORRIGIDA) ---
cores_cursos_palette <- c(
  "Administracao" = "#F8766D", # Vermelho/Coral
  "Arquitetura"   = "#A3A500", # Olive
  "Direito"       = "#00BF7D", # Verde
  "Economia"      = "#00B0F6", # Azul Claro
  "Medicina"      = "#E76BF3"  # Rosa/Magenta
)
# ------------------------------------------------

# Preparando os dados
df_analise <- df %>%
  mutate(
    Sexo_label = factor(Sexo, 
                        levels = c(0, 1), 
                        labels = c("Masculino", "Feminino")),
    Curso = factor(Curso, levels = c("Medicina", "Economia", "Direito", 
                                     "Arquitetura", "Administracao")),
    Nivel.de.satisfacao = factor(Nivel.de.satisfacao, 
                                 levels = c("Baixa", "Media", "Alta"))
  )


# 2. GRÁFICO 1 (Refatorado): Alunos por Semestre e por Curso
# ------------------------------------------------
# (Blocos 2.1 a 2.5 permanecem idênticos, gerando os 5 histogramas)
# --- 2.1: ADMINISTRAÇÃO ---
print("--- Gráfico 11.1: Semestres - Administração ---")
df_adm <- df_analise %>% filter(Curso == "Administracao")
max_sem_adm <- max(df_adm$Semestres.cursados)
grafico_adm <- ggplot(df_adm, aes(x = Semestres.cursados)) +
  geom_histogram(binwidth = 1, fill = cores_cursos_palette["Administracao"], color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(1, max_sem_adm, by = 1)) + 
  labs(title = "Distribuição de Alunos por Semestre: Administração",
       x = "Semestres Cursados", y = "Contagem de Alunos") +
  theme_minimal()
print(grafico_adm)
ggsave("./plots/11_hist_semestres_Administracao.png", plot = grafico_adm)

# --- 2.2: ARQUITETURA ---
print("--- Gráfico 11.2: Semestres - Arquitetura ---")
df_arq <- df_analise %>% filter(Curso == "Arquitetura")
max_sem_arq <- max(df_arq$Semestres.cursados)
grafico_arq <- ggplot(df_arq, aes(x = Semestres.cursados)) +
  geom_histogram(binwidth = 1, fill = cores_cursos_palette["Arquitetura"], color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(1, max_sem_arq, by = 1)) + 
  labs(title = "Distribuição de Alunos por Semestre: Arquitetura",
       x = "Semestres Cursados", y = "Contagem de Alunos") +
  theme_minimal()
print(grafico_arq)
ggsave("./plots/11_hist_semestres_Arquitetura.png", plot = grafico_arq)

# --- 2.3: DIREITO ---
print("--- Gráfico 11.3: Semestres - Direito ---")
df_dir <- df_analise %>% filter(Curso == "Direito")
max_sem_dir <- max(df_dir$Semestres.cursados)
grafico_dir <- ggplot(df_dir, aes(x = Semestres.cursados)) +
  geom_histogram(binwidth = 1, fill = cores_cursos_palette["Direito"], color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(1, max_sem_dir, by = 1)) + 
  labs(title = "Distribuição de Alunos por Semestre: Direito",
       x = "Semestres Cursados", y = "Contagem de Alunos") +
  theme_minimal()
print(grafico_dir)
ggsave("./plots/11_hist_semestres_Direito.png", plot = grafico_dir)

# --- 2.4: ECONOMIA ---
print("--- Gráfico 11.4: Semestres - Economia ---")
df_eco <- df_analise %>% filter(Curso == "Economia")
max_sem_eco <- max(df_eco$Semestres.cursados)
grafico_eco <- ggplot(df_eco, aes(x = Semestres.cursados)) +
  geom_histogram(binwidth = 1, fill = cores_cursos_palette["Economia"], color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(1, max_sem_eco, by = 1)) + 
  labs(title = "Distribuição de Alunos por Semestre: Economia",
       x = "Semestres Cursados", y = "Contagem de Alunos") +
  theme_minimal()
print(grafico_eco)
ggsave("./plots/11_hist_semestres_Economia.png", plot = grafico_eco)

# --- 2.5: MEDICINA ---
print("--- Gráfico 11.5: Semestres - Medicina ---")
df_med <- df_analise %>% filter(Curso == "Medicina")
max_sem_med <- max(df_med$Semestres.cursados) 
grafico_med <- ggplot(df_med, aes(x = Semestres.cursados)) +
  geom_histogram(binwidth = 1, fill = cores_cursos_palette["Medicina"], color = "white", alpha = 0.8) +
  scale_x_continuous(breaks = seq(1, max_sem_med, by = 1)) + 
  labs(title = "Distribuição de Alunos por Semestre: Medicina",
       x = "Semestres Cursados", y = "Contagem de Alunos") +
  theme_minimal()
print(grafico_med)
ggsave("./plots/11_hist_semestres_Medicina.png", plot = grafico_med)


# 3. GRÁFICO 12: Desempenho por Curso (COM ORDEM CORRIGIDA)
# ------------------------------------------------
print("--- Gráfico 12: Desempenho por Curso ---")
grafico_desempenho_curso <- ggplot(df_analise, aes(x = Curso, y = Desempenho.academico, fill = Curso)) +
  geom_boxplot(show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.2, show.legend = FALSE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") + 
  scale_fill_manual(values = cores_cursos_palette) +
  labs(title = "Distribuição do Desempenho Acadêmico por Curso",
       x = "Curso",
       y = "Desempenho Acadêmico (Nota)") +
  theme_minimal() +
  coord_flip() 
print(grafico_desempenho_curso)
ggsave("./plots/12_boxplot_desempenho_curso.png", plot = grafico_desempenho_curso)


# 4. GRÁFICO 13: Desempenho por Semestre (COM COR CINZA)
# ------------------------------------------------
print("--- Gráfico 13: Desempenho vs. Semestres Cursados ---")

grafico_desempenho_semestres <- ggplot(df_analise, aes(x = Semestres.cursados, y = Desempenho.academico)) +
  
  # --- MUDANÇA AQUI ---
  # Pontos agora são cinza escuro para evitar confusão com cores de curso
  geom_point(alpha = 0.5, color = "gray20") + 
  # --- FIM DA MUDANÇA ---
  
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  stat_cor(method = "pearson", label.x = 7, label.y = 2.5) + # Posição do R
  labs(title = "Relação entre Desempenho e Semestres Cursados",
       x = "Semestres Cursados",
       y = "Desempenho Acadêmico (Nota)") +
  theme_minimal()
print(grafico_desempenho_semestres)
ggsave("./plots/13_dispersao_desempenho_semestres.png", plot = grafico_desempenho_semestres)


# 5. GRÁFICO 14: Desempenho por Semestre e por Curso (COM ORDEM CORRIGIDA)
# ------------------------------------------------
print("--- Gráfico 14: Desempenho vs. Semestres (Faceted by Curso) ---")
grafico_desempenho_semestres_curso <- ggplot(df_analise, aes(x = Semestres.cursados, y = Desempenho.academico, color = Curso)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) + 
  scale_color_manual(values = cores_cursos_palette) +
  facet_wrap(~ Curso, ncol = 2) + 
  labs(title = "Relação Desempenho vs. Semestres (por Curso)",
       x = "Semestres Cursados",
       y = "Desempenho Acadêmico (Nota)") +
  theme_minimal()
print(grafico_desempenho_semestres_curso)
ggsave("./plots/14_dispersao_desempenho_semestres_por_curso.png", plot = grafico_desempenho_semestres_curso, width = 10, height = 8)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# FIM DA FASE 5
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---