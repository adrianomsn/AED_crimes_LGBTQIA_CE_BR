#### -----------------------------------------SCRIPT DE GRAFICOS DE ANALISE ------------------------------------####
###'
###'
###' GRAFICOS PARA ANALISE SOBRE CRIMES SEXUAIS, DE HOMOFOBIA E TRANSFOBIA NO CEARA
###'
###'
###' AUTOR: ADRIANO NETO, EM 19/05/2023
###' 
###' OBJETIVO: GERAR GRAFICOS PARA ANALISE DE CRIMES DE HOMOFOBIA E TRASNFOBIA NO CEARA
###' 
###'-----------------------------------------------------------------------------------------------------------

# Limpeza do ambiente
rm(list = ls())

# -------     Pacotes ------
pacotes = c(
  "dplyr",
  "rio",
  "ggplot2",
  "sf",
  "plotly",
  "geobr",
  "report",
  "viridis"
)

# Atencao ao diretorio no qual se encontra o seu script insumo
# qualquer problema verifique seu diretorio com 'getwd()' e se tiver
# divergencia altere utilizando o 'setwd()'

# Carrega script insumo de bases de dados
source("gera_base_crimes_sex_ceara.R")


# --------------- Graficos para analise ------------------------------

# Ainda ha muita subnotificao nos dados relatados a cerca de crimes de homofobia e transfobia
# portando nao levarei tanto em consideracao esses dados da maneira usual
# Creio que o correto e levar esse grafico como uma analise acerca do aumento de notificacao
# o que e positivo tendo em vista o cenario 

# Calcula o número de crimes por ANO
crimes_por_ano <- base_cr_hom_trans %>%
  group_by(ANO) %>% 
  summarise(QUANTIDADE = n())

# Evolucao dos crimes de homofobia e transfobia no ceara ao longo do tempo
graf_1 <- ggplot(crimes_por_ano, aes(x = ANO, y = QUANTIDADE)) +
  geom_line(color = "darkblue", size = 1) +  # Ajustando a cor e o tamanho da linha
  geom_point(color = "darkred", size = 3) +  # Ajustando a cor e o tamanho dos pontos
  theme_minimal() +
  labs(
    title = "Evolução dos Crimes de Homofobia e Transfobia ao Longo dos Anos",
    x = "Ano", 
    y = "Quantidade de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste se houver legenda
    legend.justification = c(0, 0.5)  # Ajuste se houver legenda
  )

ggplotly(graf_1)

# # Distrubuicao espacial dos crimes de hofomobia e transfobia no ceara

graf_2 <- ggplot(data = mapa_crimes) +
  geom_sf(aes(fill = QUANTIDADE), color = "grey", size = 0.1) +
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  theme_minimal() +
  labs(
    title = "Distribuição Espacial dos Crimes de Homofobia e Transfobia no Ceará 2021 - 2023",
    fill = "Quantidade de ocorrências"
  ) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.justification = c(0, 0.5),
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA)    # Fundo do gráfico cinza
  )

ggplotly(graf_2)

#--------------------------------

# calcula o montate de crimes por mes
crimes_por_mes <- base_cr_hom_trans %>%
  group_by(MES) %>% 
  summarise(QUANTIDADE = n())

# Distribuição Mensal dos Crimes de Homofobia e Transfobia
graf_3 <- ggplot(crimes_por_mes, aes(x = MES, y = QUANTIDADE, fill = MES)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição Mensal dos Crimes de Homofobia e Transfobia",
    x = "Mês", 
    y = "Quantidade de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )
ggplotly(graf_3)

# ------------------------------

# calcula o montante de crimes por dia da semana
crimes_por_turno <- base_cr_hom_trans %>%
  group_by(HORA) %>% 
  summarise(QUANTIDADE = n())

# Ordem dos turnos
ordem_turno <- c('MADRUGADA', 'MANHA', 'TARDE', 'NOITE')

# Redefine a ordem dos turnos
crimes_por_turno$HORA <- factor(crimes_por_turno$HORA, levels = ordem_turno)

# Frequência de crimes de Homofobia e Transfobia ao longo do dia
graf_4 <- ggplot(crimes_por_turno, aes(x = HORA, y = QUANTIDADE, fill = HORA)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Frequência de crimes de Homofobia e Transfobia ao longo do dia",
    x = "Turno", 
    y = "Quantidade de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_4)

# ----------------------------

# Calcula a quantidade crimes ocorridos por identidade de genero da vitima
crimes_por_ID_GEN <- base_cr_hom_trans %>%
  group_by(ID_GEN) %>% 
  summarise(QUANTIDADE = n())

# Distribuição dos Crimes de Homofobia e Transfobia por Identidade de Genero da vitima
graf_5 <- ggplot(crimes_por_ID_GEN, aes(x = ID_GEN, y = QUANTIDADE, fill = ID_GEN)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição dos Crimes de Homofobia e Transfobia por Identidade de Gênero da vítima",
    x = "Identidade de Gênero da vítima", y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_5)

# Calcula a quantidade crimes ocorridos por orientacao sexual da vitima
crimes_por_ORIENT_SEX <- base_cr_hom_trans %>%
  group_by(ORIENT_SEX) %>% 
  summarise(QUANTIDADE = n())

# Distribuição dos Crimes de Homofobia e Transfobia por Orientacao Sexual da vitima
graf_6 <- ggplot(crimes_por_ORIENT_SEX, aes(x = ORIENT_SEX, y = QUANTIDADE, fill = ORIENT_SEX)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição dos Crimes de Homofobia e Transfobia por Orientação Sexual da vítima",
    x = "Orientação Sexual da vítima", y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_6)


# ----------------------------

# Calcula a quantidade crimes ocorridos por por faixa etaria da vitima
crimes_por_FAIXA_ETARIA <- base_cr_hom_trans %>%
  mutate(FAIXA_ETARIA = case_when(
    is.na(FAIXA_ETARIA) ~ "SEM INFORMACAO",
    TRUE ~ as.character(FAIXA_ETARIA)
  )) %>%
  group_by(FAIXA_ETARIA) %>% 
  summarise(QUANTIDADE = n())

# Distribuição dos Crimes de Homofobia e Transfobia por Faixa Etária da vítima
graf_7 <- ggplot(crimes_por_FAIXA_ETARIA, aes(x = FAIXA_ETARIA, y = QUANTIDADE, fill = FAIXA_ETARIA)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição dos Crimes de Homofobia e Transfobia por Faixa Etária da vítima",
    x = "Faixa Etária da vítima", y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_7)


# Contar o número de crimes por escolaridade
crimes_por_V_ESCOLARIDADE <- base_cr_hom_trans %>%
  group_by(V_ESCOLARIDADE) %>% 
  summarise(QUANTIDADE = n())

# Ordem das escolaridades
ordem_escolaridade <- c('NAO ALFABETIZADO', 'ALFABETIZADO', 'ENSINO FUNDAMENTAL INCOMPLETO', 'ENSINO FUNDAMENTAL COMPLETO', 'ENSINO MEDIO INCOMPLETO',
                        'ENSINO MEDIO COMPLETO', 'SUPERIOR INCOMPLETO', 'SUPERIOR COMPLETO', 'NAO INFORMADA')

# Redefine a ordem dos escolaridades
crimes_por_V_ESCOLARIDADE$V_ESCOLARIDADE <- factor(crimes_por_V_ESCOLARIDADE$V_ESCOLARIDADE, levels = ordem_escolaridade)

# Distribuição dos Crimes de Homofobia e Transfobia por Escolaridade
graf_8 <- ggplot(crimes_por_V_ESCOLARIDADE, aes(x = V_ESCOLARIDADE, y = QUANTIDADE, fill = V_ESCOLARIDADE)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição dos Crimes de Homofobia e Transfobia por Escolaridade da vítima",
    x = "Escolaridade da vítima", y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_8)

# Calcula quantidade crimes por raca da vitima
crimes_por_V_RACA <- base_cr_hom_trans %>%
  group_by(V_RACA) %>% 
  summarise(QUANTIDADE = n())


# Ordem das escolaridades
ordem <- c('PRETA', 'PARDA', 'BRANCA', 'NAO INFORMADA')

# Redefine a ordem dos escolaridades
crimes_por_V_RACA$V_RACA <- factor(crimes_por_V_RACA$V_RACA, levels = ordem)

# Distribuição dos Crimes de Homofobia e Transfobia por Raça
graf_9 <- ggplot(crimes_por_V_RACA, aes(x = V_RACA, y = QUANTIDADE, fill = V_RACA)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Distribuição dos Crimes de Homofobia e Transfobia por Raça da vítima",
    x = "Raça da vítima", y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = c(0.2, 0.8),  # Ajuste da posição da legenda
    legend.justification = c(0, 0.5)  # Ajuste da justificativa da legenda
  )

ggplotly(graf_9)
