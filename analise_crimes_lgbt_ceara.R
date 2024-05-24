#### -----------------------------------------SCRIPT DE ANALISE ------------------------------------####
###'
###'
###' ANALISE EXPLORATORIA SOBRE VIOLENCIA CONTRA COMUNIDADE LGBTQIA+ NO CEARA
###'
###'
###' AUTOR: ADRIANO NETO, EM 19/05/2023
###' 
###' OBJETIVO: GERA RESULTADOS DE ANALISE SOBRE O TEMA
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
  "viridis",
  "webshot2",
  "webshot",
  "ggplot",
  "ggpubr"
)

# Seta o diretorio correto
setwd('C:\\Users\\super\\Downloads\\')
getwd()
# Carrega script insumo de bases de dados E graficos
source("gera_grafico_analise_crimes_ceara.R")



# ------------------ Analise ------------------------

# Evolucao dos crimes de homofobia e transfobia no ceara ao longo do tempo (2021-2023)

# Plota o grafico
print(graf_1)

# Gera o relatorio estatistico
relatorio_ano<- report(crimes_por_ano)

# Exibe o relatorio
print(relatorio_ano)
boxplot(crimes_por_ano$QUANTIDADE)

# Base no formato long ficou ideal
crimes_por_mes_ano <- base_cr_hom_trans %>%
  group_by(MES, ANO) %>% 
  summarise(QUANTIDADE = n())

# Grafico de evolucao dos crimes ao longo dos meses
graf_long <- ggplot(crimes_por_mes_ano, aes(x = MES, y = QUANTIDADE, group = ANO, color = as.factor(ANO))) +
  geom_line(size = 1) +  # Ajuste do tamanho da linha
  geom_point(size = 3) +  # Ajuste do tamanho dos pontos
  geom_label(aes(label = QUANTIDADE), size = 4, color = "black", fill = "white", fontface = "bold") +  # Adiciona os valores absolutos com caixa de fundo
  theme_minimal() +
  scale_color_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis
  labs(
    title = "Evolução dos Crimes de Homofobia e Transfobia ao Longo dos Meses",
    x = "Mês", 
    y = "Quantidade de Crimes",
    color = "Ano"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    plot.title = element_text(size = 20, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do texto da legenda
    legend.title = element_text(size = 15, face = "bold"), # Ajusta o tamanho e estilo do título da legenda
    axis.title.x = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título do eixo x
    axis.title.y = element_text(size = 15, face = "bold")   # Ajusta o tamanho e estilo do título do eixo y
  )

print(graf_long)


# Cria tabela de contingencia
crimes_por_id_orient<- as.data.frame(xtabs(~ID_GEN+ORIENT_SEX, base_cr_hom_trans))

# Ordem 
ordem <- c('HOMEM CIS', 'HOMEM TRANS', 'MULHER CIS', 'MULHER TRANS', 'TRAVESTI', 'NAO INFORMADO')

# Redefine a ordem 
crimes_por_id_orient$ID_GEN <- factor(crimes_por_id_orient$ID_GEN, levels = ordem)

# Grafico de crimes por identidade de gen e orientacao sexual
graf_barras <- ggplot(crimes_por_id_orient, aes(x = ID_GEN, y = Freq, fill = as.factor(ORIENT_SEX))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +  # Gráfico de barras com contorno branco e barras lado a lado
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black", fontface = "bold") +  # Adiciona os valores absolutos
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis
  labs(
    title = "Distribuição dos Crimes contra LGBTQIA+ por Identidade de Gênero e Orientação Sexual",
    x = "Identidade de Gênero", 
    y = "Quantidade de Crimes",
    fill = "Orientação Sexual"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    plot.title = element_text(size = 20, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do texto da legenda
    legend.title = element_text(size = 15, face = "bold"), # Ajusta o tamanho e estilo do título da legenda
    axis.title.x = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título do eixo x
    axis.title.y = element_text(size = 15, face = "bold")   # Ajusta o tamanho e estilo do título do eixo y
  )

print(graf_barras)

# Relatorio
# report_teste<- report(crimes_wide)
# print(report_teste)

# faz agrupamentos para facilitar analise
base_alt <- base_cr_hom_trans %>% 
  mutate(ID_GEN = case_when(ID_GEN == "HOMEM CIS" ~ "MASCULINO",
                            ID_GEN == 'HOMEM TRANS' ~ 'MASCULINO',
                            ID_GEN == 'MULHER CIS' ~ 'FEMININO',
                            ID_GEN == 'MULHER TRANS' ~ 'FEMININO',
                            ID_GEN == 'TRAVESTI' ~ 'OUTRO',
                            ID_GEN == 'NAO INFORMADO' ~ 'OUTRO',
                            TRUE ~ ID_GEN))

# TABELA DE CONTINGENCIA
crimes_por_idade_sexo<- as.data.frame(xtabs(~FAIXA_ETARIA+ID_GEN, base_alt))

# Grafico da distribuicao dos crimes por identidade de gen e faixa etaria
graf_barras <- ggplot(crimes_por_idade_sexo, aes(x = ID_GEN, y = Freq, fill = as.factor(FAIXA_ETARIA))) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +  # Gráfico de barras com contorno branco e barras lado a lado
  geom_text(aes(label = Freq), position = position_dodge(width = 0.9), vjust = -0.5, size = 4, color = "black", fontface = "bold") +  # Adiciona os valores absolutos
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis
  labs(
    title = "Distribuição dos Crimes contra LGBTQIA+ por Identidade de Gênero e Faixa Etária",
    x = "Identidade de Gênero", 
    y = "Quantidade de Crimes",
    fill = "Faixa Etária"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    plot.title = element_text(size = 20, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 12, face = "bold"),  # Ajusta o tamanho e estilo do texto da legenda
    legend.title = element_text(size = 15, face = "bold"), # Ajusta o tamanho e estilo do título da legenda
    axis.title.x = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título do eixo x
    axis.title.y = element_text(size = 15, face = "bold")   # Ajusta o tamanho e estilo do título do eixo y
  )

print(graf_barras)

# Transformando para formato wide
crimes_wide <- crimes_por_id_orient %>%
  pivot_wider(names_from = ID_GEN, values_from = Freq)

# Distribuicao da amostra
boxplot(crimes_por_ano$QUANTIDADE)

# Seleciona a coluna de quantidade de crimes
quantidade_crimes <- crimes_por_mes_ano$QUANTIDADE

# Crie um gráfico Q-Q
ggqqplot(quantidade_crimes, 
         title = "Q-Q Plot da Quantidade de Crimes",
         xlab = "Teóricos",
         ylab = "Quantidades de Crimes") +
  theme_minimal()

# Realize o teste de Shapiro-Wilk
shapiro_test <- shapiro.test(quantidade_crimes)

# Mostre o resultado do teste
print(shapiro_test)

#
crimes_por_natureza<- as.data.frame(xtabs(~NATUREZA+ANO, base_cr_hom_trans))

# Frequencia de natureza dos crimes
graf_11 <- ggplot(crimes_por_natureza, aes(x = NATUREZA, y = Freq, label = Freq, fill = NATUREZA)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  geom_label(aes(label = Freq), size = 4, color = "black", fill = "white", fontface = "bold") +  # Adiciona os valores absolutos com caixa de fundo
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Frequência dos crimes por tipo de conduta/natureza",
    x = "Natureza", 
    y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = "right",  # Ajuste da posição da legenda para o topo
    legend.justification = c(1, 1),  # Ajuste da justificativa da legenda para o canto direito superior
    plot.title = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 8, face = "bold")  # Ajusta o tamanho e estilo do texto da legenda
  )
print(graf_11)

# Natureza dos crimes ao longo do tempo
graf_12 <- ggplot(crimes_por_natureza, aes(x = ANO, y = Freq, group = NATUREZA, color = as.factor(NATUREZA))) +
  geom_line(size = 1) +  # Ajuste do tamanho da linha
  geom_point(size = 3) +  # Ajuste do tamanho dos pontos
  geom_label(aes(label = Freq), size = 4, color = "black", fill = "white", fontface = "bold") +  # Adiciona os valores absolutos com caixa de fundo
  theme_minimal() +
  scale_color_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis
  labs(
    title = "Natureza dos crimes contra a comunidade LGBTQIA+ ao longo do tempo",
    x = "Natureza", 
    y = "Número de Crimes",
    color = "Ano"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    plot.title = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto da legenda
    legend.title = element_text(size = 15, face = "bold"), # Ajusta o tamanho e estilo do título da legenda
    axis.title.x = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título do eixo x
    axis.title.y = element_text(size = 15, face = "bold")   # Ajusta o tamanho e estilo do título do eixo y
  )

print(graf_12)


# Cria agrupamentos para facilitar visualizacao grafica e analise
crimes_por_local <- base_cr_hom_trans %>% 
  mutate(LOCAL = case_when(LOCAL == "AEROPORTO" ~ "LOCAL PUBLICO",
                                    LOCAL == 'AEROPORTO,PORTO,RODOVIARIA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'CLUBE' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'ESTADIOS, GINASIOS, ETC.' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'HOSPITAL, CLINICA, ETC' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'ESTACIONAMENTO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'HABITACAO COLETIVA' ~ 'RESIDENCIA',
                                    LOCAL == 'PRACA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'RESIDENCIA PARTICULAR' ~ 'RESIDENCIA',
                                    LOCAL == 'UNIVERSIDADE/FACULDADE' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'METRO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'RODOVIA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'PRAIA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'VAN' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'AMBIENTE VIRTUAL(INTERNET)' ~ 'INTERNET',
                                    LOCAL == 'CONVENTO,IGREJA,ETC.' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'EVENTO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'HOTEL, PENSAO, ETC' ~ 'RESIDENCIA',
                                    LOCAL == 'MOTOTAXI' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'PREDIO EM OBRAS' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'VAN' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'SINDICATO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'VEICULO DE APLICATIVO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'BAR, RESTAURANTE, ETC' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'EDIFICIO PUBLICO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'EXPOCRATO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'INDUSTRIA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'PROPRIEDADE AGRICOLA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'TAXI' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'VIA FERREA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'CAMPO DE FUTEBOL(SUBURBIO)' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'ESCOLA/COLEGIO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'FAVELA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'INST.FINANCEIRA(BANCO,CX ELET)' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'ONIBUS' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'REPARTICAO PUBLICA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'TRANSPORTE ALTERNATIVO' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'VIA PUBLICA' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'CASA COMERCIAL' ~ 'LOCAL PUBLICO',
                                    LOCAL == 'MERCADO PUBLICO, FEIRA.' ~ 'LOCAL PUBLICO',
                           TRUE ~ LOCAL))

# Cria a variavel de crimes por localidade
crimes_por_local<- as.data.frame(xtabs(~LOCAL, crimes_por_local))

# Grafico de numero de crimes por localidade registrada
graf_13 <- ggplot(crimes_por_local, aes(x = LOCAL, y = Freq, label = Freq, fill = LOCAL)) +
  geom_bar(stat = "identity", color = "white") +  # Adiciona contorno branco às barras
  geom_label(aes(label = Freq), size = 4, color = "black", fill = "white", fontface = "bold") +  # Adiciona os valores absolutos com caixa de fundo
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma", direction = -1) +  # Paleta de cores Viridis para preenchimento
  labs(
    title = "Número de crime por localidade",
    x = "Local", 
    y = "Número de Crimes"
  ) +
  theme(
    panel.background = element_rect(fill = "grey", color = NA),  # Fundo do painel cinza
    plot.background = element_rect(fill = "grey", color = NA),   # Fundo do gráfico cinza
    legend.position = "right",  # Ajuste da posição da legenda para o topo
    legend.justification = c(1, 1),  # Ajuste da justificativa da legenda para o canto direito superior
    plot.title = element_text(size = 15, face = "bold"),  # Ajusta o tamanho e estilo do título
    axis.text.x = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo x
    axis.text.y = element_text(size = 10, face = "bold"),  # Ajusta o tamanho e estilo do texto do eixo y
    legend.text = element_text(size = 8, face = "bold")  # Ajusta o tamanho e estilo do texto da legenda
  )
print(graf_13)
