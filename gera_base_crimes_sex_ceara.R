#### ------------------------------------------------------------------------------------------------------------####
###'
###'
###' MODELAGEM, LIMPEZA E PROCESSAMENTO DOS DADOS SOBRE CRIMES SEXUAIS, DE HOMOFOBIA E TRANSFOBIA NO CEARA
###'
###'
###' AUTOR: ADRIANO NETO, EM 14/05/2023
###' 
###' OBJETIVO: GERAR UMA BASE DE DADOS TRATADOS PARA UMA ANALISE EXPLORATORIA
###' 
###'  -----------------------------------------------------------------------------------------------------------####

# Limpeza
rm(list = ls())


# -------     Pacotes ------
pacotes = c(
  "dplyr",
  "rio",
  "tidyverse",
  "ggplot2",
  "stringr",
  "writexl",
  "sf",
  "readxl",
  "stringi",
  "lubridate",
  "plotly",
  "RColorBrewer"
)

for (x in pacotes) {
  if (!x %in% installed.packages()) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
}

lapply(pacotes, require, character.only = T)

rm(pacotes)

# Caminhos a serem utilizados
caminho<- "C:/Users/Pichau/Desktop/TESTE/"

# Importa as bases a serem utilizadas
# Essas bases se concentram em informacoes sobre as vitimas de crimes sexuais, crimes de transfobia ou homofobia

base_cr_sex <- import(file.path(caminho, "Crimes-Sexuais_2009-a-2023.xlsx"))

base_cr_hom_trans <- import(file.path(caminho, "Homofobia-e-Transfobia.xlsx"))


# ---- Padronizacao da base de dados --------------

# Padronizando variaveis

# Padroniza ajustando as variaveis para letra maiscula 
base_cr_sex <- base_cr_sex %>%
  rename_all(toupper)

base_cr_hom_trans <- base_cr_hom_trans %>%
  rename_all(toupper)

# Faz o mesmo para as observacoes
base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate_all(toupper)

#  Remove acentos dos nomes de variaveis

# Cria funcao de remocao de acentos
rmacento <- function(colns) {
  colns <- stri_trans_general(colns, "Latin-ASCII")
}

# Remove acentos das observacoes da base
base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate(across(everything(), rmacento))

# Remover acentos das variaveis da base
names(base_cr_hom_trans) <- rmacento(names(base_cr_hom_trans))

# Ajustando formato da variavel hora para conter apenas o horario
# e o formato da variavel DATA para 'dia/mes/ano'

# Altera de caracter para formato de data e hora
base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate(
    HORA = format(as.POSIXct(HORA, format = "%Y-%m-%d %H:%M:%S"), format = "%H:%M"),
    DATA = format(as.Date(DATA, format = "%Y-%m-%d"), format = "%d/%m/%Y")
  )

# Altera os separadores de data

# Criar faixas de horario durante o dia para facilitar a analise


# Transforma intervalos de horário por termos indicativos
base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate(
    HORA = case_when(
      between(HORA, "06:00", "11:59") ~ "MANHA",
      between(HORA, "12:00", "17:59") ~ "TARDE",
      between(HORA, "18:00", "23:59") ~ "NOITE",
      between(HORA, "00:00", "05:59") ~ "MADRUGADA",
      TRUE ~ HORA
    )
  )

# Cria variavel MES extraindo a info da variavel DATA
base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate(MES = toupper(month(
    DATA,
    label = TRUE,
    abbr = FALSE,
    locale = "pt_BR.UTF-8"
  ))) %>%
  relocate(MES, .after = `DIA DA SEMANA`) %>% 
  arrange(MES)

# Converte MES em fator para ordenar
base_cr_hom_trans$MES <- factor(
  base_cr_hom_trans$MES,
  levels = c(
    "JANEIRO",
    "FEVEREIRO",
    "MARCO",
    "ABRIL",
    "MAIO",
    "JUNHO",
    "JULHO",
    "AGOSTO",
    "SETEMBRO",
    "OUTUBRO",
    "NOVEMBRO",
    "DEZEMBRO"
  ),
  ordered = TRUE
)

# Ordena as observacoes por cronologia do ano
base_cr_hom_trans <- base_cr_hom_trans %>% 
  arrange(MES)

# Cria faixa etaria para facilitar a analise posterior

intervalos <- c(0, 11, 18, 29, 49, 64, Inf)

base_cr_hom_trans <- base_cr_hom_trans %>%
  mutate(FAIXA_ETARIA = cut(
    as.numeric(`IDADE DA VITIMA`),
    breaks = intervalos,
    labels = c("01-12", "12-18", "18-29", "30-49", "50-64", "65+"),
    include.lowest = TRUE
  )) %>%
  relocate(FAIXA_ETARIA, .after = DATA)

# Padronizacao dos nomes da variaveis
base_cr_hom_trans <- base_cr_hom_trans %>%
  rename(
    DIA_SEMANA = "DIA DA SEMANA",
    ID_GEN = "IDENTIDADE DE GENERO",
    ORIENT_SEX = "ORIENTACAO SEXUAL",
    V_IDADE = "IDADE DA VITIMA",
    V_ESCOLARIDADE = "ESCOLARIDADE DA VITIMA",
    V_RACA = "RACA DA VITIMA"
  )


# Criando A variável ANO a partir da variável DATA

base_cr_hom_trans<-base_cr_hom_trans %>% 
  mutate(ANO = year(as.Date(DATA, format = "%d/%m/%Y"))) %>% 
  relocate(ANO, .after = MES)

# Exemplo de gráfico de linhas para o número de ocorrências ao longo dos anos
ocorrencias_por_ano <- base_cr_hom_trans %>%
  group_by(ANO) %>%
  summarise(Contagem = n())

ggplot(ocorrencias_por_ano, aes(x = ANO, y = Contagem)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Ocorrências por Ano",
       x = "Ano",
       y = "Contagem") +
  theme_minimal()

# Converte um gráfico ggplot2 para plotly
graf_1<- ggplot(base_cr_hom_trans, aes(x = MES, fill = HORA)) +
  geom_bar(position = "stack") +
  labs(title = "Ocorrência Mensal de crimes de homofobia e transfobia",
       x = "Mês",
       y = "Quantidade",
       fill = "Hora do dia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(graf_1)

# Ordenar os dias da semana
base_cr_hom_trans$DIA_SEMANA <- factor(base_cr_hom_trans$DIA_SEMANA, 
                                       levels = c("DOMINGO", "SEGUNDA", "TERCA", "QUARTA", "QUINTA", "SEXTA", "SABADO"))
# graf 2
graf_2 <- ggplot(base_cr_hom_trans, aes(x = DIA_SEMANA)) +
  geom_bar(position = "stack") +
  labs(title = "Distribuição diária dos crimes de homofobia e transfobia",
       x = "Dia da Semana",
       y = "Quantidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(graf_2)

# Utilizando o rcolorbrewer para usar paletas de cores predeterminadas
graf_3 <- ggplot(base_cr_hom_trans, aes(x = DIA_SEMANA, fill = DIA_SEMANA)) +
  geom_bar(position = "stack") +
  labs(title = "Distribuição diária dos crimes de homofobia e transfobia",
       x = "Dia da Semana",
       y = "Quantidade") +
  scale_fill_brewer(palette = "Set3") + # Escolha a paleta de cores que preferir
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(graf_3)

# TODOS OS GRAFICOS ABAIXO PRECISAO DE REVISAO E SAO APENAS TEMPORARIOS
# # Gráfico de barras da quantidade de ocorrências por mês
# ggplot(base_cr_hom_trans, aes(x = MES)) +
#   geom_bar(fill = "skyblue", color = "black") +
#   labs(title = "Ocorrências por Mês",
#        x = "Mês",
#        y = "Contagem") +
#   theme_minimal()
# 
# # Gráfico de dispersão da idade das vítimas
# ggplot(base_cr_hom_trans, aes(x = V_IDADE, y = outra_variavel_numerica)) +
#   geom_point(color = "darkgreen") +
#   labs(title = "Idade das Vítimas vs. Outra Variável",
#        x = "Idade da Vítima",
#        y = "Outra Variável") +
#   theme_minimal()
# 
# 
# # Supondo que você tenha uma coluna chamada "latitude" e outra "longitude" em seu dataframe
# # Converta suas coordenadas em um objeto sf
# coords <- st_as_sf(base_cr_hom_trans, coords = c("longitude", "latitude"), crs = 4326)
# 
# # Crie um mapa básico de pontos
# ggplot() +
#   geom_sf(data = coords) +
#   labs(title = "Distribuição Espacial dos Dados",
#        caption = "Fonte: Seus Dados") +
#   theme_minimal()
