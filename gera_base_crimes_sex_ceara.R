#### -----------------------------------------SCRIPT DE MODELAGEM DE DADOS ------------------------------------####
###'
###'
###' MODELAGEM, LIMPEZA E PROCESSAMENTO DOS DADOS SOBRE CRIMES SEXUAIS, DE HOMOFOBIA E TRANSFOBIA NO CEARA
###'
###'
###' AUTOR: ADRIANO NETO, EM 14/05/2023
###' 
###' OBJETIVO: GERAR UMA BASE DE DADOS TRATADOS PARA UMA ANALISE EXPLORATORIA
###' 
###'-----------------------------------------------------------------------------------------------------------

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
  "RColorBrewer",
  "geobr",
  "report",
  "viridis"
)

for (x in pacotes) {
  if (!x %in% installed.packages()) {
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
}

lapply(pacotes, require, character.only = T)

rm(pacotes)

# Caminhos a serem utilizados
caminho<- "C:\\Users\\super\\Downloads\\"

# Importa as bases a serem utilizadas
# Essas bases se concentram em informacoes sobre as vitimas de crimes sexuais, crimes de transfobia ou homofobia

base_cr_sex <- import(file.path(caminho, "Crimes-Sexuais_2009-a-2023.xlsx"))

base_cr_hom_trans <- import(file.path(caminho, "Homofobia-e-Transfobia.xlsx"))

# Cria versoes originais das bases antes de qualquer alteracao
base_cr_h_t_og<-base_cr_hom_trans
base_cr_sex_og<-base_cr_sex

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

# Cria função de remoção de acentos e substituição de caracteres especiais
rmacento <- function(colns) {
  # Transforma caracteres latinos em ASCII
  colns <- stri_trans_general(colns, "Latin-ASCII")
  
  # Substitui caracteres especiais
  colns <- gsub("ç", "c", colns)
  colns <- gsub("Ç", "C", colns)
  # Adicione outras substituições se necessário, por exemplo:
  # colns <- gsub("ß", "ss", colns)
  # colns <- gsub("ñ", "n", colns)
  
  return(colns)
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

# A informacao dos meses pelo pacote lubridate gera informacoes de meses com caracteres especiais
# tipo o mes de março vem com 'ç', dessa forma foi necessario remover novamente com a funcao rmacento

# Remover acentos das variaveis da base
names(base_cr_hom_trans) <- rmacento(names(base_cr_hom_trans))

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


# ----- Estruturando dados espaciais para analise espacial ---------

# Importa dados sobre os municipios do estado do Ceara
municipios_ce <- read_municipality(code_muni = "CE", year = 2020)

# Calcula a quantidade de crimes por municipio
crimes_por_municipio <- base_cr_hom_trans %>%
  group_by(MUNICIPIO) %>%
  summarise(QUANTIDADE = n())

## Padronizando informacoes

# Transforma observacoes para maiusculo
municipios_ce <- municipios_ce %>%
  mutate_at(vars(setdiff(names(.), c("code_region", "geom"))), toupper)

# Remove acentos e caracteres especiais
municipios_ce <- municipios_ce %>%
  mutate(across(!geom, rmacento))


# Une  as bases de dados de crimes com a base de dados espaciais
mapa_crimes <- left_join(municipios_ce, crimes_por_municipio, by = c("name_muni" = "MUNICIPIO"))
