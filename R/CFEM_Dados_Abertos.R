#       rm(list = ls())

# if (!require(svDialogs)) { 
#   install.packages('svDialogs') 
#   library(svDialogs) 
# }
options(editor = 'notepad')
library(tidyverse)
library(lubridate)
library(svDialogs)
source('./R/geocod.R')


# Carregamento ----
CFEM_Arrecadacao <- #fonte: Dados Abertos
  read.table(
    file = "./data/CFEM_Arrecadacao.csv",
    # file = "https://app.anm.gov.br/DadosAbertos/ARRECADACAO/CFEM_Arrecadacao.csv",
    sep = ";", dec = ',', quote = "\"",
    fileEncoding = "Latin1", 
    skip = 1418021,
    colClasses = c(
      "character",         #  Ano        
      "character",         #  Mês        
      "character",       #  Processo        
      "character",       #  AnoDoProcesso        
      "character",       #  Tipo_PF_PJ        
      "character",       #  CPF_CNPJ        
      "character",       #  Substância        
      "character",       #  UF        
      "character",       #  Município        
      "character",          #  QuantidadeComercializada        
      "character",       #  UnidadeDeMedida        
      "character"           #  ValorRecolhido
    ))


colnames(CFEM_Arrecadacao) <- 
  c("Ano", "Mês", "Processo", "AnoDoProcesso", "Tipo_PF_PJ", "CPF_CNPJ", 
    "Substância", "UF", "Município", "QuantidadeComercializada", 
    "UnidadeDeMedida", "ValorRecolhido")



# dlgMessage(rstudio = FALSE,
#   c('    RECOLHIMENTOS DA CFEM', " ","    Escolha os anos a seguir")
# )
# ANOS <-
#   dlgList(rstudio = FALSE,
#           choices = c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
#       "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", 
#       "2018", "2019", "2020", "2021"),
#     multiple = TRUE,
#     title = c("CTRL+mouse p/ diversos")
#   )$res

# CFEM_Arrecadacao <- 
#   CFEM_Arrecadacao[CFEM_Arrecadacao$Ano %in% ANOS,]

# _____________________________________________________________________________________________________________________________


# Ajuste de Classe

CFEM_Arrecadacao$Ano <- 
  as.integer(CFEM_Arrecadacao$Ano) |> parse_date_time(orders = "%Y") |> year()

CFEM_Arrecadacao$Mês <- 
  as.integer(CFEM_Arrecadacao$Mês) 

CFEM_Arrecadacao$QuantidadeComercializada <- 
  as.numeric(gsub(
    str_squish(CFEM_Arrecadacao$QuantidadeComercializada), 
    pattern = ",", replacement = "."))

CFEM_Arrecadacao$ValorRecolhido <- 
  as.numeric(gsub(
    str_squish(CFEM_Arrecadacao$ValorRecolhido), 
    pattern = ",", replacement = "."))

# Ajuste de Processo

# 
# df <- CFEM_Arrecadacao[,c(3,4)]
# colnames(df) <- c("processo","ano")
# 
# FUNA_processo_sigmine_SCM(df = df)

CFEM_Arrecadacao$Processo <- 
  paste(CFEM_Arrecadacao$Processo, CFEM_Arrecadacao$AnoDoProcesso, sep = "/")



# id Municipio_UF
CFEM_Arrecadacao$Municipio_UF <- 
  paste(
    str_squish(CFEM_Arrecadacao$Município),
    CFEM_Arrecadacao$UF, sep = "/")

# grandezas
CFEM_Arrecadacao$QuantidadeComercializada <- 
  as.numeric(gsub(str_squish(CFEM_Arrecadacao$QuantidadeComercializada), pattern = ",", replacement = "."))

CFEM_Arrecadacao$ValorRecolhido <- 
  as.numeric(gsub(str_squish(CFEM_Arrecadacao$ValorRecolhido), pattern = ",", replacement = "."))


# Formata??o e ajustes
CFEM_Arrecadacao$Município <- 
  CFEM_Arrecadacao$Município |> str_squish()

CFEM_Arrecadacao$mun_chave_primaria <- 
  CFEM_Arrecadacao$Municipio_UF |> iconv(to = "ASCII//TRANSLIT") |> tolower()

geocod$mun_chave_primaria <- 
  paste(geocod$Município, geocod$UF_sigla, sep = "/") |> 
  str_squish() |> iconv(to = "ASCII//TRANSLIT") |> tolower()


# CFEM_Arrecadacao <- 
#   left_join(CFEM_Arrecadacao, geocod[,c(12,8)], by = c("mun_chave_primaria"))



CFEM_Arrecadacao$UF <- 
  str_squish(CFEM_Arrecadacao$UF)

CFEM_Arrecadacao$UnidadeDeMedida <- 
  str_squish(CFEM_Arrecadacao$UnidadeDeMedida)

CFEM_Arrecadacao$Substância <- 
  CFEM_Arrecadacao$Substância |> str_squish() 

# CFEM_Arrecadacao$substancia.AMB <- 
#   CFEM_Arrecadacao$substancia.SCM
# 
# CFEM_Arrecadacao$substancia.AMB <- 
#   CFEM_Arrecadacao$substancia.AMB |> FUNA_minusculas()


# ____ impondo mês.ANO
CFEM_Arrecadacao$Período <-
  as_date(paste(
    CFEM_Arrecadacao$Ano,
    month(as.integer(CFEM_Arrecadacao$Mês), label = TRUE),
    "1",
    sep = "-"
  )) 

# ____ impondo trimestre

CFEM_Arrecadacao$trimestre <-
  lubridate::quarter(
    lubridate::ymd(
      CFEM_Arrecadacao$Período), with_year = TRUE)


# ____ impondo semestre
CFEM_Arrecadacao$semestre <-
  lubridate::semester(
    lubridate::ymd(
      CFEM_Arrecadacao$Período), with_year = TRUE)


  saveRDS(CFEM_Arrecadacao,'./data/CFEM_Arrecadacao.Rds')