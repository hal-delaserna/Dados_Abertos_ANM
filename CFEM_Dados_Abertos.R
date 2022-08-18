#       rm(list = ls())

if (!require(svDialogs)) { 
  install.packages('svDialogs') 
  library(svDialogs) 
}
options(editor = 'notepad')
library(tidyverse)
library(lubridate)
library(svDialogs)

 source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.r')
 source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/graficos_AMB.r')
 source('D:/Users/humberto.serna/Documents/D_Lake/Dados_Abertos_ANM/Funcoes_CFEM.R')
 source('D:/Users/humberto.serna/Documents/D_Lake/geocod.R')



# Carregamento ----
cfem_BR <- #fonte: Dados Abertos
  read.table(
    file = "./D_Lake/Dados_Abertos_ANM/ARRECADACAO/CFEM_Arrecadacao.csv",
    # file = "https://app.anm.gov.br/DadosAbertos/ARRECADACAO/CFEM_Arrecadacao.csv",
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    dec = ',',
    quote = "\"",
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


dlgMessage(rstudio = FALSE,
  c('    RECOLHIMENTOS DA CFEM', " ","    Escolha os anos a seguir")
)
ANOS <-
  dlgList(rstudio = FALSE,
          choices = c("2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
      "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", 
      "2018", "2019", "2020", "2021"),
    multiple = TRUE,
    title = c("CTRL+mouse p/ diversos")
  )$res

cfem_BR <- 
  cfem_BR[cfem_BR$Ano %in% ANOS,]

# _____________________________________________________________________________________________________________________________


# Ajuste de Classe

cfem_BR$Ano <- 
  as.integer(cfem_BR$Ano)

cfem_BR$Mês <- 
  as.integer(cfem_BR$Mês)

cfem_BR$QuantidadeComercializada <- 
  as.numeric(gsub(
    str_squish(cfem_BR$QuantidadeComercializada), 
    pattern = ",", replacement = "."))

cfem_BR$ValorRecolhido <- 
  as.numeric(gsub(
    str_squish(cfem_BR$ValorRecolhido), 
    pattern = ",", replacement = "."))

# Ajuste de Processo

# 
# df <- cfem_BR[,c(3,4)]
# colnames(df) <- c("processo","ano")
# 
# FUNA_processo_sigmine_SCM(df = df)

cfem_BR$Processo <- 
  paste(cfem_BR$Processo, cfem_BR$AnoDoProcesso, sep = "/")



# id Municipio_UF
cfem_BR$Municipio_UF <- 
  paste(
    str_squish(cfem_BR$Município),
    cfem_BR$UF, sep = "/")

# grandezas
cfem_BR$QuantidadeComercializada <- 
  as.numeric(gsub(str_squish(cfem_BR$QuantidadeComercializada), pattern = ",", replacement = "."))

cfem_BR$ValorRecolhido <- 
  as.numeric(gsub(str_squish(cfem_BR$ValorRecolhido), pattern = ",", replacement = "."))


# Formatação e ajustes
cfem_BR$Município <- 
  cfem_BR$Município %>% str_squish()

cfem_BR$mun_chave_primaria <- 
  cfem_BR$Municipio_UF %>% FUNA_removeAcentos() %>% FUNA_minusculas()

geocod$mun_chave_primaria <- 
  paste(geocod$Município, geocod$UF_sigla, sep = "/") %>% 
  str_squish() %>% FUNA_removeAcentos() %>% FUNA_minusculas()


# cfem_BR <- 
#   left_join(cfem_BR, geocod[,c(12,8)], by = c("mun_chave_primaria"))



cfem_BR$UF <- 
  str_squish(cfem_BR$UF)

cfem_BR$UnidadeDeMedida <- 
  str_squish(cfem_BR$UnidadeDeMedida)

cfem_BR$Substância <- 
  cfem_BR$Substância %>% str_squish() 

# cfem_BR$substancia.AMB <- 
#   cfem_BR$substancia.SCM
# 
# cfem_BR$substancia.AMB <- 
#   cfem_BR$substancia.AMB %>% FUNA_minusculas()


# ____ impondo mês.ANO
cfem_BR$Período <-
  as_date(
    paste(cfem_BR$Ano, month(as.integer(cfem_BR$Mês), label = TRUE),"1",   sep = "-")
    ) 

# ____ impondo trimestre

cfem_BR$trimestre <-
  lubridate::quarter(
    lubridate::ymd(
      cfem_BR$Período), with_year = TRUE)


# ____ impondo semestre
cfem_BR$semestre <-
  lubridate::semester(
    lubridate::ymd(
      cfem_BR$Período), with_year = TRUE)


 # saveRDS(cfem_BR[,-c(2, 14)],'D:/Users/humberto.serna/Documents/D_Lake/CFEM_Dados_Abertos.RDATA')