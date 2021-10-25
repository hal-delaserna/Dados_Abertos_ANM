
#       rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)

source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.r')
source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/graficos_AMB.r')
source('D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Funcoes_CFEM.R')


# Carregamento ----
cfem_BR <- #fonte: Dados Abertos
  read.table(
    file = paste(Sys.getenv("R_USER"), '/CSV_Data/Cfem.csv', sep = ""),
    header = FALSE,
    skip = 731220, # ano 2016 em diante
    sep = ",",
    #fill = TRUE,
    stringsAsFactors = FALSE,
    dec = ',',
    quote = "\"",
    col.names = c("periodo", "mes.de.referencia", "processo", "ano.do.Processo", "tipo.PF.PJ", "cpfcnpj", "titular", 
                  "fase.do.Processo", "substancia.SCM", "uf", "municipio", "unidade.de.Medida", 
                  "quantidade.Comercializada", "valor.Recolhido.CFEM"),
    colClasses = c(processo = "character", ano.do.Processo = "character")
    )

# ____ carregando alíquotas
aliquota <-
  read.table(
    file = paste(Sys.getenv("R_USER"), '/CSV_Data/cfem_aliquotas.csv', sep = ""),
    header = TRUE,sep = ";",stringsAsFactors = FALSE,dec = ',')

# ____ matriz alíquotas
matriz_aliquotas <- 
  read.table(
    file = paste(Sys.getenv("R_USER"), '/CSV_Data/cfem_matriz_alíquotas.csv', sep = ""),
    header = TRUE,sep = ";",stringsAsFactors = FALSE)

# ____ carregando IGP-DI
IGP_DI <-
  read.table(
    file = paste(Sys.getenv("R_USER"), '/CSV_Data/IGP_DI.csv', sep = ""),
    header = TRUE,sep = ";",stringsAsFactors = FALSE, dec = ',')

# _____________________________________________________________________________________________________________________________

# ____ impondo trimestre
cfem_BR$trimestre <-
  lubridate::quarter(
    lubridate::ymd(
    paste(cfem_BR$periodo, cfem_BR$mes.de.referencia, "1", sep = "_")
  ), with_year = TRUE)


# ____ impondo semestre
cfem_BR$semestre <-
  lubridate::semester(
    lubridate::ymd(
      paste(cfem_BR$periodo, cfem_BR$mes.de.referencia, "1", sep = "_")
    ), with_year = TRUE)


# ____ impondo mês.ANO
cfem_BR$mes.de.referencia <-
  as_date(paste(cfem_BR$periodo, month(cfem_BR$mes.de.referencia, label = TRUE),"1",   sep = "-")) 
    

# Formatação e ajustes
cfem_BR$municipio <- 
  cfem_BR$municipio %>% str_squish() %>% FUNA_removeAcentos() %>% FUNA_minusculas()

cfem_BR$uf <- 
  str_squish(cfem_BR$uf)

cfem_BR$unidade.de.Medida <- 
  str_squish(cfem_BR$unidade.de.Medida)

cfem_BR$substancia.SCM <- 
  cfem_BR$substancia.SCM %>% str_squish() %>% FUNA_removeAcentos() %>% gsub(pattern = ";", replacement = "") %>% FUNA_minusculas()

# id Municipio_UF
cfem_BR$municipio_UF <- 
  paste(cfem_BR$municipio, cfem_BR$uf, sep = "_")

# _____ Ajuste de Razão Social ----
cfem_BR$titular <-
  cfem_BR$titular %>% gsub(pattern = "\\.", replacement = " ") %>% gsub(pattern = "-", replacement = " ") %>% gsub(pattern = "/", replacement = " ") %>% FUNA_removeAcentos() %>% FUNA_minusculas()
cfem_BR$titular <-
  cfem_BR$titular %>%   
  gsub(pattern = "l t d a", replacement = 'ltda') %>%   
  gsub(pattern = "d t v m", replacement = "dtvm") %>%   
  gsub(pattern = "l t d", replacement = 'ltda') %>%  
  gsub(pattern = "ltd", replacement = 'ltda') %>%  
  gsub(pattern = "ltdaa", replacement = 'ltda') %>%   
  gsub(pattern = "dtm", replacement = 'dtvm') %>%   
  gsub(pattern = " s a$", replacement = ' sa') %>%   
  gsub(pattern = " s a ", replacement = ' sa ') %>%   
  gsub(pattern = "pedsras", replacement = "pedras") %>% 
  stringr::str_squish()

# __________ Ouro DTVM
cfem_BR[cfem_BR$cpfcnpj == "08.673.569/0001-20",]$titular  <- c("fd gold dtvm ltda")
cfem_BR[cfem_BR$cpfcnpj == "11.495.073/0001-18",]$titular  <- c("om dtvm ltda")
cfem_BR[cfem_BR$cpfcnpj == "00.460.065/0001-10",]$titular  <- c("coluna sa dtvm")	
cfem_BR[cfem_BR$cpfcnpj == "62.237.649/0001-88",]$titular  <- c("carol dtvm")	

# Ajuste de Substâncias ----
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.SCM[i], pattern = "agua mineral|potavel|termal|termais")) {
    cfem_BR$substancia.SCM[i] <- c("agua mineral")
  }
}
  

# AREIA ----
# _____ ajuste de unidades de massa e volume (tentativa de) ----
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia[i], pattern = "areia")) {
    
    if (cfem_BR$unidade.de.Medida[i] == "m3") {
      
      cfem_BR$quantidade.Comercializada[i] <-
        1.64 * cfem_BR$quantidade.Comercializada[i]
      
      cfem_BR$unidade.de.Medida[i] <- "m3-> t"
      
    } else if (cfem_BR$unidade.de.Medida[i] == "kg") {
      cfem_BR$quantidade.Comercializada[i] <-
        1000 * cfem_BR$quantidade.Comercializada[i]
      
      cfem_BR$unidade.de.Medida[i] <- "kg-> t"
    }
  }
}


# Exportar ---- 

save(cfem_BR, file = "./CSV_Data/cfem_BR.RData")
