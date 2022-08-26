#    rm(list=ls())
library(tidyverse)
library(foreign)

# _____ compatibilizar processo no formato sigmine p/ o formato Cadastro mineiro ----

#colnames(processos_ANM)[1] <- "Processo"
processos_ANM$processo <- NA
for (i in 1:nrow(processos_ANM)) {
  if (str_count(processos_ANM$Processo[i]) == 8) {
    
    processos_ANM$processo[i] <-
      paste("000", processos_ANM$Processo[i], sep = ".")
    
  }  else if (str_count(processos_ANM$Processo[i]) == 9) {
    processos_ANM$processo[i] <-
      paste(
        paste(
          "00",
          str_sub(
            processos_ANM$Processo[i],
            start = 1,
            end = 1
          ),
          sep = ""
        ),
        str_sub(
          processos_ANM$Processo[i],
          start = 2,
          end = 9
        ),
        sep = "."
      )
  } else if (str_count(processos_ANM$Processo[i]) == 10) {
    processos_ANM$processo[i] <-
      paste(
        paste(
          "0",
          str_sub(
            processos_ANM$Processo[i],
            start = 1,
            end = 2
          ),
          sep = ""
        ),
        str_sub(
          processos_ANM$Processo[i],
          start = 3,
          end = 10
        ),
        sep = "."
      )
  } else {
    if (str_count(processos_ANM$Processo[i]) == 11) {
      processos_ANM$processo[i] <-
        paste(
          paste(
            str_sub(
              processos_ANM$Processo[i],
              start = 1,
              end = 3
            ),
            str_sub(
              processos_ANM$Processo[i],
              start = 4,
              end = 11
            ),
            sep = "."
          ))
    }}}


# # CARREGAMENTO E FORMATAÇÃO ----
# Cessoes_de_Direitos <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Cessoes_de_Direitos.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Guia_de_Utilizacao_Autorizada <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Guia_de_Utilizacao_Autorizada.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Licenciamento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Licenciamento.csv'), header = TRUE, sep = ",", quote = "\"", fill = TRUE, stringsAsFactors = FALSE)
# PLG <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/PLG.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Portaria_de_Lavra <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Portaria_de_Lavra.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Registro_de_Extracao_Publicado <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Registro_de_Extracao_Publicado.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Requerimento_de_Lavra <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Requerimento_de_Lavra.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Requerimento_de_Licenciamento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Requerimento_de_Licenciamento.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Requerimento_de_Pesquisa <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Requerimento_de_Pesquisa.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Requerimento_de_PLG <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Requerimento_de_PLG.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
# Requerimento_de_Registro_de_Extracao_Protocolizado <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/Requerimento_de_Registro_de_Extracao_Protocolizado.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)



 