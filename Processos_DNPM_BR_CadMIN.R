#    rm(list=ls())
library(tidyverse)
library(foreign)

# CARREGAMENTO E FORMATAÇÃO ----
Cessoes_de_Direitos <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Cessoes_de_Direitos.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Guia_de_Utilizacao_Autorizada <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Guia_de_Utilizacao_Autorizada.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Licenciamento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Licenciamento.csv'), header = TRUE, sep = ",", quote = "\"", fill = TRUE, stringsAsFactors = FALSE)
PLG <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/PLG.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Portaria_de_Lavra <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Portaria_de_Lavra.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Registro_de_Extracao_Publicado <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Registro_de_Extracao_Publicado.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Requerimento_de_Lavra <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Requerimento_de_Lavra.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Requerimento_de_Licenciamento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Requerimento_de_Licenciamento.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Requerimento_de_Pesquisa <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Requerimento_de_Pesquisa.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Requerimento_de_PLG <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Requerimento_de_PLG.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Requerimento_de_Registro_de_Extracao_Protocolizado <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/CSV_Data/Dados_Abertos_ANM/Requerimento_de_Registro_de_Extracao_Protocolizado.csv'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)


# _____ Join das fases processuais ----
lista_dnpm <- #Licenciamento
  rbind(Guia_de_Utilizacao_Autorizada, rbind(Licenciamento, rbind(PLG, rbind(
    Portaria_de_Lavra,
#    rbind(
      Registro_de_Extracao_Publicado #,
#      rbind(
#        Requerimento_de_Lavra,
#        rbind(
#          Requerimento_de_Licenciamento,
#          rbind(
#            Requerimento_de_Pesquisa,
#            rbind(
#              Requerimento_de_PLG,
#              Requerimento_de_Registro_de_Extracao_Protocolizado
            )
          )
        )
      )
#    )
#  ))))

# _____ compatibilizar processo no formato sigmine p/ o formato Cadastro mineiro ----

#colnames(lista_dnpm)[1] <- "Processo"
lista_dnpm$processo <- NA
for (i in 1:nrow(lista_dnpm)) {
  if (str_count(lista_dnpm$Processo[i]) == 8) {
    
    lista_dnpm$processo[i] <-
      paste("000", lista_dnpm$Processo[i], sep = ".")
    
  }  else if (str_count(lista_dnpm$Processo[i]) == 9) {
    lista_dnpm$processo[i] <-
      paste(
        paste(
          "00",
          str_sub(
            lista_dnpm$Processo[i],
            start = 1,
            end = 1
          ),
          sep = ""
        ),
        str_sub(
          lista_dnpm$Processo[i],
          start = 2,
          end = 9
        ),
        sep = "."
      )
  } else if (str_count(lista_dnpm$Processo[i]) == 10) {
    lista_dnpm$processo[i] <-
      paste(
        paste(
          "0",
          str_sub(
            lista_dnpm$Processo[i],
            start = 1,
            end = 2
          ),
          sep = ""
        ),
        str_sub(
          lista_dnpm$Processo[i],
          start = 3,
          end = 10
        ),
        sep = "."
      )
  } else {
    if (str_count(lista_dnpm$Processo[i]) == 11) {
      lista_dnpm$processo[i] <-
        paste(
          paste(
            str_sub(
              lista_dnpm$Processo[i],
              start = 1,
              end = 3
            ),
            str_sub(
              lista_dnpm$Processo[i],
              start = 4,
              end = 11
            ),
            sep = "."
          ))
    }}}


# Delimitando Fases de interesse ----
DNPM_alvos <-
  lista_dnpm[lista_dnpm$Fase.Atual %in% c(
#    "Guia_de_Utilizacao_Autorizada",
    "Licenciamento"#,
#    "PLG",
#    "Portaria_de_Lavra",
#    "Registro_de_Extracao_Publicado"#,
#    "Requerimento_de_Lavra",
 #   "Requerimento_de_Licenciamento",
#    "Requerimento_de_Pesquisa",
 #   "Requerimento_de_PLG",
 #   "Requerimento_de_Registro_de_Extracao_Protocolizado"
  ), ]
  

# processos alvo ---

DNPM_alvos <-
  DNPM_alvos[grepl(DNPM_alvos$Municipio.s., pattern = "- SP"), ]

#DNPM_ativos[DNPM_ativos$Processo=="821030/2001",]
SP_dbf <-
  foreign::read.dbf(
    'D:/Users/humberto.serna/Desktop/Georeferenciamento/Poligonais/SP/SP.dbf',
    as.is = FALSE)
 



SP <- 
  merge(SP_dbf,DNPM_alvos[,-c(1,3)], by.x = "PROCESSO", by.y = "Processo")
colnames(SP)[12] <- "cpfcnpj"
# há dois registros duplicados. Execute o anexo.


SP$ativo <- 1
for (i in 1:nrow(SP)) {
  if (is.na(SP$cpfcnpj[i]) == TRUE) {
    SP$cpfcnpj[i] <- 0
    SP$ativo[i] <- 0
    }}

write.dbf(SP_dbf, file = "D:/Users/humberto.serna/Desktop/Georeferenciamento/Poligonais/SP_Copia/SP.dbf")




write.table(x = DNPM_alvos, file = 'clipboard', )



# anexo









