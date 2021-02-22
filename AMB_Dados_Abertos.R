#   rm(list = ls())
#   options(editor = "notepad")
library(tidyverse)
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/graficos_AMB.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_Producao.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_VPM.R")

# CARREGAMENTO ----
#_____Bruta ----
producao_bruta <-
  read.table(
    "D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Producao_Bruta.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "iso-8859-1",
    colClasses = c("character")
  )

# colunas 
colnames(producao_bruta) <- 
  c("Ano", "UF", "Classe.Substancia", "Substancia", 
    "Quantidade.Producao.minerio", "Quantidade.Contido", 
    "Unidade.Contido", "Indicacao.Contido", "Quantidade.Venda", 
    "Valor.Venda", "Quantidade.Transformacao.Consumo.Utilizacao", 
    "Valor.Transformacao.Consumo.Utilizacao", 
    "Quantidade.Transferencia.Transformacao.Utilizacao.Consumo", 
    "Valor.Transferencia.Transformacao.Utilizacao.Consumo")

colnames(producao_bruta) <- 
  FUNA_minusculas(
  FUNA_removeAcentos(colnames(producao_bruta)))


#__________Transformando colunas quantitativas de CHARACTER >> NUMERIC ----

producao_bruta$quantidade.producao.minerio <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$quantidade.producao.minerio))

producao_bruta$quantidade.contido <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$quantidade.contido))

producao_bruta$quantidade.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$quantidade.venda))

producao_bruta$valor.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$valor.venda))

producao_bruta$quantidade.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$quantidade.transformacao.consumo.utilizacao))

producao_bruta$valor.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$valor.transformacao.consumo.utilizacao))

producao_bruta$quantidade.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$quantidade.transferencia.transformacao.utilizacao.consumo))

producao_bruta$valor.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_bruta$valor.transferencia.transformacao.utilizacao.consumo))


#_____Beneficiada ----

producao_beneficiada <-
  read.table(
    "D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Producao_Beneficiada.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "iso-8859-1",
    colClasses = c("character")
  )


# colunas 
colnames(producao_beneficiada) <- 
  c(
    "Ano","UF","Classe.Substância","Substância","Quantidade.Produção",
    "Unidade.de.Medida.Produção","Quantidade.Contido","Unidade.Contido",
    "Indicação.Contido","Quantidade.Venda","Unidade.de.Medida.Venda",
    "Valor.Venda","Quantidade.Consumo/Utilização.na.Usina",
    "Unidade.de.Medida.Consumo/Utilização.na.Usina",
    "Valor.Consumo./.Utilização.na.Usina",
    "Quantidade.Transferência.para.Transformação./.Utilização./.Consumo",
    "Unidade.de.Medida.Transferência.para.Transformação./.Utilização./.Consumo",
    "Valor.Transferência.para.Transformação./.Utilização./.Consumo.")



colnames(producao_beneficiada) <- 
  FUNA_minusculas(
    FUNA_removeAcentos(colnames(producao_beneficiada)))


#__________Transformando colunas quantitativas de CHARACTER >> NUMERIC ----

producao_beneficiada$quantidade.producao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$quantidade.producao))

producao_beneficiada$quantidade.contido <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$quantidade.contido))

producao_beneficiada$quantidade.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$quantidade.venda))

producao_beneficiada$valor.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$valor.venda))

producao_beneficiada$quantidade.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$quantidade.transformacao.consumo.utilizacao))

producao_beneficiada$valor.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$valor.transformacao.consumo.utilizacao))

producao_beneficiada$quantidade.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$quantidade.transferencia.transformacao.utilizacao.consumo))

producao_beneficiada$valor.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producao_beneficiada$valor.transferencia.transformacao.utilizacao.consumo))


# visões ----

# Substância - UF 

# producao_bruta
df <-
  summarise(
    group_by(.data =
               filter(.data = producao_bruta, substancia == "Manganês", ano %in% c(2015,2019)), ano, uf),
    "quantidade_Venda" = sum(quantidade.venda),
    "valor(R$)" = sum(valor.venda),
    "ROM" = sum(quantidade.producao.minerio),
    "Quantidade_Contido" = sum(quantidade.contido),
    "unidade_contido" = unique(unidade.contido))


# producao_beneficiada
df <-
  summarise(
    group_by(.data =
               filter(.data = producao_beneficiada, substancia == "Manganês", ano %in% c(2015,2019)), ano, uf),
    "quantidade_Venda" = sum(quantidade.venda),
    "valor(R$)" = sum(valor.venda),
    "quantidade.producao" = sum(quantidade.producao),
    "Quantidade_Contido" = sum(quantidade.contido),
    "unidade_contido" = unique(unidade.contido))



# write.table(df, file = "clipboard", sep = "\t", na = "", row.names = FALSE, dec = ",")


 










