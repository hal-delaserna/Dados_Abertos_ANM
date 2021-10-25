#   rm(list = ls())
options(editor = "notepad")
library(tidyverse)
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/graficos_AMB.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_Producao.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_VPM.R")

# CARREGAMENTO ----
#_____Bruta ----
# producao_bruta <-
producaoBRUTA <-
  read.table(
    "https://app.anm.gov.br/DadosAbertos/AMB/Producao_Bruta.csv",
    #"D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Producao_Bruta.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "iso-8859-1",
    colClasses = c("character")
  )

# colunas 
colnames(producaoBRUTA) <- 
  c("Ano", "UF", "Classe.Substancia", "Substancia", 
    "Quantidade.Producao.minerio", "Quantidade.Contido", 
    "Unidade.Contido", "Indicacao.Contido", "Quantidade.Venda", 
    "Valor.Venda", "Quantidade.Transformacao.Consumo.Utilizacao", 
    "Valor.Transformacao.Consumo.Utilizacao", 
    "Quantidade.Transferencia.Transformacao.Utilizacao.Consumo", 
    "Valor.Transferencia.Transformacao.Utilizacao.Consumo")

colnames(producaoBRUTA) <- 
  tolower(
    FUNA_removeAcentos(colnames(producaoBRUTA)))


#__________Transformando colunas quantitativas de CHARACTER >> NUMERIC ----

producaoBRUTA$ano <- 
  as.integer(producaoBRUTA$ano)

producaoBRUTA$quantidade.producao.minerio <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$quantidade.producao.minerio))

producaoBRUTA$quantidade.contido <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$quantidade.contido))

producaoBRUTA$quantidade.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$quantidade.venda))

producaoBRUTA$valor.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$valor.venda))

producaoBRUTA$quantidade.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$quantidade.transformacao.consumo.utilizacao))

producaoBRUTA$valor.transformacao.consumo.utilizacao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$valor.transformacao.consumo.utilizacao))

producaoBRUTA$quantidade.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$quantidade.transferencia.transformacao.utilizacao.consumo))

producaoBRUTA$valor.transferencia.transformacao.utilizacao.consumo <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBRUTA$valor.transferencia.transformacao.utilizacao.consumo))


#_____Beneficiada ----

# producao_beneficiada <-
producaoBENEFICIADA <-
  read.table(
    "https://app.anm.gov.br/DadosAbertos/AMB/Producao_Beneficiada.csv",
    #"D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Producao_Beneficiada.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "iso-8859-1",
    colClasses = c("character")
  )


# colunas 
colnames(producaoBENEFICIADA) <- 
  c(
    "Ano","UF","Classe.Substância","Substância","Quantidade.Produção",
    "Unidade.de.Medida.Produção","Quantidade.Contido","Unidade.Contido",
    "Indicação.Contido","Quantidade.Venda","Unidade.de.Medida.Venda",
    "Valor.Venda","Quantidade.Consumo.Utilização.na.Usina",
    "Unidade.de.Medida.Consumo.Utilização.na.Usina",
    "Valor.Consumo.Utilização.na.Usina",
    "Quantidade.Transferência.para.Transformação.Utilização.Consumo",
    "Unidade.de.Medida.Transferência.para.Transformação.Utilização.Consumo",
    "Valor.Transferência.para.Transformação.Utilização.Consumo")



colnames(producaoBENEFICIADA) <- 
  tolower(
    FUNA_removeAcentos(colnames(producaoBENEFICIADA)))


#__________Transformando colunas quantitativas de CHARACTER >> NUMERIC ----

producaoBENEFICIADA$quantidade.producao <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$quantidade.producao))

producaoBENEFICIADA$quantidade.contido <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$quantidade.contido))

producaoBENEFICIADA$quantidade.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$quantidade.venda))

producaoBENEFICIADA$valor.venda <- 
  as.numeric(
    gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$valor.venda))
 
 producaoBENEFICIADA$quantidade.consumo.utilizacao.na.usina <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$quantidade.consumo.utilizacao.na.usina))

 producaoBENEFICIADA$valor.consumo.utilizacao.na.usina <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$valor.consumo.utilizacao.na.usina))

 producaoBENEFICIADA$quantidade.transferencia.para.transformacao.utilizacao.consumo <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$quantidade.transferencia.para.transformacao.utilizacao.consumo))

 producaoBENEFICIADA$valor.transferencia.para.transformacao.utilizacao.consumo <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = producaoBENEFICIADA$valor.transferencia.para.transformacao.utilizacao.consumo))

 
 
 
 
 #_____Agua_Mineral ----
 agua_Mineral <-
   read.table(
     "https://app.anm.gov.br/dadosabertos/AMB/Agua_Mineral_Producao.csv",
     #"D:/Users/humberto.serna/Documents/CSV_Data/Dados_Abertos_ANM/Agua_Mineral_Producao.csv",
     header = TRUE,
     sep = ",",
     dec = ",",
     stringsAsFactors = FALSE,
     encoding = "iso-8859-1",
     colClasses = c("character")
   )
 
 # colunas 
 colnames(agua_Mineral) <- 
   c("ano", "UF", "Classe.Substância.Mineral", "Substância.Mineral", 
     "Quantidade.litros.Garrafão", "Quantidade.litros.Garrafa.Plástica", 
     "Quantidade.litros.Garrafa.de.Vidro", "Quantidade.litros.Copo", 
     "Quantidade.litros.Outras.Embalagens", "Valor.Garrafão", 
     "Valor.Garrafa.Plástica", "Valor.Garrafa.de.Vidro", 
     "Valor.Copo", "Valor.Outras.Embalagens", "Quantidade.litros.Composição.Produtos.Industrializados", 
     "Valor.Composição.Produtos.Industrializados", "Unidade.de.Medida"
   )
 
 colnames(agua_Mineral) <- 
   tolower(
     FUNA_removeAcentos(colnames(agua_Mineral)))
 
 
 #__________Transformando colunas quantitativas de CHARACTER >> NUMERIC ----
 
 agua_Mineral$ano <- 
   as.integer(agua_Mineral$ano)
 
 agua_Mineral$quantidade.litros.garrafao <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.garrafao))
 
 agua_Mineral$quantidade.litros.garrafa.plastica <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.garrafa.plastica))
 
 agua_Mineral$quantidade.litros.garrafa.de.vidro <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.garrafa.de.vidro))
 
 agua_Mineral$quantidade.litros.copo <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.copo))
 
 agua_Mineral$quantidade.litros.outras.embalagens <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.outras.embalagens))
 
 agua_Mineral$valor.garrafao <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.garrafao))
 
 agua_Mineral$valor.garrafa.plastica <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.garrafa.plastica))
 
 agua_Mineral$valor.garrafa.de.vidro <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.garrafa.de.vidro))
 
 agua_Mineral$valor.copo <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.copo))
 
 agua_Mineral$valor.outras.embalagens <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.outras.embalagens))

 agua_Mineral$quantidade.litros.composicao.produtos.industrializados <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$quantidade.litros.composicao.produtos.industrializados))

 agua_Mineral$valor.composicao.produtos.industrializados <- 
   as.numeric(
     gsub(pattern = ",", replacement = ".", x = agua_Mineral$valor.composicao.produtos.industrializados))
 

# write.table(df, file = "clipboard", sep = "\t", na = "", row.names = FALSE, dec = ",")


 










