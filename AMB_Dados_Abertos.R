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
    #"D:/Users/humberto.serna/Documents/D_Lake/Dados_Abertos_ANM/Producao_Bruta.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "ANSI")

# colunas 
# "Ano base","UF","Classe Substância","Substância Mineral","Quantidade Produção - Minério ROM (t)","Quantidade Contido","Unidade de Medida - Contido","Indicação Contido","Quantidade Venda (t)","Valor Venda (R$)","Quantidade Transformação / Consumo / Utilização (t)","Valor Transformação / Consumo / Utilização nesta mina (R$)","Quantidade Transferência para Transformação / Utilização / Consumo (t)","Valor Transferência para Transformação / Utilização / Consumo (R$)"

colnames(producaoBRUTA) <- 
  c("Ano.base","UF","Classe.Substância","Substância.Mineral","Quantidade.Produção.Minério.ROM",
    "Quantidade.Contido","Unidade.de.Medida.Contido","Indicação.Contido","Quantidade.Venda",
    "Valor.Venda","Quantidade.Transformação.Consumo.Utilização","Valor.Transformação.Consumo.Utilização.nesta.mina",
    "Quantidade.Transferência.para.Transformação.Utilização.Consumo",
    "Valor.Transferência.para.Transformação.Utilização.Consumo")

#_____ BENEFICIADA ----
# producao_beneficiada <-
producaoBENEFICIADA <-
  read.table(
    "https://app.anm.gov.br/DadosAbertos/AMB/Producao_Beneficiada.csv",
    #"D:/Users/humberto.serna/Documents/D_Lake/Dados_Abertos_ANM/Producao_Bruta.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "ANSI")

# colunas 
# "Ano base","UF","Classe Substância","Substância Mineral","Quantidade Produção","Unidade de Medida - Produção","Quantidade Contido","Unidade de Medida - Contido","Indicação Contido","Quantidade Venda","Unidade de Medida - Venda","Valor Venda (R$)","Quantidade Consumo/Utilização na Usina","Unidade de Medida - Consumo/Utilização na Usina","Valor Consumo / Utilização na Usina (R$)","Quantidade Transferência para Transformação / Utilização / Consumo","Unidade de Medida - Transferência para Transformação / Utilização / Consumo","Valor Transferência para Transformação / Utilização / Consumo (R$)"

colnames(producaoBENEFICIADA) <- 
  c("Ano.base","UF","Classe.Substância","Substância.Mineral","Quantidade.Produção","Unidade.de.Medida.Produção",
    "Quantidade.Contido","Unidade.de.Medida.Contido","Indicação.Contido","Quantidade.Venda","Unidade.de.Medida.Venda",
    "Valor.Venda","Quantidade.Consumo.Utilização.na.Usina","Unidade.de.Medida.Consumo.Utilização.na.Usina",
    "Valor.Consumo.Utilização.na.Usina","Quantidade.Transferência.para.Transformação.Utilização.Consumo",
    "Unidade.de.Medida.Transferência.para.Transformação.Utilização.Consumo",
    "Valor.Transferência.para.Transformação.Utilização.Consumo")

 
 #_____Agua_Mineral ----
agua_Mineral <-
  read.table(
    "https://app.anm.gov.br/DadosAbertos/AMB/Agua_Mineral_Producao.csv",
    #"D:/Users/humberto.serna/Documents/D_Lake/Dados_Abertos_ANM/Producao_Bruta.csv",
    header = TRUE,
    sep = ",",
    dec = ",",
    stringsAsFactors = FALSE,
    encoding = "ANSI")
 
 
# colunas 
# "Ano base","UF","Classe Substância Mineral","Substância Mineral","Quantidade (litros) Garrafão","Quantidade (litros) Garrafa Plástica","Quantidade (litros) Garrafa de Vidro","Quantidade (litros) Copo","Quantidade (litros) Outras Embalagens","Valor (R$) Garrafão","Valor (R$) Garrafa Plástica","Valor (R$) Garrafa de Vidro","Valor (R$) Copo","Valor (R$) Outras Embalagens","Quantidade (litros) Composição Produtos Industrializados","Valor (R$) Composição Produtos Industrializados","Unidade de Medida"

 colnames(agua_Mineral) <- 
   c("Ano.base","UF","Classe.Substância.Mineral","Substância.Mineral","Quantidade.Garrafão",
     "Quantidade.Garrafa.Plástica","Quantidade.Garrafa.de.Vidro","Quantidade.Copo",
     "Quantidade.Outras.Embalagens","Valor.Garrafão","Valor.Garrafa.Plástica","Valor.Garrafa.de.Vidro","Valor.Copo",
     "Valor.Outras.Embalagens","Quantidade.Composição.Produtos.Industrializados","Valor.Composição.Produtos.Industrializados",
     "Unidade.de.Medida")
 
 
# write.table(df, file = "clipboard", sep = "\t", na = "", row.names = FALSE, dec = ",")


