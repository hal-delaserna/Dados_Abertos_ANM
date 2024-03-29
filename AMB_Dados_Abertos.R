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
# "Ano base","UF","Classe Subst�ncia","Subst�ncia Mineral","Quantidade Produ��o - Min�rio ROM (t)","Quantidade Contido","Unidade de Medida - Contido","Indica��o Contido","Quantidade Venda (t)","Valor Venda (R$)","Quantidade Transforma��o / Consumo / Utiliza��o (t)","Valor Transforma��o / Consumo / Utiliza��o nesta mina (R$)","Quantidade Transfer�ncia para Transforma��o / Utiliza��o / Consumo (t)","Valor Transfer�ncia para Transforma��o / Utiliza��o / Consumo (R$)"

colnames(producaoBRUTA) <- 
  c("Ano.base","UF","Classe.Subst�ncia","Subst�ncia.Mineral","Quantidade.Produ��o.Min�rio.ROM",
    "Quantidade.Contido","Unidade.de.Medida.Contido","Indica��o.Contido","Quantidade.Venda",
    "Valor.Venda","Quantidade.Transforma��o.Consumo.Utiliza��o","Valor.Transforma��o.Consumo.Utiliza��o.nesta.mina",
    "Quantidade.Transfer�ncia.para.Transforma��o.Utiliza��o.Consumo",
    "Valor.Transfer�ncia.para.Transforma��o.Utiliza��o.Consumo")

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
# "Ano base","UF","Classe Subst�ncia","Subst�ncia Mineral","Quantidade Produ��o","Unidade de Medida - Produ��o","Quantidade Contido","Unidade de Medida - Contido","Indica��o Contido","Quantidade Venda","Unidade de Medida - Venda","Valor Venda (R$)","Quantidade Consumo/Utiliza��o na Usina","Unidade de Medida - Consumo/Utiliza��o na Usina","Valor Consumo / Utiliza��o na Usina (R$)","Quantidade Transfer�ncia para Transforma��o / Utiliza��o / Consumo","Unidade de Medida - Transfer�ncia para Transforma��o / Utiliza��o / Consumo","Valor Transfer�ncia para Transforma��o / Utiliza��o / Consumo (R$)"

colnames(producaoBENEFICIADA) <- 
  c("Ano.base","UF","Classe.Subst�ncia","Subst�ncia.Mineral","Quantidade.Produ��o","Unidade.de.Medida.Produ��o",
    "Quantidade.Contido","Unidade.de.Medida.Contido","Indica��o.Contido","Quantidade.Venda","Unidade.de.Medida.Venda",
    "Valor.Venda","Quantidade.Consumo.Utiliza��o.na.Usina","Unidade.de.Medida.Consumo.Utiliza��o.na.Usina",
    "Valor.Consumo.Utiliza��o.na.Usina","Quantidade.Transfer�ncia.para.Transforma��o.Utiliza��o.Consumo",
    "Unidade.de.Medida.Transfer�ncia.para.Transforma��o.Utiliza��o.Consumo",
    "Valor.Transfer�ncia.para.Transforma��o.Utiliza��o.Consumo")

 
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
# "Ano base","UF","Classe Subst�ncia Mineral","Subst�ncia Mineral","Quantidade (litros) Garraf�o","Quantidade (litros) Garrafa Pl�stica","Quantidade (litros) Garrafa de Vidro","Quantidade (litros) Copo","Quantidade (litros) Outras Embalagens","Valor (R$) Garraf�o","Valor (R$) Garrafa Pl�stica","Valor (R$) Garrafa de Vidro","Valor (R$) Copo","Valor (R$) Outras Embalagens","Quantidade (litros) Composi��o Produtos Industrializados","Valor (R$) Composi��o Produtos Industrializados","Unidade de Medida"

 colnames(agua_Mineral) <- 
   c("Ano.base","UF","Classe.Subst�ncia.Mineral","Subst�ncia.Mineral","Quantidade.Garraf�o",
     "Quantidade.Garrafa.Pl�stica","Quantidade.Garrafa.de.Vidro","Quantidade.Copo",
     "Quantidade.Outras.Embalagens","Valor.Garraf�o","Valor.Garrafa.Pl�stica","Valor.Garrafa.de.Vidro","Valor.Copo",
     "Valor.Outras.Embalagens","Quantidade.Composi��o.Produtos.Industrializados","Valor.Composi��o.Produtos.Industrializados",
     "Unidade.de.Medida")
 
 
# write.table(df, file = "clipboard", sep = "\t", na = "", row.names = FALSE, dec = ",")


