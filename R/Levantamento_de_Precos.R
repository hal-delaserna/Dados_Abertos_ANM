# rm(list = ls())
library(tidyverse)

source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_Consumidores.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/graficos_AMB.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_Producao.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_Reserva.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_AMB/Funcoes_VPM.R")
source(file = "D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/carregamento_Bases_AMB_outras.r") 


# CARREGAMENTO -----
#_____ carregamento GeoCod -----
GeoCodigos_IBGE <-
  read.table(
    file = paste(Sys.getenv('R_USER'), "/D_Lake/GeoCodigos_IBGE.csv", sep = ""),
    header = TRUE,
    sep = ";",
    quote = "",
    stringsAsFactors = FALSE,
    encoding = 'ANSI',
    fill = TRUE)


#_____ Carregamento CFEM_DADOS ABERTOS ----

cfem_BR <- #fonte: Dados Abertos
  read.table(file = paste(Sys.getenv("R_USER"),'/D_Lake/cfem.csv', sep = ""),
             header = TRUE,sep = ",",fill = TRUE,stringsAsFactors = FALSE, dec = ',',
             colClasses = c(Processo = "character", CPF.CNPJ = "character"))

colnames(cfem_BR) <- 
  c("periodo", "processo", "ano.do.Processo", "cpfcnpj", "titular", 
    "fase.do.Processo", "substancia.SCM", "uf", "municipio", "unidade.de.Medida", 
    "quantidade.Comercializada", "valor.Recolhido.CFEM")

#__________ Formatação e ajustes ----
cfem_BR$municipio <- 
  cfem_BR$municipio %>% str_squish() %>% FUNA_removeAcentos() %>% FUNA_minusculas()

cfem_BR$uf <- 
  str_squish(cfem_BR$uf)

cfem_BR$unidade.de.Medida <- 
  str_squish(cfem_BR$unidade.de.Medida)

cfem_BR$substancia.SCM <- 
  cfem_BR$substancia.SCM %>% str_squish() %>% FUNA_removeAcentos() %>% gsub(pattern = ";", replacement = "") %>% FUNA_minusculas()

#__________ id Municipio_UF 
cfem_BR$municipio_UF <- 
  paste(cfem_BR$municipio, cfem_BR$uf, sep = "_")

#__________ valor da operação inferido -----
# CUIDADO! APENAS PARA AGREGADOS DE CC. SERÁ APERFEIÇOADO P/ TODAS SUBSTANCIAS
for (i in 1:nrow(cfem_BR)) {
  if (cfem_BR$periodo[i] < 2018) {
    cfem_BR$valorDaOperacao[i] <-
      cfem_BR$valor.Recolhido.CFEM[i] / 0.02
  } else {
    cfem_BR$valorDaOperacao[i] <-
      cfem_BR$valor.Recolhido.CFEM[i] / 0.01
  }
}


#__________ ajuste de unidades de massa e volume (tentativa de)----
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


#__________ vetor de "preços" ---- 
#(cuidado com as unidades de massa/volume!)

cfem_BR$preco <-
  round(cfem_BR$valorDaOperacao / cfem_BR$quantidade.Comercializada,
        digits = 2)


# ________________________________________________________----

# AMOSTRA -----
# 
meso <- c("barra do turvo","cajati","cananeia","eldorado","iguape","ilha comprida","itanhaem","itariri","jacupiranga","juquia","miracatu","mongagua","pariquera acu","pedro de toledo","peruibe","registro","sete barras")
ra <- c("barra do turvo","cajati","cananeia","eldorado","iguape","ilha comprida","itariri","jacupiranga","juquia","miracatu","pariquera acu","pedro de toledo","registro","sete barras")
micro <- c("barra do turvo","cajati","cananeia","eldorado","iguape","ilha comprida","jacupiranga","juquia","miracatu","pariquera acu","registro","sete barras")
contiguidade_2 <- c("aluminio", "cananeia", "cotia", "embu guacu", "ibiuna", "iguape", "ilha comprida", "itanhaem", "itariri", "juquia", "juquitiba", "mairinque", "miracatu", "pariquera acu", "pedro de toledo", "peruibe", "piedade", "pilar do sul", "registro", "sao lourenco da serra", "sao miguel arcanjo", "sao roque", "sete barras", "tapirai", "votorantim")



# Amostra RAL-AMB
# _____ Visão spread Prod BRUTA ----
bruta <-
  select(producaoBRUTA[producaoBRUTA$substancia.amb == "areia" &
                         producaoBRUTA$municipio %in% contiguidade_2 &
                         producaoBRUTA$ano %in% c(2016, 2017, 2018, 2019),], everything()) %>% 
  group_by(ano, municipio) %>% summarise(median(preco, na.rm = TRUE)) %>%
  spread(key = ano, value = `median(preco, na.rm = TRUE)`)

# _____ Visão spread Prod BENEFICIADA ---- 
beneficiada <-
  select(producaoBENEFICIADA[producaoBENEFICIADA$substancia.amb == "areia" &
                               producaoBENEFICIADA$municipio %in% contiguidade_2 &
                               producaoBENEFICIADA$ano %in% c(2016, 2017, 2018, 2019),], everything()) %>% 
  group_by(ano, municipio) %>% summarise(median(preco, na.rm = TRUE)) %>%
  spread(key = ano, value = `median(preco, na.rm = TRUE)`)


# Amostra cfem_BR ----

areia_miracatu_regiao <-
  select(cfem_BR[grepl(cfem_BR$substancia, pattern = "areia") &
                   cfem_BR$uf == "SP" &
                   cfem_BR$municipio %in% contiguidade_2 &
                   cfem_BR$periodo %in% c(2016,2017, 2018, 2019),], everything())


# _____ Visão spread cfem ----
cfem_miracatu <-
  areia_miracatu_regiao %>% group_by(periodo) %>% summarise(median(preco, na.rm = TRUE)) %>%
  spread(key = periodo, value = `median(preco, na.rm = TRUE)`)


# consumidores
MESO <- "barra do turvo|cajati|cananeia|eldorado|iguape|ilha comprida|itanhaem|itariri|jacupiranga|juquia|miracatu|mongagua|pariquera acu|pedro de toledo|peruibe|registro|sete barras"
RA <- "barra do turvo|cajati|cananeia|eldorado|iguape|ilha comprida|itariri|jacupiranga|juquia|miracatu|pariquera acu|pedro de toledo|registro|sete barras" 
MICRO <- "barra do turvo|cajati|cananeia|eldorado|iguape|ilha comprida|jacupiranga|juquia|miracatu|pariquera acu|registro|sete barras"
CONTIGUIDADE_2 <- c("aluminio|cananeia|cotia|embu guacu|ibiuna|iguape|ilha comprida|itanhaem|itariri|juquia|juquitiba|mairinque|miracatu|pariquera acu|pedro de toledo|peruibe|piedade|pilar do sul|registro|sao lourenco da serra|sao miguel arcanjo|sao roque|sete barras|tapirai|votorantim")
consumidoresUSINA_preco_MEDIO(produto = "areia",municipio = MICRO)
consumidoresUSINA_preco_MEDIO(produto = "areia",municipio = RA)
CONSUMIDORES_mesorregiãoRegistro <- consumidoresUSINA_preco_MEDIO(produto = "areia",municipio = MESO)
consumidoresUSINA_preco_MEDIO(produto = "areia",municipio = CONTIGUIDADE_2)
consumidoresMINA_Preco_MEDIO(minerio = "areia",municipio = MICRO)
consumidoresMINA_Preco_MEDIO(minerio = "areia",municipio = RA)
consumidoresMINA_Preco_MEDIO(minerio = "areia",municipio = MESO)




# ************* GRAVAR NA ÁREA DE TRANSFERÊNCIA ************* ----
write.table(x = cfem_miracatu , file = 'clipboard', sep = ";", dec = ",", row.names = FALSE, na = "-")
