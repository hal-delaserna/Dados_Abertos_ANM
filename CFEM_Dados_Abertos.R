#       rm(list = ls())
# options(editor = 'notepad')
library(tidyverse)

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

# ____ impondo trimestre
cfem_BR$trimestre <-
  lubridate::quarter(
    ymd(
    paste(cfem_BR$periodo, cfem_BR$mes.de.referencia, "1", sep = "_")
  ), with_year = TRUE)


# ____ impondo semestre
cfem_BR$semestre <-
  lubridate::semester(
    ymd(
      paste(cfem_BR$periodo, cfem_BR$mes.de.referencia, "1", sep = "_")
    ), with_year = TRUE)


# ____ impondo mês.ANO
cfem_BR$mes.de.referencia <-
  paste(month(cfem_BR$mes.de.referencia, label = TRUE), cfem_BR$periodo, sep = ".")
    


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


# Formatação e ajustes ----
cfem_BR$municipio <- 
  cfem_BR$municipio %>% str_squish() %>% FUNA_removeAcentos() %>% FUNA_minusculas()

cfem_BR$uf <- 
  str_squish(cfem_BR$uf)

cfem_BR$unidade.de.Medida <- 
  str_squish(cfem_BR$unidade.de.Medida)

cfem_BR$substancia.SCM <- 
  cfem_BR$substancia.SCM %>% str_squish() %>% FUNA_removeAcentos() %>% gsub(pattern = ";", replacement = "") %>% FUNA_minusculas()

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



# id Municipio_UF ----
cfem_BR$municipio_UF <- 
  paste(cfem_BR$municipio, cfem_BR$uf, sep = "_")
# ________________________________________________________----



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

# Valor da operação inferido -----

for (i in 1:nrow(areia_miracatu_regiao)) {
  if (areia_miracatu_regiao$periodo[i] < 2018) {
    areia_miracatu_regiao$valorDaOperacao[i] <-
      areia_miracatu_regiao$valor.Recolhido.CFEM[i] / 0.02
  } else {
    areia_miracatu_regiao$valorDaOperacao[i] <-
      areia_miracatu_regiao$valor.Recolhido.CFEM[i] / 0.01
  }
}

# _____ vetor de "preços" ---- 
            #(cuidado com as unidades de massa/volume!)

cfem_BR$preco <-
  round(cfem_BR$ # valorDaOperacao / cfem_BR$quantidade.Comercializada,
          digits = 2)


# ________________________________________________________----



# Maiores Empresas em cada Substância - ANO ----

df <-
  group_by(cfem_BR[cfem_BR$periodo %in% c(2018, 2019, 2020),],
           cpfcnpj, substancia.SCM, periodo) %>% summarise(
             "Q_Comercializada" = sum(quantidade.Comercializada),
             "CFEM" = sum(valor.Recolhido.CFEM))



# extração do top 6 em cada substância ---- 
lista <- list()
i <- 1
for (ano in 2018:2020) {
  for (substancia in sort(unique(cfem_BR$substancia.SCM))) {
    lista[[i]] <-
      head(arrange(df[df$periodo == ano &
                           df$substancia.SCM == substancia,], desc(CFEM)))
    i <- i + 1
  }
}

df_1d <- 
  do.call(what = "rbind", args = lista)


# _____ df_wide_maiores_empresas_ano ----

df_wide_maiores_empresas_ano <-
  arrange(spread(
    left_join(
      df_1d[, -4], # -Q_Comercializada
      #cpfcnpj,
      unique(cfem_BR[, c("cpfcnpj", "titular")]), 
      by = "cpfcnpj"),  
    key = periodo,
    value = CFEM
  ), desc(`2020`))[, c(1, 3, 2, 6:4)]



# CPFCNPJs_alvo ----

cpfcnpj_alvo <- 
  read.table(
  './CSV_Data/empresas_da_amostra_IPM_Informe_Mineral.csv',
  header = TRUE,
  sep = ";",
  quote = "\"",
  fill = FALSE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

colnames(cpfcnpj_alvo) <- c("substancia", "cpfcnpj")
cpfcnpj_alvo$substancia <- 
  FUNA_removeAcentos(cpfcnpj_alvo$substancia)

# id_cnpj_substancia
cpfcnpj_alvo$id_cnpj_substancia <- 
  paste(cpfcnpj_alvo$cpfcnpj, cpfcnpj_alvo$substancia, sep = "_")
cfem_BR$id_cnpj_substancia <- 
  paste(cfem_BR$cpfcnpj, cfem_BR$substancia, sep = "_")



# tabela wide
spread(
  group_by(cfem_BR[cfem_BR$periodo %in% c(2018, 2019, 2020) &
                     cfem_BR$cpfcnpj %in% cpfcnpj_alvo$cpfcnpj, c("id_cnpj_substancia",
                                                                  "periodo",
                                                                  "valor.Recolhido.CFEM")],
           id_cnpj_substancia, periodo) %>% summarise("cfem" = sum(valor.Recolhido.CFEM)),
  key = periodo,
  value = cfem) %>% arrange(desc(`2019`)) %>% View()



# tabela wide
spread(
  group_by(cfem_BR[cfem_BR$periodo %in% c(2018, 2019, 2020) &
                     cfem_BR$cpfcnpj %in% cpfcnpj_alvo$cpfcnpj, c("id_cnpj_substancia",
                                                                  "periodo",
                                                                  "valor.Recolhido.CFEM")],
            periodo) %>% summarise("cfem" = sum(valor.Recolhido.CFEM)),
  key = periodo,
  value = cfem) %>% arrange(desc(`2019`))



spread(
group_by(
  cfem_BR[cfem_BR$periodo %in% c(2019,2020) & grepl(pattern = "vale$|vale ", x = cfem_BR$titular),],
         titular, periodo) %>% summarise('quantidade' = sum(quantidade.Comercializada)),
key = periodo, value = quantidade) %>% View()

ipm <- 
read.table(file = "clipboard", header = TRUE, sep = "\t", dec = ",")

colnames(ipm) <- c("Empresa", "Substância", "Produto", "quantidade.Vendida.Tranferida", 
                   "vendas")
ipm$vu <- ipm$vendas/ipm$quantidade.Vendida.Tranferida


summarise(group_by(ipm, Substância),
          mean(vu, na.rm = TRUE),
          median(vu, na.rm = TRUE))




summarise(group_by(ipm, Empresa, Substância),
          mean(vu, na.rm = TRUE),
          median(vu, na.rm = TRUE)) %>% filter(Substância == 'Níquel')



ipm[ipm$Substância == "Níquel",]  


vpm <- 
  read.table(file = "clipboard", header = TRUE, sep = "\t", dec = ",",)

vpm$id <- 
  paste(vpm$Substância, vpm$Produto, sep = "_")


spread(vpm, key = periodo, value = VPM) %>% arrange(desc(`1S-2020`))



# ************* GRAVAR ************* ----
write.table(
  x = b ,
  file = 'cnpjs_alvo.csv',
  sep = ";",
  dec = ",",
  row.names = FALSE,
  na = "-"
)




