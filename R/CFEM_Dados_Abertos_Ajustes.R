#       rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)


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

# Ajuste de Substância AMB ----

# Água Mineral
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "agua mineral|potavel|termal|termais")) {
    cfem_BR$substancia.AMB[i] <- c("agua mineral")
  } 
}

# Areia 
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "areia aluvionar|areia comum|areia fluvial|areia in natura|areia lavada")) {
    cfem_BR$substancia.AMB[i] <- c("areia")
  }
}

# Areia INDUSTRIAL
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "areia de fundicao|areia p/ vidro|areia quartzosa")) {
    cfem_BR$substancia.AMB[i] <- c("areia industrial")
  }
}


# BRITA 
for (i in 1:5000) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "^brita$|britada|cascalho|pedregulho|^granito$|basalto")) {
    cfem_BR$substancia.AMB[i] <- c("rochas britadas e cascalho")
  }
}

# ORNAMENTAL 
for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "ornamental|revestimento")) {
    cfem_BR$substancia.AMB[i] <- c("rochas ornamentais")
  }
}


# ARGILAS

for (i in 1:nrow(cfem_BR)) {
  if (grepl(cfem_BR$substancia.AMB[i], pattern = "argila branca")) {
    cfem_BR$substancia.AMB[i] <- c("argilas plásticas")
  } else {
    if (grepl(cfem_BR$substancia.AMB[i], 
              pattern = "argila comum|argila ferruginosa|argila p/cer. vermelh|argila vermelha|argila p/cer. vermelha")) {
      cfem_BR$substancia.AMB[i] <- c("argilas comuns")
    }
    
    
  }
}



# 
# # APENDICE ---
# 
# # ____ carregando alíquotas
# aliquota <-
#   read.table(
#     file = paste(Sys.getenv("R_USER"), '/D_Lake/cfem_aliquotas.csv', sep = ""),
#     header = TRUE,sep = ";",stringsAsFactors = FALSE,dec = ',')
# 
# # ____ matriz alíquotas
# matriz_aliquotas <- 
#   read.table(
#     file = paste(Sys.getenv("R_USER"), '/D_Lake/cfem_matriz_alíquotas.csv', sep = ""),
#     header = TRUE,sep = ";",stringsAsFactors = FALSE)
# 
# # ____ carregando IGP-DI
# IGP_DI <-
#   read.table(
#     file = paste(Sys.getenv("R_USER"), '/D_Lake/IGP_DI.csv', sep = ""),
#     header = TRUE,sep = ";",stringsAsFactors = FALSE, dec = ',')
# 
# 
# 
# # AREIA ----
# # _____ ajuste de unidades de massa e volume (tentativa de) ----
# for (i in 1:nrow(cfem_BR)) {
#   if (grepl(cfem_BR$substancia.AMB[i], pattern = "^areia$")) {
#     
#     if (cfem_BR$unidade.de.Medida[i] == "m3") {
#       
#       cfem_BR$quantidade.Comercializada[i] <-
#         1.64 * cfem_BR$quantidade.Comercializada[i]
#       
#       cfem_BR$unidade.de.Medida[i] <- "m3-> t"
#       
#     } else if (cfem_BR$unidade.de.Medida[i] == "kg") {
#       cfem_BR$quantidade.Comercializada[i] <-
#         1000 * cfem_BR$quantidade.Comercializada[i]
#       
#       cfem_BR$unidade.de.Medida[i] <- "kg-> t"
#     }
#   }
# }
# 
# 
# # Exportar ---- 
# 
# save(cfem_BR, file = "./D_Lake/cfem_BR.RData")
