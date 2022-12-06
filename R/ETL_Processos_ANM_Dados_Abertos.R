# rm(list = ls())

  arquivos <-
    c(
        'Cessoes_de_Direitos.csv',
        'Guia_de_Utilizacao_Autorizada.csv',
        'Licenciamento.csv',
        'PLG.csv',
        'Portaria_de_Lavra.csv',
        'Registro_de_Extracao_Publicado.csv',
        'Requerimento_de_Lavra.csv',
        'Requerimento_de_Licenciamento.csv',
        'Requerimento_de_Pesquisa.csv',
        'Requerimento_de_PLG.csv',
        'Requerimento_de_Registro_de_Extracao_Protocolizado.csv'
      )
  
  processos_ANM <- as.list(NA)
  for (i in 1:length(arquivos)) {
    processos_ANM[[i]] <-
      read.table(
        file = paste(
          sep = "",
          "https://app.dnpm.gov.br/DadosAbertos/SCM/",
          arquivos[[i]]
        ),
        header = TRUE,
        sep = ",",
        fill = TRUE,
        stringsAsFactors = FALSE,
        encoding = "ANSI", 
        quote = "\""
      )
  }
  processos_ANM <-
    do.call("rbind", processos_ANM)
  
  
  arquivos <-
    c(
        'Licenciamento.csv',
        'PLG.csv',
        'Portaria_de_Lavra.csv',
        'Registro_de_Extracao_Publicado.csv',
        'Requerimento_de_Lavra.csv',
        'Requerimento_de_Licenciamento.csv',
        'Requerimento_de_Pesquisa.csv',
        'Requerimento_de_PLG.csv',
        'Requerimento_de_Registro_de_Extracao_Protocolizado.csv'
      )
  
  processos_ANM <- as.list(NA)
  for (i in 1:length(arquivos)) {
    
    processos_ANM[[i]] <-
      read.table(
        file = paste0("./data/", arquivos[[i]]),
        header = TRUE,
        sep = ",",
        fill = TRUE,
        stringsAsFactors = FALSE,
        fileEncoding = "Latin1", 
        quote = "\""
      )
  }
  
  processos_ANM <-
    do.call("rbind", processos_ANM)

  
# saveRDS(processos_ANM, 'D:/Users/humberto.serna/Documents/D_Lake/Processos_ANM_Dados_Abertos.RDATA')

