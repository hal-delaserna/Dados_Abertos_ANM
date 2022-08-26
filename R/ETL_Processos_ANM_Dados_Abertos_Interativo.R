# rm(list = ls())

if (!require(svDialogs)) {
  install.packages('svDialogs')
  library(svDialogs)
}



res <- 
  dlg_message(c("Baixar arquivos de DadosAbertos.gov? Se os baixou clique 'no'"),
            "yesno")$res

if (res == "yes") {
  dlgMessage(
    c(
      'Selecione os arquivos para download.',
      'Use CTRL para mais de um'
    )
  )
  arquivos <-
    dlgList(
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
      ),
      multiple = TRUE,
      title = 'Selecione os arquivos'
    )$res
  
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
  
} else {
  
  
  res <- 
    dlg_message(c("Usar arquivos previamente baixados?"),
                "yesno")$res
  
  if (res == "yes") {
    
  prevdir <-
    dlgDir(default = getwd(), title = 'INFORME A PASTA DOS ARQUIVOS BAIXADOS.')$res
  
  arquivos <-
    dlgList(preselect = c('Licenciamento.csv','PLG.csv','Portaria_de_Lavra.csv','Registro_de_Extracao_Publicado.csv','Requerimento_de_Lavra.csv','Requerimento_de_Licenciamento.csv','Requerimento_de_Pesquisa.csv','Requerimento_de_PLG.csv','Requerimento_de_Registro_de_Extracao_Protocolizado.csv'),
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
      ),
      multiple = TRUE,
      title = 'CTRL + mouse para selecionar diversos'
    )$res
  
  processos_ANM <- as.list(NA)
  for (i in 1:length(arquivos)) {
    
    processos_ANM[[i]] <-
      read.table(
        file = paste(sep = "/", prevdir, arquivos[[i]]),
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
}} 


# saveRDS(processos_ANM, 'D:/Users/humberto.serna/Documents/D_Lake/Processos_ANM_Dados_Abertos.RDATA')

