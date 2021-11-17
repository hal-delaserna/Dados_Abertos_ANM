rm(list = ls())

if (!require(svDialogs)) {
  install.packages('svDialogs')
  library(svDialogs)
}


# Simpler version with msgBox and okCancelBox


res <- 
  dlg_message(c("Baixar arquivos de DadosAbertos.gov? Se possuir os arquivos clique 'no'"),
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
    dlgDir(default = getwd(), title = 'Informe a pasta dos arquivos baixados.')$res
  
  arquivos <-
    dlgList(
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
      title = 'Selecionae vários usando CTRL'
    )$res
  
  processos_ANM <- as.list(NA)
  for (i in 1:length(arquivos)) {
    
    processos_ANM[[i]] <-
      read.table(
        file = paste(sep = "/", prevdir, arquivos[[i]]),
        header = TRUE,
        sep = ";",
        fill = TRUE, quote = "",
        stringsAsFactors = FALSE, encoding = "ANSI"
      )
  }
  
  processos_ANM <-
    do.call("rbind", processos_ANM)
}} 

