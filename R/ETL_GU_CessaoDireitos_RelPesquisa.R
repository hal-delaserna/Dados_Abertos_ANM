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
        "Cessoes_de_Direitos.csv",
        "Guia_de_Utilizacao_Autorizada.csv",
        "Relatorio_de_Pesquisa_Aprovado.csv"
      ),
      multiple = FALSE,
      title = 'Selecione os arquivos'
    )$res
# ciclo de download  
  url <- "https://app.anm.gov.br/DadosAbertos/SCM/"
    for (i in arquivos) {
      download.file(url = paste0(url, i), 
                    destfile = paste0("./data/",i))
    }
    
# problema de "aspas" dentro da string
    file <- "EOF_gravando_colonQuotes.sh"
    sub_dir <- "data"
    shell.exec(file.path(sub_dir, file, fsep = "\\"))
    
    # requer interroper para esperar execução de shell script
    Sys.sleep(10)

# ciclo de carregamento    
    processos_ANM <- as.list(NA)
    for (i in 1:length(arquivos)) {
      
      processos_ANM[[i]] <-
        read.table(
          file = paste0("./data/", arquivos[i]),
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
  
} else {
  
  
  res <- 
    dlg_message(c("Usar arquivos previamente baixados?"),
                "yesno")$res
  
  if (res == "yes") {
    
  
  arquivos <-
    dlgList(preselect = c('Relatorio_de_Pesquisa_Aprovado.csv'),
      c(
        "Cessoes_de_Direitos.csv",
        "Guia_de_Utilizacao_Autorizada.csv",
        "Relatorio_de_Pesquisa_Aprovado.csv"
      ),
      multiple = FALSE,
      title = 'CTRL + mouse para selecionar diversos'
    )$res
  
  
  processos_ANM <- as.list(NA)
  for (i in 1:length(arquivos)) {
    
    processos_ANM[[i]] <-
      read.table(
        file = paste0("./data/", arquivos[i]),
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
}} 


 saveRDS(processos_ANM, paste0('./data/ETL_Processos_ANM_Dados_Abertos_', format(Sys.time(), format = "%Y%m%d%H%M"),'.Rds'))

