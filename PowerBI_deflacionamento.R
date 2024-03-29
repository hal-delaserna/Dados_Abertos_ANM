rm(list = ls())


IGP_IPAI <- httr::GET(
  sprintf("http://ipeadata.gov.br/api/odata4/ValoresSerie(SERCODIGO='%s')", "IGP_IPAI")
)

IGP_IPAI <- 
  dplyr::mutate(dplyr::select(dplyr::bind_rows(httr::content(IGP_IPAI)[[2]]), 
                                "ano" = `VALDATA`, "value" = `VALVALOR`),
                  ano = lubridate::year(ano))


# IPA-DI - origem - prod. industriais - �ndice (ago. 1994 = 100)



Producao_Bruta <-
  read.table(
    file = 'https://app.anm.gov.br/DadosAbertos/AMB/Producao_Bruta.csv',
    sep = ",",
    header = TRUE,
    dec = ","
  )

Producao_Beneficiada <-
  read.table(
    file = 'https://app.anm.gov.br/DadosAbertos/AMB/Producao_Beneficiada.csv',
    sep = ",",
    header = TRUE,
    dec = ","
  )

Agua_Mineral_Producao <-
  read.table(
    file = 'https://app.anm.gov.br/dadosabertos/AMB/Agua_Mineral_Producao.csv',
    sep = ",",
    header = TRUE, 
    dec = ","
  )


Producao_Bruta$DFL_Valor.Venda..R.. <- NA
Producao_Bruta$DFL_Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. <- NA
Producao_Bruta$DFL_Valor.Transforma��o...Consumo...Utiliza��o.nesta.mina..R.. <- NA
Producao_Beneficiada$DFL_Valor.Venda..R.. <- NA
Producao_Beneficiada$DFL_Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. <- NA
Producao_Beneficiada$DFL_Valor.Consumo...Utiliza��o.na.Usina..R.. <- NA
Agua_Mineral_Producao$DFL_Valor..R...Garraf�o <- NA
Agua_Mineral_Producao$DFL_Valor..R...Garrafa.Pl�stica <- NA
Agua_Mineral_Producao$DFL_Valor..R...Garrafa.de.Vidro <- NA
Agua_Mineral_Producao$DFL_Valor..R...Copo <- NA
Agua_Mineral_Producao$DFL_Valor..R...Outras.Embalagens <- NA
Agua_Mineral_Producao$DFL_Valor..R...Composi��o.Produtos.Industrializados <- NA

for (y in unique(Producao_Bruta$Ano.base)) {
  
    Producao_Bruta[Producao_Bruta$Ano.base == y, ]$DFL_Valor.Venda..R.. <-
      Producao_Bruta[Producao_Bruta$Ano.base == y, ]$Valor.Venda..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Bruta$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Producao_Bruta[Producao_Bruta$Ano.base == y, ]$DFL_Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. <-
      Producao_Bruta[Producao_Bruta$Ano.base == y, ]$Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Bruta$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Producao_Bruta[Producao_Bruta$Ano.base == y, ]$DFL_Valor.Transforma��o...Consumo...Utiliza��o.nesta.mina..R.. <-
      Producao_Bruta[Producao_Bruta$Ano.base == y, ]$Valor.Transforma��o...Consumo...Utiliza��o.nesta.mina..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Bruta$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$DFL_Valor.Venda..R.. <-
      Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$Valor.Venda..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Beneficiada$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$DFL_Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. <-
      Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$Valor.Transfer�ncia.para.Transforma��o...Utiliza��o...Consumo..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Beneficiada$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$DFL_Valor.Consumo...Utiliza��o.na.Usina..R.. <-
      Producao_Beneficiada[Producao_Beneficiada$Ano.base == y, ]$Valor.Consumo...Utiliza��o.na.Usina..R.. *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Producao_Beneficiada$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    

    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Garraf�o <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Garraf�o *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Garrafa.Pl�stica <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Garrafa.Pl�stica *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Garrafa.de.Vidro <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Garrafa.de.Vidro *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value    
    
    
    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Copo <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Copo *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Outras.Embalagens <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Outras.Embalagens *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value
    
    
    Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$DFL_Valor..R...Composi��o.Produtos.Industrializados <-
      Agua_Mineral_Producao[Agua_Mineral_Producao$Ano.base == y, ]$Valor..R...Composi��o.Produtos.Industrializados *
      IGP_IPAI[IGP_IPAI$ano == tail(unique(Agua_Mineral_Producao$Ano.base), 1), ]$value /
      IGP_IPAI[IGP_IPAI$ano == y, ]$value    
    

    }









