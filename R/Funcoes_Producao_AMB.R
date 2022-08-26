# Funções PRODUÇÃO BRUTA ----



#_____BRUTA_Quantidade_groupBY_SUBSTANCIA ----
BRUTA_Quantidade_groupBY_SUBSTANCIA <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, Substância.Mineral) %>%
            summarise(soma = sum(Quantidade.Produção.Minério.ROM)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "contido") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, Substância.Mineral) %>% 
              summarise(soma = sum(Quantidade.Contido)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "Quantidade.Venda") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                     grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                     grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
                group_by(Ano.base, Substância.Mineral) %>% 
                summarise(soma = sum(Quantidade.Venda)),
              key = Ano.base,
              value = soma
            )
          
          return(x)
          
        }
      }
    }
  }



#_____BRUTA_Quantidade_groupBY_UF ----
BRUTA_Quantidade_groupBY_UF <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "rom") {
    if (volume == "rom") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, UF) %>%
            summarise(soma = sum(Quantidade.Produção.Minério.ROM)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "contido") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, UF) %>% 
              summarise(soma = sum(Quantidade.Contido)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "Quantidade.Venda") {
          x <-
            spread(
              select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                     grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                     grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
                group_by(Ano.base, UF) %>% 
                summarise(soma = sum(Quantidade.Venda)),
              key = Ano.base,
              value = soma
            )
          
          return(x)
          
        }
      }
    }
  }


#_____BRUTA_Valor_groupBY_SUBSTANCIA ----
BRUTA_Valor_groupBY_SUBSTANCIA <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "vpm") {
    if (volume == "vpm") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, Substância.Mineral) %>%
            summarise(soma = sum(Valor.Venda, Valor.Transformação.Consumo.Utilização.nesta.mina, Valor.Transferência.para.Transformação.Utilização.Consumo)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, Substância.Mineral) %>% 
              summarise(soma = sum(Valor.Venda)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      }
    }
  }



#_____BRUTA_Valor_groupBY_UF ----
BRUTA_Valor_groupBY_UF <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "vpm") {
    if (volume == "vpm") {
      x <-
        spread(
          select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, UF) %>%
            summarise(soma = sum(Valor.Venda, Valor.Transformação.Consumo.Utilização.nesta.mina, Valor.Transferência.para.Transformação.Utilização.Consumo)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBRUTA[grepl(producaoBRUTA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBRUTA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBRUTA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, UF) %>% 
              summarise(soma = sum(Valor.Venda)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      }
    }
  }


# Funções PRODUÇÃO BENEFICIADA ----

#_____BENEFICIADA_Quantidade_groupBY_SUBSTANCIA ----
BENEFICIADA_Quantidade_groupBY_SUBSTANCIA <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "Producao") {
    if (volume == "Producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, Substância.Mineral) %>%
            summarise(soma = sum(Quantidade.Produção)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "Contido") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, Substância.Mineral) %>% 
              summarise(soma = sum(Quantidade.Contido)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "Venda") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                     grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                     grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
                group_by(Ano.base, Substância.Mineral) %>% 
                summarise(soma = sum(Quantidade.Venda)),
              key = Ano.base,
              value = soma
            )
          
          return(x)
          
        }
      }
    }
  }



#_____BENEFICIADA_Quantidade_groupBY_UF ----
BENEFICIADA_Quantidade_groupBY_UF <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "Producao") {
    if (volume == "Producao") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                       grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                       grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, UF) %>%
            summarise(soma = sum(Quantidade.Produção)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "Contido") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                         grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                         grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, UF) %>% 
              summarise(soma = sum(Quantidade.Contido)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      } else {
        if (volume == "Venda") {
          x <-
            spread(
              select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                           grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                           grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
                group_by(Ano.base, UF) %>% 
                summarise(soma = sum(Quantidade.Venda)),
              key = Ano.base,
              value = soma
            )
          
          return(x)
          
        }
      }
    }
  }


#_____BENEFICIADA_Valor_groupBY_SUBSTANCIA ----
BENEFICIADA_Valor_groupBY_SUBSTANCIA <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "vpm") {
    if (volume == "vpm") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, Substância.Mineral) %>%
            summarise(soma = sum(Valor.Venda, Valor.Consumo.Utilização.na.Usina, Valor.Transferência.para.Transformação.Utilização.Consumo)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "Venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, Substância.Mineral) %>% 
              summarise(soma = sum(Valor.Venda)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      }
    }
  }



#_____BENEFICIADA_Valor_groupBY_UF ----
BENEFICIADA_Valor_groupBY_UF <-
  function(SUBS_Agrupadora = ".",
           SUBS_Classe = ".",
           UF = ".",
           volume = "vpm") {
    if (volume == "vpm") {
      x <-
        spread(
          select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                 grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                 grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,],
                 everything()) %>%
            group_by(Ano.base, UF) %>%
            summarise(soma = sum(Valor.Venda, Valor.Consumo.Utilização.na.Usina, Valor.Transferência.para.Transformação.Utilização.Consumo)),
          key = Ano.base,
          value = soma
        )
      
      return(x)
      
    } else {
      if (volume == "venda") {
        x <-
          spread(
            select(producaoBENEFICIADA[grepl(producaoBENEFICIADA$Substância.Mineral, pattern = SUBS_Agrupadora) == TRUE &
                                   grepl(producaoBENEFICIADA$Classe.Substância, pattern = SUBS_Classe) == TRUE &
                                   grepl(producaoBENEFICIADA$UF, pattern = UF) == TRUE ,], everything()) %>%
              group_by(Ano.base, UF) %>% 
              summarise(soma = sum(Valor.Venda)),
            key = Ano.base,
            value = soma
          )
        
        return(x)
      }
    }
  }

