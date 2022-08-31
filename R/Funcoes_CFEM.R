library(tidyverse)

# Fun??es ----




# FUNA_CFEM_Substancia_Ano
FUNA_CFEM_GroupBY_Substancia_Ano <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Ano, Substância) %>%
            summarise(sum(ValorRecolhido)),
          key = Ano,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Ano, Substância) %>%
            summarise(sum(valor.Real)),
          key = Ano,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }


# FUNA_CFEM_Substancia_Semestre
FUNA_CFEM_GroupBY_Substancia_Semestre <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(semestre, Substância) %>%
            summarise(sum(ValorRecolhido)),
          key = semestre,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(semestre, Substância) %>%
            summarise(sum(valor.Real)),
          key = semestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }





# FUNA_CFEM_Substancia_Trimestre
FUNA_CFEM_GroupBY_Substancia_Trimestre <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(trimestre, Substância) %>%
            summarise(sum(ValorRecolhido)),
          key = trimestre,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(trimestre, Substância) %>%
            summarise(sum(valor.Real)),
          key = trimestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Substancia_M?s
FUNA_CFEM_GroupBY_Substancia_Mes <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Período, Substância) %>%
            summarise(sum(ValorRecolhido)),
          key = Período,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Período, Substância) %>%
            summarise(sum(valor.Real)),
          key = Período,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Ano
FUNA_CFEM_GroupBY_Ano <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Ano) %>%
            summarise(sum(ValorRecolhido)),
          key = Ano,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Ano) %>%
            summarise(sum(valor.Real)),
          key = Ano,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Semestre
FUNA_CFEM_GroupBY_Semestre <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(semestre) %>%
            summarise(sum(ValorRecolhido)),
          key = semestre,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(semestre) %>%
            summarise(sum(valor.Real)),
          key = semestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }



# FUNA_CFEM_Trimestre
FUNA_CFEM_GroupBY_Trimestre <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(trimestre) %>%
            summarise(sum(ValorRecolhido)),
          key = trimestre,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(trimestre) %>%
            summarise(sum(valor.Real)),
          key = trimestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }



# FUNA_CFEM_M?s
FUNA_CFEM_GroupBY_Mes <-
  function(Substância = ".",
           CPF_CNPJ = ".",
           Município = ".",
           UF = ".",
           Ano = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Período) %>%
            summarise(sum(ValorRecolhido)),
          key = Período,
          value = `sum(ValorRecolhido)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(CFEM_Arrecadacao[grepl(CFEM_Arrecadacao$Substância, pattern = Substância) == TRUE &
                           grepl(CFEM_Arrecadacao$CPF_CNPJ, pattern = CPF_CNPJ) == TRUE &
                           grepl(CFEM_Arrecadacao$Município, pattern = Município) == TRUE &
                           grepl(CFEM_Arrecadacao$UF, pattern = UF) == TRUE &
                           grepl(CFEM_Arrecadacao$Ano, pattern = Ano) == TRUE, ], everything()) %>%
            group_by(Período) %>%
            summarise(sum(valor.Real)),
          key = Período,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }

