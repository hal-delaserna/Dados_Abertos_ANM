library(tidyverse)

# Funções ----




# FUNA_CFEM_Substancia_Ano
FUNA_CFEM_GroupBY_Substancia_Ano <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(periodo, substancia.SCM) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = periodo,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(periodo, substancia.SCM) %>%
            summarise(sum(valor.Real)),
          key = periodo,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }


# FUNA_CFEM_Substancia_Semestre
FUNA_CFEM_GroupBY_Substancia_Semestre <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(semestre, substancia.SCM) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = semestre,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(semestre, substancia.SCM) %>%
            summarise(sum(valor.Real)),
          key = semestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }





# FUNA_CFEM_Substancia_Trimestre
FUNA_CFEM_GroupBY_Substancia_Trimestre <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(trimestre, substancia.SCM) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = trimestre,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(trimestre, substancia.SCM) %>%
            summarise(sum(valor.Real)),
          key = trimestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Substancia_Mês
FUNA_CFEM_GroupBY_Substancia_Mes <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(mes.de.referencia, substancia.SCM) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = mes.de.referencia,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(mes.de.referencia, substancia.SCM) %>%
            summarise(sum(valor.Real)),
          key = mes.de.referencia,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Ano
FUNA_CFEM_GroupBY_Ano <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(periodo) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = periodo,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(periodo) %>%
            summarise(sum(valor.Real)),
          key = periodo,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }




# FUNA_CFEM_Semestre
FUNA_CFEM_GroupBY_Semestre <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(semestre) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = semestre,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
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
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(trimestre) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = trimestre,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(trimestre) %>%
            summarise(sum(valor.Real)),
          key = trimestre,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }



# FUNA_CFEM_Mês
FUNA_CFEM_GroupBY_Mes <-
  function(substancia = ".",
           cpfcnpj = ".",
           municipio = ".",
           uf = ".",
           anos = ".",
           nominal = TRUE) {
    if (nominal == TRUE) {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(mes.de.referencia) %>%
            summarise(sum(valor.Recolhido.CFEM)),
          key = mes.de.referencia,
          value = `sum(valor.Recolhido.CFEM)`
        )
      
      return(x)
      
    } else {
      x <-
        spread(
          select(cfem_BR[grepl(cfem_BR$substancia.SCM, pattern = substancia) == TRUE &
                           grepl(cfem_BR$cpfcnpj, pattern = cpfcnpj) == TRUE &
                           grepl(cfem_BR$municipio, pattern = municipio) == TRUE &
                           grepl(cfem_BR$uf, pattern = uf) == TRUE &
                           grepl(cfem_BR$periodo, pattern = anos) == TRUE, ], everything()) %>%
            group_by(mes.de.referencia) %>%
            summarise(sum(valor.Real)),
          key = mes.de.referencia,
          value = `sum(valor.Real)`
        )
      return(x)
    }
  }

