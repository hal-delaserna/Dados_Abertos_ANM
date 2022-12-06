library(tidyverse)
library(shiny)
library(shinydashboard)
options(editor = 'notepad')

# RAIS 2010 - 2019 ----
RAIS_2010_2019_VINC <- 
  readRDS('D:/Users/humberto.serna/Documents/D_Lake/RAIS_CAGED/RAIS_2010_2019_VINC.Rda')

# CNAE 2.3 ----
source('D:/Users/humberto.serna/Documents/D_Lake/CNAE.R')


# USER INTERFACE ---------------------------------------------------------------

ui <-
  dashboardPage(
    dashboardHeader(title = "Produção Mineral Brasileira"), # ____ Header ----
                dashboardSidebar(sidebarMenu( # ____ Sidebar / SidebarMenu ----
                  menuItem(text = "Produção Bruta", tabName = "bruta"),
                  menuItem(text = "Produção Beneficiada", tabName = "beneficiada"),
                  menuItem(text = "Água Mineral", tabName = "agua")
                )),
                dashboardBody(tabItems( # ____ dashboardBody ----
                  tabItem(
                    tabName = "bruta",
                    fluidRow(column(
                      width = 12,
                      h2('Produção Bruta nas dimensões valor e massa')
                    )),
                    br(),
                    fluidRow(
                      infoBoxOutput(outputId = "num_metalicas", width = 4),
                      infoBoxOutput(outputId = "num_naometalicas", width = 4),
                      infoBoxOutput(outputId = "num_energeticos", width = 4)
                    ),
                    fluidRow(column(
                      width = 12,
                      plotOutput(outputId = "grafico_vpm_ano")
                    ))
                  ),
                  tabItem(tabName = "beneficiada",
                          "b"),
                  tabItem(tabName = "agua",
                          "c")
                )))

# SERVER ----------------------------------------------------------------------
server <-
  function(input, output, session) {
    
    # carregamento ----
    producaoBruta <-
      read.table(
        'https://app.anm.gov.br/dadosabertos/AMB/Producao_Bruta.csv',
        fileEncoding = "iso-8859-1",
        header = TRUE,
        sep = ",",
        dec = ",",
        stringsAsFactors = FALSE
      )
    
    producaoBeneficiada <-
      read.table(
        # '/home/hal/Downloads/Producao_Bruta.csv',
        'https://app.anm.gov.br/dadosabertos/AMB/Producao_Beneficiada.csv',
        fileEncoding = "iso-8859-1",
        header = TRUE,
        sep = ",",
        dec = ",",
        stringsAsFactors = FALSE
      )
    
    aguaMineral <- 
      read.table(
        # '/home/hal/Downloads/Producao_Bruta.csv',
        'https://app.anm.gov.br/dadosabertos/AMB/Agua_Mineral_Producao.csv',
        fileEncoding = "iso-8859-1",
        header = TRUE,
        sep = ",",
        dec = ",",
        stringsAsFactors = FALSE
      )
    
    # renderInfoBox ----
    
    output$num_metalicas <- renderInfoBox({
      numero_de_metalicas <-
        length(unique(producaoBruta[producaoBruta$Classe.Substância == "Metálicos",]$Substância.Mineral))
      infoBox(
        title = "Metálicos",
        value = numero_de_metalicas,
        color = "orange",
        #icon = icon("atom"),
        fill = TRUE
      )   # infoBox Aceita hiperlink
    })
    
    output$num_naometalicas <- renderInfoBox({
      numero_de_naometalicas <-
        length(unique(producaoBruta[producaoBruta$Classe.Substância == "Não-Metálicos",]$Substância.Mineral))
      infoBox(
        title = "Não-Metálicos",
        value = numero_de_naometalicas,
        color = "orange",
        #icon = icon("atom"),
        fill = TRUE
      )   # infoBox Aceita hiperlink
    })
    
    output$num_energeticos <- renderInfoBox({
      numero_de_energeticos <-
        length(unique(producaoBruta[producaoBruta$Classe.Substância == "Energéticos",]$Substância.Mineral))
      infoBox(
        title = "Energéticos",
        value = numero_de_energeticos,
        color = "orange",
        #icon = icon("atom"),
        fill = TRUE
      )   # infoBox Aceita hiperlink
    })
    
    # renderPlot VPM ----
    
    
    output$grafico_vpm_ano <- renderPlot({
      vpm <-
        rbind(
          summarise(group_by(producaoBruta, Ano.base),"vpm" = sum(Valor.Transferência.para.Transformação...Utilização...Consumo..R..,Valor.Transformação...Consumo...Utilização.nesta.mina..R..,Valor.Venda..R..)),
          summarise(group_by(producaoBeneficiada, Ano.base),"vpm" = sum(Valor.Venda..R..,Valor.Transferência.para.Transformação...Utilização...Consumo..R..,Valor.Consumo...Utilização.na.Usina..R..)),
          summarise(group_by(producaoAguaMineral, Ano.base),"vpm" = sum(Valor..R...Garrafão,Valor..R...Garrafa.Plástica,Valor..R...Garrafa.de.Vidro,Valor..R...Copo,Valor..R...Outras.Embalagens,Valor..R...Composição.Produtos.Industrializados)))
      
      ggplot(vpm) + geom_bar(mapping = aes(x = as.factor(Ano.base), y = vpm), stat = "identity", fill = "#955000") + theme_minimal()
      })
    
  }

shinyApp(ui, server)




