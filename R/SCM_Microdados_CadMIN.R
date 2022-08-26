#    rm(list=ls())
library(tidyverse)
library(foreign)

# CARREGAMENTO E FORMATAÇÃO ----

CondicaoPropriedadeSolo <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/CondicaoPropriedadeSolo.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
DocumentoLegal <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/DocumentoLegal.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Evento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/Evento.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
FaseProcesso <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/FaseProcesso.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
MotivoEncerramentoSubstancia <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/MotivoEncerramentoSubstancia.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Municipio <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/Municipio.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Pessoa <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/Pessoa.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Processo <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/Processo.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoAssociacao <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoAssociacao.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoDocumentacao <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoDocumentacao.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoEvento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoEvento.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoMunicipio <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoMunicipio.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoPessoa <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoPessoa.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoPropriedadeSolo <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoPropriedadeSolo.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoSubstancia <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoSubstancia.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
ProcessoTitulo <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/ProcessoTitulo.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
SituacaoDocumentoLegal <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/SituacaoDocumentoLegal.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
Substancia <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/Substancia.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoAssociacao <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoAssociacao.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoDocumento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoDocumento.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoDocumentoLegal <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoDocumentoLegal.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoRelacao <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoRelacao.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoRepresentacaoLegal <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoRepresentacaoLegal.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoRequerimento <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoRequerimento.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoResponsabilidadeTecnica <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoResponsabilidadeTecnica.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
TipoUsoSubstancia <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/TipoUsoSubstancia.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
UnidadeAdministrativaRegional <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/UnidadeAdministrativaRegional.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)
UnidadeProtocolizadora <- read.table(file = paste(sep = "",Sys.getenv("R_USER"),'/D_Lake/Dados_Abertos_ANM/microdados-scm/UnidadeProtocolizadora.txt'), header = TRUE, sep = ";", quote = "", fill = TRUE, stringsAsFactors = FALSE)


# Delimitando Fases de interesse ----
DNPM_alvos <-
  lista_dnpm[lista_dnpm$Fase.Atual %in% c(
    #    "Guia_de_Utilizacao_Autorizada",
    "Licenciamento"#,
    #    "PLG",
    #    "Portaria_de_Lavra",
    #    "Registro_de_Extracao_Publicado"#,
    #    "Requerimento_de_Lavra",
    #    "Requerimento_de_Licenciamento",
    #    "Requerimento_de_Pesquisa",
    #    "Requerimento_de_PLG",
    #    "Requerimento_de_Registro_de_Extracao_Protocolizado"
  ), ]


# processos alvo ---

DNPM_alvos <-
  DNPM_alvos[grepl(DNPM_alvos$Municipio.s., pattern = "- SP"), ]

#DNPM_ativos[DNPM_ativos$Processo=="821030/2001",]
SP_dbf <-
  foreign::read.dbf(
    'D:/Users/humberto.serna/Desktop/Georeferenciamento/Poligonais/SP/SP.dbf',
    as.is = FALSE)




SP <- 
  merge(SP_dbf,DNPM_alvos[,-c(1,3)], by.x = "PROCESSO", by.y = "Processo")
colnames(SP)[12] <- "cpfcnpj"
# há dois registros duplicados. Execute o anexo.


SP$ativo <- 1
for (i in 1:nrow(SP)) {
  if (is.na(SP$cpfcnpj[i]) == TRUE) {
    SP$cpfcnpj[i] <- 0
    SP$ativo[i] <- 0
  }}

write.dbf(SP_dbf, file = "D:/Users/humberto.serna/Desktop/Georeferenciamento/Poligonais/SP_Copia/SP.dbf")




write.table(x = DNPM_alvos, file = 'clipboard', )



# TELA LIVRE ----


ProcessoSubstancia_ <-
  left_join(
    left_join(
      left_join(ProcessoSubstancia[, c("DSProcesso", "IDSubstancia", "IDTipoUsoSubstancia")],
                TipoUsoSubstancia[, c("IDTipoUsoSubstancia", "DSTipoUsoSubstancia")], by = "IDTipoUsoSubstancia"),
      Substancia[, c("IDSubstancia", "NMSubstancia")],
      by = "IDSubstancia"
    ),
    Processo[, c("DSProcesso", "BTAtivo")],
    by = "DSProcesso"
  )

Processos_Ativos <- 
  filter(ProcessoSubstancia_, BTAtivo == "S")

Processos_Ativos <-
  left_join(
    left_join(Processos_Ativos, ProcessoMunicipio, by = "DSProcesso"),
    Municipio, by = "IDMunicipio")


Processos_Ativos <-
  left_join(
    left_join(Processos_Ativos, 
              ProcessoPessoa[ProcessoPessoa$IDTipoRelacao %in% c(1),c("DSProcesso", "IDPessoa")], 
              by = "DSProcesso"), Pessoa, by = "IDPessoa")


Processos_Ativos <- 
  filter(Processos_Ativos, SGUF == "MG")


Grupamentos_Mineiros_MG <- 
  Processos_Ativos[grepl(Processos_Ativos$DSProcesso, pattern = "^9"), c(
    "NMPessoa",
    "NMSubstancia",
    "NMMunicipio",
    "SGUF",
    "DSProcesso",
    "NRCPFCNPJ",
    "TPPessoa"
  )]


# Processos_Inativos <- 
#   filter(ProcessoSubstancia_, BTAtivo == "N")





write.table(x = Grupamentos_Mineiros_MG, 
            file = "D:/Users/humberto.serna/Documents/Grupamentos_Mineiros_MG.csv", 
            sep = ";", dec = ",", row.names = FALSE)









