library(magrittr)
library(forcats)
library(novocaged)
library(tidyverse)
library(readr)

#### Script para preparar os dados #######

# pasta de trabalho

setwd("C:/Users/tadeu/OneDrive/Documentos/GitHub/caged-portuario/atualizacao_csv")

# Baixar os dados 
#https://github.com/gustavoppmonteiro/novocaged 
# ftp://ftp.mtps.gov.br/pdet/microdados/NOVO CAGED/   

novocaged::baixa_novo_caged(ano = 2024,
                            mes_inicial = 4,
                            mes_final = 4,
                            nome_pasta="atualizacao",
                            nome_arquivo = "abril_24")

# Testar leitura de arquivo baixado 
abril.24 <- arrow::read_parquet("maio.parquet")

# muda formato do arquivo "cagedJanFev22.parquet" para .csv e salva na pasta "atualizacao_csv"

novocaged::salva_parquet_em_csv(caminho_parquet = "maio.parquet",
                                caminho_csv = "maio.csv")

#Create a vector of file names using the list.files() function.

csv_files <- list.files(pattern = "*.csv")


#Use a loop to read each CSV file into a data frame. You can use the read.csv() function to read each file. 

# Step 3: Read and merge the CSV files

merged_data <- data.frame() # create an empty data frame to store the merged data

for (i in 1:length(csv_files)) {
filename <- csv_files[i]
df <- read.csv(filename, header = TRUE, stringsAsFactors = FALSE) # read each CSV file
merged_data <- rbind(merged_data, df) # merge the data frames
}


# Filtro com cnaes do setor portuário

caged_labor<-merged_data %>% 
  filter(subclasse %in% c(5231101,5231102, 5239700, 5011401, 5011402, 5021101, 5021102, 5022001, 5022002, 5030101, 
                          5030102, 5030103, 5091201, 5091202, 5099801, 5099899, 5232000, 5250802))



# Voltar para pasta de trabalho do projeto

setwd("C:/Users/tadeu/OneDrive/Documentos/GitHub/caged-portuario")



# Criar coluna de data 
caged_labor$date <- as.Date(paste0(caged_labor$competenciamov, "01"), format = "%Y%m%d")


#### Organizar os dados para fazer o label recode

#lendo dados das subclasses da cnae

library(readxl)

subclasses <- readxl::read_xls(
  "cnae_subclasse.xls",
  col_names = c("secao", "divisao", "grupo", 
                "classe", "subclasse", "denominacao"),
  skip = 5
)



#definindo vetor de letras maiúsculas
letras <- LETTERS[seq(1, 26)]

# filtrando a coluna secao pelas letras que identificão as seções
# e por linhas que possuam valores NA
#isso buscar retirar os cabeçalhos do arquivos cnae_subclasse.xls

subclasses <- 
  subclasses %>% 
  filter(secao %in% letras | is.na(secao))

# selecionando APENAS as seções da cnae

secao <- 
  subclasses %>% 
  filter(!is.na(secao)) %>% 
  select(secao, denominacao) %>% 
  rename(n_secao = denominacao)

# selecionando APENAS as divisões da cnae
divisao <- 
  subclasses %>% 
  filter(!is.na(divisao)) %>% 
  select(divisao, denominacao) %>% 
  rename(n_divisao = denominacao)

# selecionando APENAS as grupos da cnae
grupo <- 
  subclasses %>% 
  filter(!is.na(grupo)) %>% 
  select(grupo, denominacao) %>% 
  rename(n_grupo = denominacao)

# selecionando APENAS as classes da cnae
classe <- 
  subclasses %>% 
  filter(!is.na(classe)) %>% 
  select(classe, denominacao) %>% 
  rename(n_classe = denominacao)


# autocompletando as colunas para baixo
subclasses <- 
  subclasses %>% 
  fill(secao:classe, .direction = "down")

# filtrando tudo aquilo que não é NA coluna subclasse
#tabela limpa

subclasses <- 
  subclasses %>% 
  filter(!is.na(subclasse))


################## Fazendo o join #################
#da tabela secao com a tabela subclasses

subclasses  <- 
  left_join(subclasses, #primeira tabela
            secao, #segunda tabela
            by = "secao")


#da tabela divisao com a tabela subclasses
subclasses  <- 
  left_join(subclasses, #primeira tabela
            divisao, #segunda tabela
            by = "divisao")

#da tabela grupo com a tabela subclasses
subclasses  <- 
  left_join(subclasses, #primeira tabela
            grupo, #segunda tabela
            by = "grupo")

#da tabela classe com a tabela subclasses
subclasses  <- 
  left_join(subclasses, #primeira tabela
            classe, #segunda tabela
            by = "classe")

# substituindo todas as pontuações por nada das colunas
#grupo, classe, subclasse
subclasses <-
  subclasses %>% 
  mutate_at(.vars = vars("grupo", "classe", "subclasse"), .funs = ~str_replace_all(., "[:punct:]", ""))


################## Filtrar o mês para análise para o BR ###################################

last_month_br<- caged_labor %>% 
  filter(date == max(date))

#da variáveilsubclasse

last_month_br$subclasse<- factor(last_month_br$subclasse)


last_month_br <- 
  left_join(last_month_br, #primeira tabela
            subclasses, #segunda tabela
            by = c("subclasse" = "subclasse")) %>% 
  mutate(subclasse = denominacao) %>% #substituindo 
  select(!denominacao)

###


############### Transformar a coluna CBO para o tipo factor ######################## 

caged_labor$cbo2002ocupacao<- factor(caged_labor$cbo2002ocupacao)


#lendo arquivo da cbo 2002
cbo <- read_delim(file = 'cbo2002.csv', delim = ";",
                  col_type = list(.default = "f"),
                  locale = locale(encoding = "latin1"))


# substituindo o valor 517235 por 517335 da coluna CODIGO da tabela cbo
cbo <- 
  cbo %>% mutate(
    CODIGO = as.character(CODIGO),
    CODIGO = if_else(CODIGO == "517235", "517335", CODIGO),
    CODIGO = as.factor(CODIGO))



# Fazer o join da tavela do Caged e da CBO pela variáveil cbo_2002. Feito no outro arquivo


##### Fazer label encoding das colunas do tipo factor


caged_labor$indtrabparcial<-factor(caged_labor$indtrabparcial)

caged_labor$indtrabparcial<-fct_recode(caged_labor$indtrabparcial, 
                                       'Sim' = '1', 'Nao' = '0', 'NA' = '9')

caged_labor$indtrabintermitente<-factor(caged_labor$indtrabintermitente)

caged_labor$indtrabintermitente<-fct_recode(caged_labor$indtrabintermitente, 
                                            "Sim" = "1", "Nao" = "0",  'NA' = '9')



caged_labor$indtrabintermitente<-factor(caged_labor$indtrabintermitente)

caged_labor$indtrabintermitente<-fct_recode(caged_labor$indtrabintermitente, 
                                            "Sim" = "1", "Nao" = "0",  'NA' = '9')



caged_labor$graudeinstrucao<- factor(caged_labor$graudeinstrucao)

caged_labor$graudeinstrucao<- fct_recode (caged_labor$graudeinstrucao,    
                                   'Analfabeto' = '1', 'Até 5ª Incompleto' = '2', '5ª Completo Fundamental' = '3', '6ª a 9ª Fundamental' = '4', 'Fundamental Completo' = '5',
                                   'Médio Incompleto' = '6', 'Médio Completo' = '7', 'Superior Incompleto' = '8', 'Superior Completo' = '9', '10' = 'Mestrado', '11' = 'Doutorado', '80' =	'Especialização'
)


caged_labor$sexo<-factor(caged_labor$sexo, labels=c("Masculino", "Feminino"))

######################

