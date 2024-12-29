
library(tidyverse)

#### Leitura do arquivo salvo 


df <- readRDS("df.rds")

glimpse()

# Crie uma nova coluna de data


# Verifica se há NA e lida com eles antes de criar a coluna data
df <- df %>%
  # Remover ou substituir NAs nos anos e meses (depende do que você quer fazer com os NAs)
  filter(!is.na(ano) & !is.na(mes)) %>%
  # Certificar que ano e mês estão no formato numérico e são inteiros
  mutate(
    ano = as.integer(ano),
    mes = as.integer(mes),
    # Cria a coluna data garantindo que o mês seja sempre de dois dígitos
    data = ymd(paste(ano, sprintf("%02d", mes), "01"))
  )


##### Fazer label encoding das colunas do tipo factor

df <- df %>%
  mutate(cnae_2_subclasse = recode(cnae_2_subclasse,
                                   '5231101' = 'Administração da infraestrutura portuária',
                                   '5231102' = 'Atividades do operador portuário',
                                   '5011401' = 'Transporte marítimo de cabotagem - carga',
                                   '5011402' = 'Transporte marítimo de cabotagem - passageiros',
                                   '5021101' = 'Transporte por navegação interior de carga, municipal, exceto travessia',
                                   '5021102' = 'Transporte por navegação interior de carga, intermunicipal, interestadual e internacional, exceto travessia',
                                   '5022001' = 'Transporte por navegação interior de passageiros em linhas regulares, municipal, exceto travessia',
                                   '5022002' = 'Transporte por navegação interior de passageiros em linhas regulares, intermunicipal, interestadual e internacional, exceto travessia',
                                   '5030101' = 'Navegação de apoio marítimo',
                                   '5030102' = 'Navegação de apoio portuário',
                                   '5030103' = 'Serviço de rebocadores e empurradores',
                                   '5091201' = 'Transporte por navegação de travessia, municipal',
                                   '5091202' = 'Transporte por navegação de travessia intermunicipal, interestadual e internacional',
                                   '5099801' = 'Transporte aquaviário para passeios turísticos',
                                   '5099899' = 'Outros transportes aquaviários não especificados anteriormente',
                                   '5012201' = 'Transporte marítimo de longo curso - carga',
                                   '5012202' = 'Transporte marítimo de longo curso - passageiros',
                                   '5231103' = 'Gestão de terminais aquaviários',
                                   '5232000' = 'Atividades de agenciamento marítimo',
                                   '5239701' = 'Serviços de praticagem',
                                   '5239799' = 'Atividades auxiliares dos transportes aquaviários não especificadas anteriormente'))


df$indicador_trabalho_parcial <- factor(df$indicador_trabalho_parcial)

df$indicador_trabalho_parcial <- fct_recode(df$indicador_trabalho_parcial, 
                                                 'Sim' = '1', 'Nao' = '0', 'NA' = '9')

df$indicador_trabalho_intermitente <- factor(df$indicador_trabalho_intermitente)

df$indicador_trabalho_intermitente <- fct_recode(df$indicador_trabalho_intermitente, 
                                                      "Sim" = "1", "Nao" = "0", 'NA' = '9')

df$grau_instrucao <- factor(df$grau_instrucao)

df$grau_instrucao <- fct_recode(df$grau_instrucao,    
                                     'Analfabeto' = '1', 
                                     'Até 5ª Incompleto' = '2', 
                                     '5ª Completo Fundamental' = '3', 
                                     '6ª a 9ª Fundamental' = '4', 
                                     'Fundamental Completo' = '5',
                                     'Médio Incompleto' = '6', 
                                     'Médio Completo' = '7', 
                                     'Superior Incompleto' = '8', 
                                     'Superior Completo' = '9', 
                                     'Mestrado' = '10', 
                                     'Doutorado' = '11', 
                                     'Especialização' = '80')



df$saldo_movimentacao <- factor(df$saldo_movimentacao)

df$saldo_movimentacao <- fct_recode(df$saldo_movimentacao, 
                                         "Admitidos" = "1", "Desligados" = "-1")



df$sexo <- factor(df$sexo)

df$sexo <- fct_recode(df$sexo, 
                           "Homem" = "1", "Mulher" = "3")



##### Mudar o tipo de dado para fazer o join abaixo
df$cbo_2002<- factor(df$cbo_2002)


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




#da variÃ¡veil cbo_2002
df <- 
  left_join(df, #primeira tabela
            cbo, #segunda tabela
            by = c("cbo_2002" = "CODIGO")) %>% 
  mutate(cbo_2002 = TITULO) %>% #substituindo 
  select(!TITULO)


# Define um vetor com as regiÃµes para criar uma nova coluna

regioes <- list(
  "Norte" = c("RO", "AC", "AM", "RR", "PA", "AP", "TO"),
  "Nordeste" = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
  "Centro-Oeste" = c("MS", "MT", "GO", "DF"),
  "Sudeste" = c("MG", "ES", "RJ", "SP"),
  "Sul" = c("PR", "SC", "RS")
)

# FunÃ§Ã£o para retornar a regiÃ£o a partir da sigla
get_regiao <- function(sigla) {
  regiao <- NA
  for (r in names(regioes)) {
    if (sigla %in% regioes[[r]]) {
      regiao <- r
      break
    }
  }
  return(regiao)
}

# Aplica a funÃ§Ã£o para criar a nova coluna regiao
df$regiao <- sapply(df$sigla_uf, get_regiao)


write.csv(df, "dados/tratados/df_limpo.csv", row.names = FALSE)



# Seleciona o ultimo dado de mes disponivel para analise do mes vigente

ultimo_dado <- df %>%
  filter(!is.na(data)) %>%
  filter(data == max(data, na.rm = TRUE))


write.csv(ultimo_dado, "dados/tratados/ultimo_dado.csv", row.names = FALSE)







library(dplyr)
library(lubridate)

# 1) Criar a coluna numérica que converte "Admitidos" e "Desligados"
df_limpo_saldo <- df_limpo %>%
  mutate(
    saldo_movimentacao_num = case_when(
      saldo_movimentacao == "Admitidos"  ~  1,
      saldo_movimentacao == "Desligados" ~ -1,
      TRUE ~ NA_real_
    )
  )

# 2) Agregar por UF, ano e mês, obtendo saldo_mensal e criando data_mensal para ordenação
df_mensal <- df_limpo_saldo %>%
  group_by(sigla_uf, ano, mes) %>%
  summarise(
    saldo_mensal = sum(saldo_movimentacao_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    data_mensal = as.Date(paste(ano, mes, "01", sep = "-"))
  )

# 3) Calcular variações trimestral (3 meses) e anual (12 meses)
df_variacao <- df_mensal %>%
  arrange(sigla_uf, data_mensal) %>%
  group_by(sigla_uf) %>%
  mutate(
    saldo_trimestral_anterior = lag(saldo_mensal, 3),
    dif_trimestral            = saldo_mensal - saldo_trimestral_anterior,
    pct_trimestral            = round((dif_trimestral / saldo_trimestral_anterior) * 100, 2),
    
    saldo_anual_anterior      = lag(saldo_mensal, 12),
    dif_anual                 = saldo_mensal - saldo_anual_anterior,
    pct_anual                 = round((dif_anual / saldo_anual_anterior) * 100, 2)
  ) %>%
  ungroup()


