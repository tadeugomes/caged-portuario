# Configurar codificação UTF-8 globalmente
options(encoding = "UTF-8")     


library(basedosdados)
library(tidyverse)

# Detach plyr if it's loaded to avoid conflicts
if ("package:plyr" %in% search()) {
  detach("package:plyr", unload=TRUE)
}


# Defina o seu projeto no Google Cloud
set_billing_id("observatorio-portuario")


# Criação da query utilizando SQL diretamente
query <- "
SELECT 
  ano,
  mes,
  sigla_uf,
  saldo_movimentacao,
  id_municipio,
  cnae_2_secao,
  categoria,
  cnae_2_subclasse,
  cbo_2002,
  grau_instrucao,
  sexo,
  salario_mensal,
  indicador_trabalho_intermitente,
  indicador_trabalho_parcial
FROM `basedosdados.br_me_caged.microdados_movimentacao`
WHERE cnae_2_subclasse IN (
      --essas duas subclasses são da divisão 50 TRANSPORTE AQUAVIÁRIO
      '5231101', --Administração da infraestrutura portuária
      '5231102', --Atividades do operador portuário
      --essas outras são novas
      '5011401', --Transporte marítimo de cabotagem - carga
      '5011402', --Transporte marítimo de cabotagem - passageiros
      '5021101', --Transporte por navegação interior de carga, municipal, exceto travessia
      '5021102', --Transporte por navegação interior de carga, intermunicipal, interestadual e internacional, exceto travessia
      '5022001', --Transporte por navegação interior de passageiros em linhas regulares, municipal, exceto travessia
      '5022002', --Transporte por navegação interior de passageiros em linhas regulares, intermunicipal, interestadual e internacional, exceto travessia
      '5030101', --Navegação de apoio marítimo
      '5030102', --Navegação de apoio portuário
      '5030103', --Serviço de rebocadores e empurradores
      '5091201', --Transporte por navegação de travessia, municipal
      '5091202', --Transporte por navegação de travessia intermunicipal, interestadual e internacional
      '5099801', --Transporte aquaviário para passeios turísticos
      '5099899', --Outros transportes aquaviários não especificados anteriormente
      --novas subclasses incluídas
      '5012201', --Transporte marítimo de longo curso - carga
      '5012202', --Transporte marítimo de longo curso - passageiros
      '5231103', --Gestão de terminais aquaviários
      '5232000', --Atividades de agenciamento marítimo
      '5239701', --Serviços de praticagem
      '5239799' --Atividades auxiliares dos transportes aquaviários não especificadas anteriormente
)"

# Coleta dos dados
df <- read_sql(query)
# Salvar os dados em csv
saveRDS(df, file = "df.rds")

# Salvar os dados em csv
write.csv(df, "dados_caged.csv", row.names = FALSE)


###################

