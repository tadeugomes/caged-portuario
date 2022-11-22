
library(tidyverse)


caged <- read.csv("dados.csv")

View(caged)

caged$sexo<-factor(caged$sexo, labels=c("Masculino", "Feminino"))



caged$grau_instrucao<- factor(caged$grau_instrucao)

caged$grau_instrucao<- fct_recode (caged$grau_instrucao,    
                                   'Analfabeto' = '1', 'Até 5ª Incompleto' = '2', '5ª Completo Fundamental' = '3', '6ª a 9ª Fundamental' = '4', 'Fundamental Completo' = '5',
                                   'Médio Incompleto' = '6', 'Médio Completo' = '7', 'Superior Incompleto' = '8', 'Superior Completo' = '9', 'Nao informado' = '80')


