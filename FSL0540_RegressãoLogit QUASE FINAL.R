###################################################################
#                                                                 #
#   30 de julho de 2023      últ. mod. 19 de maio de 2024         #
#   FSL0540 Tópicos Avançados em Sociologia da Educação           #
#   Autor: Carolina Bueno Stefani e Guilherme Olímpio Fagundes    #
#   Script adaptado de: Fernanda Peres (Unifesp),                 #
#                       Vitor Alcantara (USP)                     #
#                                                                 #
###################################################################

# PASSO 1. Pacotes ------------------------------------------------

install.packages("pacman")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("misty")
install.packages("tidyverse")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Factoshiny")
install.packages("knitr")
install.packages("hrbrthemes")
install.packages("ggridges")
install.packages("forcats")

library(ggridges)
library(ggplot2)
library(knitr)
library(dplyr)
library(misty)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(pacman)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(forcats)

pacman::p_load(rio, psych, descr, gridExtra, ggpubr, scales, 
               patchwork, effects, reshape2, foreign, sjPlot,
               pROC,ROCR,stargazer, caret,car, nnet, AER, 
               lmtest, gtsummary, DescTools)


# PASSO 2. Manusear banco de dados --------------------------------------------

# Como fatores
s21 <- read.csv2("C:\\Users\\guilh\\Desktop\\SAEB2021_ALUNO.csv", 
                 stringsAsFactors=F)

# Selecionar apenas questionários respondidos
dados <- dplyr::filter(s21, IN_PREENCHIMENTO_QUESTIONARIO %in% "1")

# Selecionar apenas questionários de São Paulo
dados <- dplyr::filter(dados, ID_UF %in% "35")

# Selecionar variáveis de interesse
# VD: Escolha imediata 'expectativa==TX_RESP_Q21'
# VI: Escolaridade da mãe, Cor ou raça, NSE e cat. adm.

dados <- select(.data=dados,
                TX_RESP_Q21,  # Expectativa chr
                TX_RESP_Q07,  # Escolaridade da mãe chr
                TX_RESP_Q04,  # Cor ou raça chr
                INSE_ALUNO) # NSE por aluno chr

# Renomear as variáveis
dados <- rename(.data = dados,
                "NSEAluno"=INSE_ALUNO,
                "raça"=TX_RESP_Q04,
                "mae"=TX_RESP_Q07,
                "expectativa"=TX_RESP_Q21)


# Converter classe para numérico
dados$NSEAluno <- as.numeric(dados$NSEAluno) 
class(dados$NSEAluno)

# Eliminar valores nulos
anyNA(dados)
dados <- na.omit(dados)

# Filtrar valores de variáveis
dados <- dplyr::filter(.data=dados,
                       expectativa %in% c("A", "B", "C", "D"), # Tiramos C por ser muito desbalanceado?
                       mae %in% c("A", "B", "C", "D", "E", "F"),
                       raça %in% c("A", "B", "C", "D", "E"))

# Renomear valores
# 'expectativa'= antes excluímos o C, mas depois falar disso.
dados$expectativa <- dplyr::recode(dados$expectativa,
                                "A"="Estudar",
                                "B"="Trabalhar",
                                "C"="Estudar e Trabalhar", # O que fazer com isso?
                                "D"="Nao_Sabe")

# 'MAE' = Somamos PPI e Brancos+Amarelos
dados$raça <- dplyr::recode(dados$raça,
                            "A"="Branca",  
                            "B"="PPI",   
                            "C"="PPI",
                            "D"="Branca",
                            "E"="PPI") 

# 'MAE' = Somamos A+B+C =  87.256
dados$mae <- dplyr::recode(dados$mae,
                           "A"="Fund-", 
                           "B"="Fund-", 
                           "C"="Fund-", 
                           "D"="Fund+",  
                           "E"="Medio+", 
                           "F"="Superior+") 

dados <- mutate(.data = dados,
                faixasNSE = case_when(
                  NSEAluno < 4.5 ~ "Baixo",
                  NSEAluno >= 4.5 & NSEAluno < 5.5 ~ "Regular",
                  NSEAluno >= 5.5 ~ "Alto"))

glimpse(dados)
table(dados$raça)
table(dados$mae)
table(dados$faixasNSE)
prop.table(table(dados$raça))*100
prop.table(table(dados$mae))*100
prop.table(table(dados$faixasNSE))*100


# Transformamos em Factor
dados <-strings2factors(dados)
glimpse(dados)

# PASSO 3.Análise das frequências das categorias da variável dependente --------

summary(dados)

# PASSO 4. Checagem das categorias de referência -------------------------------

levels(dados$expectativa)  # Estudar e Trabalhar= categoria de referência
dados$expectativa <- relevel(dados$expectativa, ref="Estudar")
levels(dados$raça)      # Branca = categoria de referência
levels(dados$mae)       # Fund- = categoria de referência
dados$mae <- relevel(dados$mae, ref="Superior+")
levels(dados$faixasNSE) # Alta =categoria de referência

# PASSO 5. Checagem dos pressupostos -------------------------------------------

# A. Variável dependente nominal: SIM
# B. Independência das observações (sem medidas repetidas): SIM
# C. Ausência de multicolinearidade: SIM

# psych::pairs.panels(dados[3:7])

# Testar um modelo de regressão linear para testar multicolinearidade
m <- lm(as.numeric(expectativa) ~ mae + raça + faixasNSE, data=dados)
car::vif(m) # Se algum valor é 10, tem multicol.


# D. Independência de alternativas irrelevantes (teste Hausman-McFadden):
# Se não existisse uma das alternativas, teríamos a mesma coisa.  
install.packages("mlogit")
library(mlogit)

dadomnl <- mlogit::mlogit.data(dados, shape = "wide", choice="expectativa")

modiia <- mlogit::mlogit(expectativa ~ 1 | mae + raça + faixasNSE,
                         data=dados, shape="wide",
                         reflevel = "Estudar e Trabalhar")

modiia2 <- mlogit::mlogit(expectativa ~ 1 | mae + raça + faixasNSE,
                         data=dados, shape="wide",
                         reflevel = "Estudar e Trabalhar",
                         alt.subset = c("Estudar e Trabalhar", "Trabalhar"))

modiia3 <- mlogit::mlogit(expectativa ~ 1 | mae + raça + faixasNSE,
                          data=dados, shape="wide",
                          reflevel = "Estudar",
                          alt.subset = c("Estudar e Trabalhar", "Estudar"))

modiia4 <- mlogit::mlogit(expectativa ~ 1 | mae + raça + faixasNSE,
                          data=dados, shape="wide",
                          reflevel = "Estudar",
                          alt.subset = c("Estudar e Trabalhar", "Nao_Sabe"))

mlogit::hmftest(modiia, modiia2)
mlogit::hmftest(modiia, modiia3)
mlogit::hmftest(modiia, modiia4)

# PASSO 6. Construção do modelo e interpretação dos resultados -----------------

# Construção do modelo e modelo nulo (usando pacote nnet)
mod <- multinom(expectativa ~ raça + mae + faixasNSE, data=dados, model=TRUE)
mod0 <- multinom(expectativa ~ 1, data=dados, model=TRUE) # Previsor


# Ajuste de modelo 
anova(mod, mod0)  # p<0.05 O modelo nulo é, logo, diferente do alternativo! Bom.
DescTools::PseudoR2(mod, which = "Nagelkerke") 

# Efeitos gerais
car::Anova(mod, type = "II", test="Wald")
# Se o Pr(>Chisq) for maior que 0.5, não é bom preditor... mas todos aqui são!

# Efeitos específicos
summary(mod)
 

# Obtenção dos valores de p por Wald (lmtest)
lmtest::coeftest(mod)
# 1) Coeficiente!
# 2) Ignoramos intercepts
# 3) Todos são previsores signitivativas para Estudar, Não_Sabe e Trabalhar
# 4) Positivo: A mãe com Superior completo, quando comparados com Fund-, o que
# significa que estudantes com m_S+ tem mais chances de não saber do que com 
# mães Fund-

## Obtenção das razões de chance com IC 95% (usando log-likelihood)
exp(coef(mod)) 
# As chances de assinalar Trabalhar é 2 vezes maior para uma pessoa preta do que
# para uma pessoa branca 


exp(confint(mod)) #IC
# Graças ao IC, tem que ser diferente a 1 (maeFund+, maeMédio+)

## Tabela completa (pacote gtsummary)
gtsummary::tbl_regression(mod, exponentiate = FALSE)
gtsummary::tbl_regression(mod, exponentiate = TRUE)


# PASSO 7. Segundo modelo ------------------------------------------------------

mod2 <- multinom(expectativa ~ raça + faixasNSE, data=dados, model=T) # Exclui escM

anova(mod0, mod2) # Ajuste do modelo 
DescTools::PseudoR2(mod2, which = "Nagelkerke")
Anova(mod2, type="II", test="Wald") # Efeitos gerais
summary(mod2)
lmtest::coeftest(mod2)
exp(coef(mod2))
exp(confint(mod2)) #IC


# PASSO 8. Comparando modelos

# AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)

# Qui-quadrado
anova(mod2, mod, test = "Chisq")

# PASSO 9. Tabelas e gráficos --------------------------------------------------

# Pelo modelo 1
tab <- table(Observado =dados$expectativa, Previsto = predict(mod))
prop.table(tab)*100

acuracia = sum(diag(tab)) / sum(tab)
acuracia

caret::confusionMatrix(predict(mod2), dados$expectativa)
caret::confusionMatrix(predict(mod), dados$expectativa)

# Pelo modelo 2
tab <- table(Observado =dados$expectativa, Previsto = predict(mod2))
prop.table(tab)*100

acuracia = sum(diag(tab)) / sum(tab)
acuracia

caret::confusionMatrix(predict(mod2), dados$expectativa)
caret::confusionMatrix(predict(mod), dados$expectativa)

# Gráficos

predictions <- predict(mod, type = "probs", se=T)
dados_prev <- cbind(dados[c("mae", "raça", "faixasNSE", "NSEAluno")], predictions)

dados_prev_long <- reshape2::melt(dados_prev,
                                  id.vars = c("mae","raça","faixasNSE","NSEAluno"),
                                  value.name = "Probabilidades",
                                  variable.name="Expectativa")

summary(dados_prev_long)
class(dados_prev_long$Probabilidades)

install.packages("readr")
library(readr)

write.csv(dados_prev_long, "dados_prev_long.csv")
exists("dados_prev_long")
getwd()

# MAE
reg_mae <- ggplot(dados_prev_long, aes(x= NSEAluno, y = Probabilidades, color = mae))+
              geom_smooth(method="gam")+
              labs(x = "Escore socioeconômico")+
              scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
              facet_grid(Expectativa ~ .)+
              theme_bw()+
              guides(color = guide_legend(override.aes = list(fill = NA)))

# Raça
reg_raça <- ggplot(dados_prev_long, aes(x= NSEAluno, y = Probabilidades, color = raça))+
              geom_smooth(method="gam")+
              labs(x = "Escore socioeconômico")+
              scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
              facet_grid(Expectativa ~ .)+
              theme_bw()+
              guides(color = guide_legend(override.aes = list(fill = NA)))

# nse
reg_nse <- ggplot(dados_prev_long, aes(x= NSEAluno, y = Probabilidades, color = faixasNSE))+
  geom_smooth(method="gam")+
  labs(x = "Escore socioeconômico")+
  scale_y_continuous(labels = scales::percent_format(decimal.mark = ","))+
  facet_grid(Expectativa ~ .)+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(fill = NA)))


plot(reg_raça)
plot(reg_mae)
plot(reg_nse)
