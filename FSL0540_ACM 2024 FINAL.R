###################################################################
#                                                                 #
#   30 de julho de 2023  (última v. 15 de maio de 2024)           #
#   FSL0540 Tópicos Avançados em Sociologia da Educação           #
#   Autor: Carolina Bueno Stefani e Guilherme Olímpio Fagundes    #
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
# Adicionados depois
install.packages("GDAtools")
install.packages("frequency")
install.packages("explor")
install.packages("haven")

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
# Adicionados
library(explor)
library(GDAtools)
library(frequency)
library(readxl)

pacman::p_load(rio, psych, descr, gridExtra, ggpubr, scales, 
               patchwork, effects, reshape2, foreign, sjPlot,
               pROC,ROCR,stargazer, caret,car, nnet, AER, 
               lmtest, gtsummary, DescTools)

# PASSO 2. Banco de dados para ACM --------------------------------

s21 <- read.csv("C:\\Users\\guilh\\Desktop\\SAEB2021_ALUNO.csv", header = T, sep = ";")

# Selecionar apenas questionários respondidos E de SP

s21_sp_int <- dplyr::filter(s21, IN_PREENCHIMENTO_QUESTIONARIO %in% "1")
s21_sp_int <- dplyr::filter(s21_sp_int, ID_UF %in% "35") # São Paulo


# Filtrar variáveis de interesse em cada SAEB

# SAEB 2021
s21_sp_int <- select(.data=s21_sp_int,
                     IN_PUBLICA, # Categoria administrativa
                     ID_SERIE,  
                     INSE_ALUNO, # NSE de Aluno (num)
                     NU_TIPO_NIVEL_INSE, # NSE de aluno (chr)
                     TX_RESP_Q01, # Sexo e gênero
                     TX_RESP_Q04, # Cor ou raça
                     TX_RESP_Q07, # Escolaridade da mãe
                   #  TX_RESP_Q08, # Escolaridade do pai
                     TX_RESP_Q17, # Você estudou majoritariamente...? 
                     TX_RESP_Q21) # expectativa


# Renomeações de variáveis
s21_sp_int <- rename(.data = s21_sp_int,
                     "admin"=IN_PUBLICA,
                     "serie"=ID_SERIE,
                     "NSEAluno"=INSE_ALUNO,
                     "NSETipo"=NU_TIPO_NIVEL_INSE,
                     "sexo"=TX_RESP_Q01,
                     "raça"=TX_RESP_Q04,
                     "mae"=TX_RESP_Q07,
                    # "pai"=TX_RESP_Q08,
                     "caEM"=TX_RESP_Q17,
                     "expectativa"=TX_RESP_Q21)

# Agora, filtramos os valores para excluir valores brancos e nulos

s21_sp_int <- dplyr::filter(s21_sp_int,
                            sexo %in% c("A", "B"),
                            serie %in% c("12", "13"),
                            expectativa %in% c("A", "B", "C", "D"), # "C" é Est+Tra
                            NSETipo %in% c("1", "2", "3", "4", "5", "6", "7", "8"),
                           # pai %in% c("A", "B", "C", "D", "E"),
                            mae %in% c("A", "B", "C", "D", "E"),
                            raça %in% c("A", "B", "C", "D", "E"),
                            caEM %in% c("A", "B", "C"))




# Recodificar variáveis, renomeando-as!

# SAEB21 Raça
table(s21_sp_int$raça)
prop.table(table(s21_sp_int$raça))*100
s21_sp_int$raça <- dplyr::recode(s21_sp_int$raça,
                                 "A"="Branca", 
                                 "B"="PPI", 
                                 "C"="PPI",  
                                 "D"="Branca",  
                                 "E"="PPI")
prop.table(table(s21_sp_int$raça))*100

# SAEB21 Escolaridade da mãe
table(s21_sp_int$mae)
prop.table(table(s21_sp_int$mae))*100
s21_sp_int$mae <- dplyr::recode(s21_sp_int$mae,
                                "A"="Fund-",
                                "B"="Fund-",
                                "C"="Fund+",
                                "D"="Medio+",  
                                "E"="Superior+") 

# SAEB21 Escolaridade do pai
#table(s21_sp_int$pai)
#prop.table(table(s21_sp_int$pai))*100
#s21_sp_int$pai <- dplyr::recode(s21_sp_int$pai,
 #                               "A"="Fund-", 
  #                              "B"="Fund-",  
   #                             "C"="Fund+",  
    #                            "D"="Medio+",  
     #                           "E"="Superior+") 

# SAEB21 Escolha imediata
s21_sp_int$expectativa <- dplyr::recode(s21_sp_int$expectativa,
                                        "A"="Estudar",
                                        "B"="Trabalhar",
                                        "C"="Est+Tra",
                                        "D"="NS")
prop.table(table(s21_sp_int$expectativa))*100


# SAEB21 Categoria administrativa estudada
s21_sp_int$caEM <- dplyr::recode(s21_sp_int$caEM, 
                                 "A"="Publica",
                                 "B"="Privada",
                                 "C"="Privada") # Juntamos
prop.table(table(s21_sp_int$caEM))*100

# SAEB21 NSE Categórica
table(s21_sp_int$NSETipo)
prop.table(table(s21_sp_int$NSETipo))*100
s21_sp_int$NSETipo <- dplyr::recode(s21_sp_int$NSETipo,
                                    "1"="I",  # Juntamos 1, 2 e 3
                                    "2"="I",
                                    "3"="I",
                                    "4"="II",
                                    "5"="III",
                                    "6"="IV",
                                    "7"="V", # Juntamos 7 e 8 
                                    "8"="V")

# Sexo -- identidade de gênero 
prop.table(table(s21_sp_int$sexo))*100
s21_sp_int$sexo <- dplyr::recode(s21_sp_int$sexo,
                                    "A"="Masculino",
                                    "B"="Feminino")
prop.table(table(s21_sp_int$sexo))*100

# Exportar para facilitar carregamentos
write.csv2(s21_sp_int, file="FSL0540_SAEB21_SP_ACM_FINAL.csv")

# De preferência, criar duplicatas dos bancos!
s21_sp_int_ACM <- select(.data= s21_sp_int, # sem pai
                         sexo, raça, mae, caEM, NSETipo, expectativa)

# PASSO 3. Análise de correspondência múltipla --------------------


# Primeira etapa: Visualização prévia

# Para definir ativa e suplementar
# mca(dados, quanti/quali.sup =)

res.ACM_s21_NSE <- MCA(s21_sp_int_ACM, graph = FALSE)

# EXPECTATIVA como suplementar
res.ACM_s21_NSE_SUP <- MCA(s21_sp_int_ACM,
                           quali.sup = 6,
                           graph = FALSE)

# Segunda etapa: Análise dos resultados estatísticos
res.ACM_s21$eig      # Resultados dos eixos
res.ACM_s21$var$eta2 # Resultados das colunas
res.ACM_s21$var$cos2 # Resultados das variáveis
res.ACM_s21$var$v.test
res.ACM_s21$var$coord
res.ACM_s21$var$contrib

res.ACM_s21_NSE$eig      # Resultados dos eixos
res.ACM_s21_NSE$var$eta2 # Resultados das colunas
res.ACM_s21_NSE$var$cos2 # Resultados das variáveis
res.ACM_s21_NSE$var$v.test
res.ACM_s21_NSE$var$coord
res.ACM_s21_NSE$var$contrib


# Terceira etapa: Construção de tabelas

# Criando tabelas no R
tab1_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$eig))
tab2_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$var$eta2))
tab3_s21 <- rownames_to_column(as.data.frame(res.ACM_s21$var$contrib))

tab1_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$eig))
tab2_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$var$eta2))
tab3_s21NSE <- rownames_to_column(as.data.frame(res.ACM_s21_NSE$var$contrib))


# Renomeando as colunas
names(tab1_s21)[1] <- "Dimensão"
names(tab1_s21)[2] <- "Eigenvalue"
names(tab1_s21)[3] <- "Variância (%)"
names(tab1_s21)[4] <- "Variância acumulada (%)"
names(tab2_s21)[1] <- "Colunas"
names(tab3_s21)[1] <- "Variáveis"

names(tab1_s21NSE)[1] <- "Dimensão"
names(tab1_s21NSE)[2] <- "Eigenvalue"
names(tab1_s21NSE)[3] <- "Variância (%)"
names(tab1_s21NSE)[4] <- "Variância acumulada (%)"
names(tab2_s21NSE)[1] <- "Colunas"
names(tab3_s21NSE)[1] <- "Variáveis"


# Exportar!
write.xlsx(tab1_s21, file="FSL0540_SAEB21_Tab1.xlsx")
write.xlsx(tab2_s21, file="FSL0540_SAEB21_Tab2.xlsx")
write.xlsx(tab3_s21, file="FSL0540_SAEB21_Tab3.xlsx")

write.xlsx(tab1_s21NSE, file="FSL0540_SAEB21NSE_Tab1.xlsx")
write.xlsx(tab2_s21NSE, file="FSL0540_SAEB21NSE_Tab2.xlsx")
write.xlsx(tab3_s21NSE, file="FSL0540_SAEB21NSE_Tab3.xlsx")


# Quarta etapa: Gráficos!


SAEB2021_ACM <- fviz_mca_var(res.ACM_s21,   #SAEB2021
                             col.var = "contrib", # Cor muda conforme contribuição da variáv.
                             repel=T, # Afasta os rótulos originais para + visual.
                             axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2021_ACM

SAEB2021_ACM_NSE <- fviz_mca_var(res.ACM_s21_NSE,   #SAEB2021
                                 col.var = "contrib", # Cor muda conforme contribuição da variáv.
                                 repel=T, # Afasta os rótulos originais para + visual.
                                 axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)

SAEB2021_ACM_NSE

# EXPECTATIVA OU NSE como sup
SAEB2021_ACM_NSE_SUP <- fviz_mca_var(res.ACM_s21_NSE_SUP,   #SAEB2021
                                 col.var = "contrib", # Cor muda conforme contribuição da variáv.
                                 repel=T, # Afasta os rótulos originais para + visual.
                                 axes=c(2,1))+  # Inverte os eixos
  theme_bw()+                    # Tema do gráfico (estética)
  geom_vline(xintercept = 0)+    # Desenha o eixo x
  geom_hline(yintercept = 0)+    # Desenha o eixo y
  scale_color_gradient2(low = "white", # Muda a legenda e as cores das variáveis
                        mid = "blue", 
                        high = "red",
                        midpoint = 4)
SAEB2021_ACM_NSE_SUP


## Script curso

explor(res.ACM_s21)
explor(res.ACM_s19)
explor(res.ACM_s21_NSE_SUP)
