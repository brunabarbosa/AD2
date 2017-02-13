library(readr)
library(dplyr)
#set directory
setwd("~/AD2/Lab3-Parte1")

#import data
treino_classificacao <- read_csv("treino_classificacao.csv")
View(treino_classificacao)

install.packages("plotly")


library(plotly)
packageVersion('plotly')

#p <- plot_ly(x = treino_classificacao$MAT_TUR_ANO, type = 'histogram')
#p

#str(treino_classificacao$MAT_TUR_ANO)
#hist(treino_classificacao$MAT_TUR_ANO + treino_classificacao$EVADIU)


evadiram <- treino_classificacao[treino_classificacao$EVADIU == TRUE, ]
naoEvadiram <- treino_classificacao[treino_classificacao$EVADIU == FALSE, ]

#Alunos que evadiram por ano -- ano que mais houve evasao de alunos: 2011
hist(evadiram$MAT_TUR_ANO)



