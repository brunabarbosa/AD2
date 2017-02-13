library(readr)
library(dplyr)
library(ggplot2)
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

#Gere uma visualização da distribuição das classes (número de instâncias de cada classe nos dados);
#fazer grafico de barras 
#alunos que evadiram / nao evadiram
#Tambem pode fazer over time

# Simple Bar Plot
counts <- table(treino_classificacao$EVADIU)
barplot(counts, main="Dados de treino",
        xlab="Evasão") 


#Facil de confirmar o que o histograma mostrou, mais pessoas evadiram em 2011
counts <- table(treino_classificacao$EVADIU, treino_classificacao$MAT_TUR_ANO)
barplot(counts, main="Dados de treino",
        xlab="Numero de alunos que evadiram por ano", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


