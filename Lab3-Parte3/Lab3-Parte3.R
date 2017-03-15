library(readr)
library(reshape2)
library(ISLR)
library(caret)
library(dplyr)
library(rpart)
library(ROSE)
library(pROC)



setwd("~/Documents/AD2/Lab3-Parte3")

lab3_kaggle_classificacao_treino <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_treino.csv")
lab3_kaggle_classificacao_teste <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_teste.csv")
lab3_kaggle_classificacao_sample_submissao <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_sample_submissao.csv")

kaggle_treino <- lab3_kaggle_classificacao_treino 
kaggle_teste <- lab3_kaggle_classificacao_teste

kaggle_teste$EVADIU <- 0

#reshape train
kaggle_treino_unique_line <- kaggle_treino %>%
  group_by(MAT_ALU_MATRICULA, DISCIPLINA, MAT_TUR_ANO, MAT_TUR_PERIODO)   %>% 
  unique() 

kaggle_treino_reshaped <- kaggle_treino  %>% 
  select(MAT_ALU_MATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(MAT_ALU_MATRICULA ~ DISCIPLINA, value.var = "MAT_MEDIA_FINAL")

#reshape test
kaggle_teste_unique_line <- kaggle_teste  %>%
  group_by(MAT_ALU_MATRICULA, DISCIPLINA, MAT_TUR_ANO, MAT_TUR_PERIODO)   %>% 
  unique() 

kaggle_teste_reshaped <- kaggle_teste  %>% 
  select(MAT_ALU_MATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(MAT_ALU_MATRICULA ~ DISCIPLINA, value.var = "MAT_MEDIA_FINAL")


treino <- merge(kaggle_treino_unique_line, kaggle_treino_reshaped)
teste <- merge(kaggle_teste_unique_line, kaggle_teste_reshaped)
#Ta faltando uma disciplina nos testes
teste$Laboratório.de.Programação.I <- NA

# Combine train and test datasets, name all_data 
all_data <- rbind(treino,teste)
 
all_data$cra <- (all_data$Cálculo.Diferencial.e.Integral.I * 4
                  + all_data$Leitura.e.Produção.de.Textos * 4
                  + all_data$Introdução.à.Computação * 4
                  + all_data$Laboratório.de.Programação.I  * 4
                  + all_data$Álgebra.Vetorial.e.Geometria.Analítica * 4
                  + all_data$Programação.I * 4
 ) / 24




#retira cra's vazios
all_data[is.na(all_data$cra),]$cra <- 0

all_data$missing <- rowSums(is.na(all_data))

sete <- all_data[all_data$missing == 7, ]

#input treino






#cinco notas NA's, colocar zero nas disciplinas
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I), ]$
  Cálculo.Diferencial.e.Integral.I <- 0
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica),]$
  Álgebra.Vetorial.e.Geometria.Analítica <- 0
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Introdução.à.Computação), ]$
  Introdução.à.Computação <- 0
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Programação.I), ]$
  Programação.I <- 0
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Laboratório.de.Programação.I), ]$
  Laboratório.de.Programação.I <- 0
all_data_reshaped[all_data_reshaped$missing == 5 & is.na(all_data_reshaped$Leitura.e.Produção.de.Textos), ]$
  Leitura.e.Produção.de.Textos <- 0


rwmns = rowMeans(all_data_reshaped[,2:7],na.rm=TRUE)
all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica)] = 
  rwmns[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica)]

all_data_reshaped$Cálculo.Diferencial.e.Integral.I[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I)] = 
  rwmns[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I)]

all_data_reshaped$Introdução.à.Computação[is.na(all_data_reshaped$Introdução.à.Computação)] = 
  rwmns[is.na(all_data_reshaped$Introdução.à.Computação)]

all_data_reshaped$Programação.I[is.na(all_data_reshaped$Programação.I)] = 
  rwmns[is.na(all_data_reshaped$Programação.I)]

all_data_reshaped$Laboratório.de.Programação.I[is.na(all_data_reshaped$Laboratório.de.Programação.I)] = 
  rwmns[is.na(all_data_reshaped$Laboratório.de.Programação.I)]

all_data_reshaped$Leitura.e.Produção.de.Textos[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos)] = 
  rwmns[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos)]

summary(all_data_reshaped)


train <- all_data_reshaped[1:1302,]
test <- all_data_reshaped[1303:1370,]

# set seed for reproducibility
set.seed(111)
table(train$EVADIU) 

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#ROSE
set.seed(111)
fit.down <- rpart(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                  + Introdução.à.Computação
                  + Laboratório.de.Programação.I
                  + Leitura.e.Produção.de.Textos
                  + Programação.I
                  + Álgebra.Vetorial.e.Geometria.Analítica
                  + missing,
                  data=train,
                  method="class")

fancyRpartPlot(fit.down)

fitted.results.rose <- predict(model.rose,newdata=all_data_reshaped,type='response')
fitted.results.rose <- ifelse(fitted.results.rose > 0.5,1,0)
all_data_reshaped$EVADIU <- fitted.results.rose
misClasificError <- mean(fitted.results.rose != test$EVADIU)
print(paste('Accuracy',1-misClasificError))
accuracy.meas(all_data_reshaped$EVADIU, fitted.results.rose)



 
 
 
 
