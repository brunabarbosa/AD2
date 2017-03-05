library(ISLR)
library(readr)
library(caret)
library(dplyr)
library(reshape2)
library(pROC)

setwd("~/AD2/Lab3-Parte2")
treino_classificacao_v2 <- read_csv("treino_classificacao_v2.csv")

##reshape table
classificacao.clean <- treino_classificacao_v2 %>%
  filter(!is.na(MAT_MEDIA_FINAL))

classificacao.clean$CREDITOS <- 4

classificacao.cra <- classificacao.clean %>%
  group_by(MAT_ALU_MATRICULA, EVADIU) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))


classificacao.model.input <- classificacao.clean %>%
  group_by(MAT_ALU_MATRICULA,disciplina)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_ALU_MATRICULA,disciplina,MAT_MEDIA_FINAL) %>% 
  mutate(disciplina = as.factor(gsub(" ",".",disciplina))) %>%
  dcast(MAT_ALU_MATRICULA ~ disciplina, mean) %>%
  merge(classificacao.cra)


split <- createDataPartition(y=classificacao.model.input$EVADIU, p = 0.75, list = FALSE)
train <- classificacao.model.input[split,]
test <- classificacao.model.input[-split,]

train.clean <- train %>%
  filter(!is.na(Álgebra.Vetorial.e.Geometria.Analítica)) %>%
  filter(!is.na(Cálculo.Diferencial.e.Integral.I)) %>%
  filter(!is.na(Introdução.à.Computação)) %>%
  filter(!is.na(Laboratório.de.Programação.I)) %>%
  filter(!is.na(Programação.I)) %>%
  filter(!is.na(Leitura.e.Produção.de.Textos)) 

test.clean <- test %>%
  filter(!is.na(Álgebra.Vetorial.e.Geometria.Analítica)) %>%
  filter(!is.na(Cálculo.Diferencial.e.Integral.I)) %>%
  filter(!is.na(Introdução.à.Computação)) %>%
  filter(!is.na(Laboratório.de.Programação.I)) %>%
  filter(!is.na(Programação.I)) %>%
  filter(!is.na(Leitura.e.Produção.de.Textos))

train.clean$EVADIU <- as.factor(train.clean$EVADIU)
train.clean$MAT_ALU_MATRICULA <- as.factor(train.clean$MAT_ALU_MATRICULA)

is.factor(train.clean$EVADIU)
is.factor(train.clean$MAT_ALU_MATRICULA)

model <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
             + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Leitura.e.Produção.de.Textos
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica,
             family=binomial, data=train)




fitted.results <- predict(model,newdata=test.clean,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))
roc.curve(test.clean$EVADIU, fitted.results)
