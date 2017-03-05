library(ISLR)
library(readr)
library(caret)
library(dplyr)
library(reshape2)
library(pROC)
library(ROSE)
setwd("~/AD2/Lab3-Parte2")
treino_classificacao_v2 <- read_csv("treino_classificacao_v2.csv")


treino_classificacao_v2 <- treino_classificacao_v2 %>%
  arrange(MAT_ALU_MATRICULA)

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

##Split dos dados

split <- createDataPartition(y=classificacao.model.input$EVADIU, p = 0.75, list = FALSE)
train <- classificacao.model.input[split,]
test <- classificacao.model.input[-split,]
table(train$EVADIU) 

##to factor
train$EVADIU <- as.factor(train$EVADIU)
train$MAT_ALU_MATRICULA <- as.factor(train$MAT_ALU_MATRICULA)

is.factor(train$EVADIU)
is.factor(train$MAT_ALU_MATRICULA)

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



#oversampling
set.seed(9560)
up_train <- upSample(x = train.clean[, -ncol(train.clean)],
                     y = train.clean$EVADIU)                         
table(up_train$EVADIU) 

#undersampling
set.seed(9560)
down_train <- downSample(x = train.clean[, -ncol(train.clean)],
                         y = train.clean$EVADIU)  
table(down_train$Class)   

#ROSE
library(ROSE)
set.seed(9560)
rose_train <- ROSE(EVADIU ~ . , data  = train.clean)$data                         
table(rose_train$EVADIU) 

##regressao logistica##
##modelo sem balanceamento
model <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
             + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Leitura.e.Produção.de.Textos
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica
             ,family=binomial, data=train.clean)
summary(model)

##modelo com balanceamento
model.up <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                + Introdução.à.Computação
                + Laboratório.de.Programação.I
                + Leitura.e.Produção.de.Textos
                + Programação.I
                + Álgebra.Vetorial.e.Geometria.Analítica
                ,family=binomial, data=up_train)
summary(model.up)

model.down <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                  + Introdução.à.Computação
                  + Laboratório.de.Programação.I
                  + Leitura.e.Produção.de.Textos
                  + Programação.I
                  + Álgebra.Vetorial.e.Geometria.Analítica
                  ,family=binomial, data=down_train)

model.rose <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                  + Introdução.à.Computação
                  + Laboratório.de.Programação.I
                  + Leitura.e.Produção.de.Textos
                  + Programação.I
                  + Álgebra.Vetorial.e.Geometria.Analítica
                  ,family=binomial, data=rose_train)

# Analysis of deviance
anova(model,test="Chisq")

fitted.results <- predict(model,newdata=test.clean,type='response')
fitted.results
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results
misClasificError <- mean(fitted.results != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))
roc.curve(test.clean$EVADIU, fitted.results)

#AUC oversampling
fitted.results.up <- predict(model.up,newdata=test.clean,type='response')
fitted.results.up <- ifelse(fitted.results.up > 0.5,1,0)
fitted.results.up
misClasificError <- mean(fitted.results.up != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))
roc.curve(test.clean$EVADIU, fitted.results.up)

#AUC undersampling
fitted.results.under <- predict(model.down,newdata=test.clean,type='response')
fitted.results.under <- ifelse(fitted.results.under > 0.5,1,0)
fitted.results.under
misClasificError <- mean(fitted.results.under != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))
roc.curve(test.clean$EVADIU, fitted.results.under)

#AUC ROSE
fitted.results.rose <- predict(model.rose,newdata=test.clean,type='response')
fitted.results.rose <- ifelse(fitted.results.rose > 0.5,1,0)
fitted.results.rose
misClasificError <- mean(fitted.results.rose != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))
roc.curve(test.clean$EVADIU, fitted.results.rose)

auc(test.clean$EVADIU, fitted.results)
auc(test.clean$EVADIU, fitted.results.up)
auc(test.clean$EVADIU, fitted.results.under)
auc(test.clean$EVADIU, fitted.results.rose)
