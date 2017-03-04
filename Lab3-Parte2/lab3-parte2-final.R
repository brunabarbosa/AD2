library(ISLR)
library(readr)
library(caret)
library(dplyr)
library(reshape2)



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

#oversampling
set.seed(9560)
up_train <- upSample(x = train[, -ncol(train)],
                     y = as.factor(train$EVADIU))                         
table(up_train$EVADIU) 


##modelo sem balanceamento




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
  
##regressao logistica##
model <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
             + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Leitura.e.Produção.de.Textos
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica
             ,family=binomial, data=train.clean)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

# MEASURING THE PREDICTIVE ABILITY OF THE MODEL

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata=test.clean,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results

misClasificError <- mean(fitted.results != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))


library(pROC)

auc(test.clean$EVADIU, fitted.results)

##plot
roc.curve(test.clean$EVADIU, fitted.results)
##regressao logistica##

#########################################################################
##dados balanceados

install.packages("ROSE")
library(ROSE)

hacide.rose <- ROSE(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                  + Introdução.à.Computação
                  + Laboratório.de.Programação.I
                  + Leitura.e.Produção.de.Textos
                  + Programação.I
                  + Álgebra.Vetorial.e.Geometria.Analítica
                  , data = train.clean, seed = 123)$data

table(data.rose$cls)

is.factor(train.clean$EVADIU)

data_balanced_over <- ovun.sample(EVADIU ~ ., data = train.clean, method = "over",N = 1960)$data
