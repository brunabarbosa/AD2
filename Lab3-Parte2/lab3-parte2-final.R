library(ISLR)
library(readr)
library(caret)
library(dplyr)
library(reshape2)
library(pROC)
library(ROSE)
library(caret)

setwd("~/AD2/Lab3-Parte2")
treino_classificacao_v2 <- read_csv("treino_classificacao_v2.csv")


treino_classificacao_v2 <- treino_classificacao_v2 %>%
  arrange(MAT_ALU_MATRICULA)

##reshape table
classificacao.clean <- treino_classificacao_v2 %>%
  filter(!is.na(MAT_MEDIA_FINAL))

classificacao.clean$CREDITOS <- 4
classificacao.clean[classificacao.clean$MAT_TUR_ANO < 2011, "ENEM"] <- FALSE
classificacao.clean[classificacao.clean$MAT_TUR_ANO >= 2011, "ENEM"] <- TRUE

classificacao.cra <- classificacao.clean %>%
  group_by(MAT_ALU_MATRICULA, EVADIU, ENEM) %>%
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

split <- createDataPartition(y=classificacao.model.input$EVADIU, p = 0.90, list = FALSE)
train <- classificacao.model.input[split,]
test <- classificacao.model.input[-split,]
table(train$EVADIU) 

##to factor
train$EVADIU <- as.factor(train$EVADIU)
train$MAT_ALU_MATRICULA <- as.factor(train$MAT_ALU_MATRICULA)
train$ENEM <- as.logical(train$ENEM)
is.factor(train$EVADIU)
is.factor(train$MAT_ALU_MATRICULA)

#train$CalLab <- ((train$Cálculo.Diferencial.e.Integral.I + train$Laboratório.de.Programação.I)/2.0)

train.clean <- train %>%
  filter(!is.na(Álgebra.Vetorial.e.Geometria.Analítica)) %>%
  filter(!is.na(Cálculo.Diferencial.e.Integral.I)) %>%
  filter(!is.na(Introdução.à.Computação)) %>%
  filter(!is.na(Laboratório.de.Programação.I)) %>%
  filter(!is.na(Programação.I)) %>%
  filter(!is.na(Leitura.e.Produção.de.Textos)) 

#train.clean$CalLab <- ((train.clean$Cálculo.Diferencial.e.Integral.I + train.clean$Laboratório.de.Programação.I)/2.0)
#train.clean$CalLab <- as.numeric(train.clean$CalLab)

test.clean <- test %>%
  filter(!is.na(Álgebra.Vetorial.e.Geometria.Analítica)) %>%
  filter(!is.na(Cálculo.Diferencial.e.Integral.I)) %>%
  filter(!is.na(Introdução.à.Computação)) %>%
  filter(!is.na(Laboratório.de.Programação.I)) %>%
  filter(!is.na(Programação.I)) %>%
  filter(!is.na(Leitura.e.Produção.de.Textos))

#test.clean$CalLab <- ((test.clean$Cálculo.Diferencial.e.Integral.I + test.clean$Laboratório.de.Programação.I)/2.0)
#test.clean$CalLab <- as.numeric(test.clean$CalLab)

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

model.new <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                 + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica
             ,family=binomial, data=train.clean)
summary(model.new)

model.attr <- glm(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                  + Introdução.à.Computação
                  + Laboratório.de.Programação.I
                  + Leitura.e.Produção.de.Textos
                  + Programação.I
                  + Álgebra.Vetorial.e.Geometria.Analítica + ENEM
                  ,family=binomial, data=train.clean)

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
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError))

auc(test.clean$EVADIU, fitted.results)


fitted.results.attr <- predict(model.attr,newdata=test.clean,type='response')
fitted.results.attr <- ifelse(fitted.results.attr > 0.5,1,0)
misClasificError.attr <- mean(fitted.results.attr != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError.attr))

auc(test.clean$EVADIU, fitted.results.attr)

#AUC oversampling
fitted.results.up <- predict(model.up,newdata=test.clean,type='response')
fitted.results.up <- ifelse(fitted.results.up > 0.5,1,0)
misClasificError.up <- mean(fitted.results.up != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError.up))

auc(test.clean$EVADIU, fitted.results.up)

#AUC undersampling
fitted.results.under <- predict(model.down,newdata=test.clean,type='response')
fitted.results.under <- ifelse(fitted.results.under > 0.5,1,0)
misClasificError.under <- mean(fitted.results.under != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError.under))

auc(test.clean$EVADIU, fitted.results.under)

#AUC ROSE
fitted.results.rose <- predict(model.rose,newdata=test.clean,type='response')
fitted.results.rose <- ifelse(fitted.results.rose > 0.5,1,0)
misClasificError.rose <- mean(fitted.results.rose != test.clean$EVADIU)
print(paste('Accuracy',1-misClasificError.rose))

auc(test.clean$EVADIU, fitted.results.rose)


roc.curve(test.clean$EVADIU, fitted.results)
roc.curve(test.clean$EVADIU, fitted.results.attr)
roc.curve(test.clean$EVADIU, fitted.results.up)
roc.curve(test.clean$EVADIU, fitted.results.under)
roc.curve(test.clean$EVADIU, fitted.results.rose)

##arvore de decisao
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(EVADIU ~ Cálculo.Diferencial.e.Integral.I
             + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Leitura.e.Produção.de.Textos
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica,
             data=train.clean,
             method="class")

##overfiting
fancyRpartPlot(fit)

Prediction <- predict(fit, test.clean, type = "class")
accuracy.meas(test.clean$EVADIU,Prediction)
roc.curve(test.clean$EVADIU,Prediction, plotit = F)

fit2 <- rpart(EVADIU ~ Cálculo.Diferencial.e.Integral.I
             + Introdução.à.Computação
             + Laboratório.de.Programação.I
             + Leitura.e.Produção.de.Textos
             + Programação.I
             + Álgebra.Vetorial.e.Geometria.Analítica,
             data=train.clean,
             method="class",
             control=rpart.control(minsplit=20, cp=0.02))

Prediction.fit2 <- predict(fit, test.clean, type = "class")
accuracy.meas(test.clean$EVADIU,Prediction.fit2)
roc.curve(test.clean$EVADIU,Prediction.fit2, plotit = F)

#caret
#k-folds + cross-validation

library(caret)


caret_model <- train(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                     + Introdução.à.Computação
                     + Laboratório.de.Programação.I
                     + Leitura.e.Produção.de.Textos
                     + Programação.I
                     + Álgebra.Vetorial.e.Geometria.Analítica, 
                     data=train.clean, method="glm", family="binomial")

caret.probs <- predict(caret_model, newdata=test.clean, type="prob")
caret.probs
caret.results <- predict(caret_model,newdata=test.clean)
caret.results
confusionMatrix(data=caret.results, reference=test.clean$EVADIU)

accuracy <- table(caret.results, test.clean[,"EVADIU"])
sum(diag(accuracy))/sum(accuracy)

roc.curve(test.clean$EVADIU,caret.results, plotit = F)
###############
set.seed(9560)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

glmfit <- train(EVADIU ~ Cálculo.Diferencial.e.Integral.I
                + Introdução.à.Computação
                + Laboratório.de.Programação.I
                + Leitura.e.Produção.de.Textos
                + Programação.I
                + Álgebra.Vetorial.e.Geometria.Analítica,
               data = train.clean,
               method='glm',
               trControl = fitControl,
               preProc=c('scale', 'center'))
glmfit

coef(glmfit$finalModel)
glmfit.pred <- predict(glmfit, test.clean)
roc.curve(test.clean$EVADIU,glmfit.pred, plotit = F)


##Lasso and Ridge

set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

ridge <- train(EVADIU ~ Cálculo.Diferencial.e.Integral.I
               + Introdução.à.Computação
               + Laboratório.de.Programação.I
               + Leitura.e.Produção.de.Textos
               + Programação.I
               + Álgebra.Vetorial.e.Geometria.Analítica, 
               data = train.clean,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridge
predict(ridge$finalModel, type='coef', mode='norm')$coefficients[ncol(train.clean),]
ridge.pred <- predict(ridge, test)
