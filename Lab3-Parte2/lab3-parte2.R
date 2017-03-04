library(ISLR)
library(readr)
library(caret)
library(dplyr)

setwd("~/AD2/Lab3-Parte2")
treino_classificacao_v2 <- read_csv("treino_classificacao_v2.csv")



## 1.Separe os dados em treino e teste;



split <- createDataPartition(y=treino_classificacao_v2$EVADIU, p = 0.9, list = FALSE)
train <- treino_classificacao_v2[split,]
test <- treino_classificacao_v2[-split,]



## 2.Use como atributos as médias das disciplinas mais o atributo que você criou na parte 1 (fique a vontade para criar mais atributos);



novoatributo <- treino_classificacao_v2 %>%
  group_by(MAT_ALU_MATRICULA)

novoatributo[novoatributo$MAT_TUR_ANO <= 2011, "ENEM"] <- 0
novoatributo[novoatributo$MAT_TUR_ANO > 2011, "ENEM"] <- 1
novoatributo$CREDITOS <- 4

str(novoatributo)
summary(novoatributo)


novoatributo <- novoatributo %>%
                arrange(MAT_ALU_MATRICULA)

classificacao.clean <- novoatributo %>%
  filter(!is.na(MAT_MEDIA_FINAL))

summary(classificacao.clean)
View(classificacao.clean)

##calcular o cra do primeiro periodo
classificacao.cra <- classificacao.clean %>%
  group_by(MAT_ALU_MATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(CRA = sum(cra.contrib)/sum(CREDITOS))

classificacao =merge(novoatributo, classificacao.cra, by="MAT_ALU_MATRICULA")

## treino e teste com novo atributo
split <- createDataPartition(y=classificacao$EVADIU, p = 0.9, list = FALSE)
train <- classificacao[split,]
test <- classificacao[-split,]


##balanceamento
#install packages
install.packages("ROSE")
library(ROSE)


prop.table(table(train$EVADIU))

##modelo com dados nao balanceados
library(rpart)
treeimb <- rpart(EVADIU ~ MAT_MEDIA_FINAL, data = train)
pred.treeimb <- predict(treeimb, newdata = train)

accuracy.meas(test$EVADIU, pred.treeimb[,2])

data(hacide)

pred.treeimb



##############

install.packages("ROSE")
library(ROSE)

data(hacide)
str(hacide.train)

table(hacide.train$cls)
prop.table(table(hacide.train$cls))

library(rpart)
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)









