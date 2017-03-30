setwd("~/Documents/AD2/Lab4-Parte1")

library(readr)
library(dplyr)

train <- read_csv("lab4_part1_train_data.csv")
test <- read_csv("lab4_part1_test_data.csv")


library(recommenderlab)
library(reshape2)
library(ggplot2)

#Dataframe sem as notas das displinas que os alunos cursaram no quarto periodo
test.sem.4.P <- test %>% mutate(MAT_MEDIA_FINAL = ifelse(periodo_relativo == 4, NA, MAT_MEDIA_FINAL)) 


remove.inf <- function(x) ifelse(x==-Inf,NA,x)

train.dcast <- train %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, fun.aggregate = max, value.var = "MAT_MEDIA_FINAL") %>%
  mutate_each(funs(remove.inf),-MAT_NOVA_MATRICULA) %>%
  select(-starts_with('NA'))

test.sem.4.p.dcast <- test.sem.4.P %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, fun.aggregate = max, value.var = "MAT_MEDIA_FINAL") %>%
  mutate_each(funs(remove.inf),-MAT_NOVA_MATRICULA) %>%
  select(-starts_with('NA'))

str(train.dcast)
str(test.sem.4.p.dcast)

##create model
R<-as.matrix(train.dcast[,-c(1)])
r <- as(R, "realRatingMatrix")

rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))

#predict
R.test <-as.matrix(test.sem.4.p.dcast[,-c(1)])
r.test <- as(R.test,"realRatingMatrix")

recom.test <- predict(rec, r.test, type="ratings")
recom.test


predictedValues <- as(recom.test, "matrix")
pred.values.df <- cbind(MAT_NOVA_MATRICULA=test.sem.4.p.dcast$MAT_NOVA_MATRICULA,as.data.frame(predictedValues))

pred.values.df.melt <- melt(pred.values.df,id.vars = "MAT_NOVA_MATRICULA", variable.name = "NOME_DISCIPLINA") %>%
  mutate(MAT_NOVA_MATRICULA = as.character(MAT_NOVA_MATRICULA),
         NOME_DISCIPLINA = as.character(NOME_DISCIPLINA)) %>%
  rename(mediaPred = value)

test.4.periodo <- test %>% filter(periodo_relativo == 4)

result.4.periodo <- inner_join(pred.values,test.4.periodo)


RMSE(result.4.periodo$mediaPred,result.4.periodo$MAT_MEDIA_FINAL) 

#Alterar os metodos de predição

length(result.4.periodo$value)
