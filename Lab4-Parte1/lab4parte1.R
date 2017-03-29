setwd("~/Documentos/AD2/Lab4-Parte1")

library(readr)
library(dplyr)

train <- read_csv("lab4_part1_train_data.csv")
test <- read_csv("lab4_part1_test_data.csv")

#lab4_part1_data <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data.csv")
#lab4_part1_data_v2 <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data_v2.csv")

library(recommenderlab)
library(reshape2)
library(ggplot2)


test.sem.4.P <- test %>% mutate(MAT_MEDIA_FINAL = ifelse(periodo_relativo == 4, NA, MAT_MEDIA_FINAL)) 

train.dcast <- train %>%
  group_by(MAT_NOVA_MATRICULA,NOME_DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_NOVA_MATRICULA,NOME_DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(NOME_DISCIPLINA = as.factor(gsub(" ",".",NOME_DISCIPLINA))) %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, mean)

test.sem.4.p.dcast <- test.sem.4.P %>%
  group_by(MAT_NOVA_MATRICULA,NOME_DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_NOVA_MATRICULA,NOME_DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(NOME_DISCIPLINA = as.factor(gsub(" ",".",NOME_DISCIPLINA))) %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, mean)

str(train.dcast)
str(test.sem.4.p.dcast)

##create model
R<-as.matrix(train.dcast[,-c(1)])
r <- as(R, "realRatingMatrix")

rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))

#predict
R.test <-as.matrix(select(test.sem.4.p.dcast, -starts_with('NA'), -MAT_NOVA_MATRICULA))
r.test <- as(R.test,"realRatingMatrix")

recom.test <- predict(rec, r.test, type="ratings")
recom.test


predictedValues <- as(recom.test, "matrix")

pred.values.df <- cbind(MAT_NOVA_MATRICULA=test.dcast$MAT_NOVA_MATRICULA,as.data.frame(predictedValues))

pred.values.df.melt <- melt(pred.values.df,id.vars = "MAT_NOVA_MATRICULA", variable.name = "NOME_DISCIPLINA") %>%
  mutate(MAT_NOVA_MATRICULA = as.character(MAT_NOVA_MATRICULA),
         NOME_DISCIPLINA = as.character(NOME_DISCIPLINA))

test.4.periodo <- test %>% filter(periodo_relativo == 4)


result.4.periodo <- inner_join(pred.values.df.melt,test)


result.4.periodo.merge <- merge(pred.values.df.melt, test)
 


