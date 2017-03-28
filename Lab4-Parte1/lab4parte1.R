setwd("~/Documents/AD2/Lab4-Parte1")

library(readr)
library(dplyr)

train <- read_csv("lab4_part1_train_data.csv")
test <- read_csv("lab4_part1_test_data.csv")

#lab4_part1_data <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data.csv")
#lab4_part1_data_v2 <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data_v2.csv")

library(recommenderlab)
library(reshape2)
library(ggplot2)

# Using acast to convert above data as follows:
#       d1  d2   d3   d4
# a1    3   4    2    5
# a2    1   6    5
# a3    4   4    2    5
train.dcast <- train %>%
  group_by(MAT_NOVA_MATRICULA,NOME_DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_NOVA_MATRICULA,NOME_DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(NOME_DISCIPLINA = as.factor(gsub(" ",".",NOME_DISCIPLINA))) %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, mean)

test.dcast <- test %>%
  group_by(MAT_NOVA_MATRICULA,NOME_DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_NOVA_MATRICULA,NOME_DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(NOME_DISCIPLINA = as.factor(gsub(" ",".",NOME_DISCIPLINA))) %>%
  dcast(MAT_NOVA_MATRICULA ~ NOME_DISCIPLINA, mean)

# Convert it as a matrix
R<-as.matrix(train.dcast[,-c(1)])

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R[,-c(1)], "realRatingMatrix")
r


# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))

# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

#recom <- predict(rec, r[1:nrow(r)], type="ratings")
#recom
#View(as(recom, "matrix"))


R <-as.matrix(test.dcast[,-c(1)])
r <- as(R[,-c(1)], "realRatingMatrix")

recom.test <- predict(rec, r[1:nrow(r)], type="ratings")
recom.test
View(as(recom.test, "matrix"))

predictedValues <- as(recom.test, "matrix")
pred.values.df <- cbind(MAT_NOVA_MATRICULA=test.dcast$MAT_NOVA_MATRICULA,as.data.frame(predictedValues))

pred.values.df.melt <- melt(pred.values.df,id.vars = "MAT_NOVA_MATRICULA", variable.name = "NOME_DISCIPLINA") %>%
  mutate(MAT_NOVA_MATRICULA = as.character(MAT_NOVA_MATRICULA),
         NOME_DISCIPLINA = as.character(NOME_DISCIPLINA))

test.4.periodo <- test %>% filter(periodo_relativo == 4)


result.4.periodo <- inner_join(pred.values.df.melt,test)


result.4.periodo.merge <- merge(pred.values.df.melt, test)
 


