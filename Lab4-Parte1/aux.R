setwd("~/Documentos/AD2/Lab4-Parte1")

library(readr)
library(dplyr)
library(recommenderlab)
library(reshape2)
library(ggplot2)
library(caret)

set.seed(107)

lab4_part1_data <- read_csv("lab4_part1_data.csv")
lab4_part1_data_v2 <- read_csv("lab4_part1_data_v2.csv")

dt = sort(sample(nrow(lab4_part1_data), nrow(lab4_part1_data)*.7))
train<-lab4_part1_data[dt,]
test<-lab4_part1_data[-dt,]

removeColumns <- c('MAT_NOVA_MATRICULA', 'ALU_ANO_INGRESSO', 'ALU_PERIODO_INGRESSO')

# Convert it as a matrix
R<-as.matrix( train[which(!(colnames(train)%in%removeColumns))])


# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r

rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))

print(rec)
names(getModel(rec))
getModel(rec)$nn

#test as matrix
R<-as.matrix( test[which(!(colnames(test)%in%removeColumns))])
r <- as(R, "realRatingMatrix")

recom.test <- predict(rec, r[1:nrow(r)], type="ratings")
recom.test
#View(as(recom.test, "matrix"))

##Filtrar
predictedValues <- as(recom.test, "matrix")
pred.values.df <- cbind(MAT_NOVA_MATRICULA=test$MAT_NOVA_MATRICULA,as.data.frame(predictedValues))

pred.values.df.melt <- melt(pred.values.df,id.vars = "MAT_NOVA_MATRICULA", variable.name = "NOME_DISCIPLINA") %>%
  mutate(MAT_NOVA_MATRICULA = as.character(MAT_NOVA_MATRICULA),
         NOME_DISCIPLINA = as.character(NOME_DISCIPLINA)) %>%
  rename(mediaPred = value)

test.4.periodo <- lab4_part1_data_v2 %>% filter(periodo_relativo == 4)

##result para merge entre os valores da predicao e o test
result.4.periodo <- inner_join(pred.values.df.melt,lab4_part1_data_v2)


RMSE(true, predicted)


test.melt <- melt(test,id.vars = c("MAT_NOVA_MATRICULA","ALU_ANO_INGRESSO", "ALU_PERIODO_INGRESSO") , variable.name = "NOME_DISCIPLINA") %>%
  mutate(MAT_NOVA_MATRICULA = as.character(MAT_NOVA_MATRICULA),
         NOME_DISCIPLINA = as.character(NOME_DISCIPLINA)) %>%
  rename(mediaReal = value)

result <- inner_join(pred.values.df.melt, test.melt)







