setwd("~/Documents/AD2/Lab4-Parte1")

library(readr)
library(dplyr)

lab4_part1_train_data <- read_csv("lab4_part1_train_data.csv")
lab4_part1_test_data <- read_csv("lab4_part1_test_data.csv")

#lab4_part1_data <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data.csv")
#lab4_part1_data_v2 <- read_csv("~/Documents/AD2/Lab4-Parte1/lab4_part1_data_v2.csv")

library(recommenderlab)
library(reshape2)
library(ggplot2)

train <- lab4_part1_train_data
test <- lab4_part1_test_data

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

# view r in other possible ways
as(r, "list")     # A list
as(r, "matrix")   # A sparse matrix

# I can turn it into data-frame
head(as(r, "data.frame"))

# normalize the rating matrix
r_m <- normalize(r)
r_m
as(r_m, "list")

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")


r_b <- binarize(r, minRating=1)
as(r_b, "matrix")

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))
#rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5))
#rec=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard"))
#rec=Recommender(r[1:nrow(r)],method="POPULAR")

# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:nrow(r)], type="ratings")
recom
View(as(recom, "matrix"))


R<-as.matrix(test.dcast[,-c(1)])
r <- as(R[,-c(1)], "realRatingMatrix")

recom.test <- predict(rec, r[1:nrow(r)], type="ratings")
recom.test
View(as(recom.test, "matrix"))

predictedValues <- as(recom.test, "matrix")

