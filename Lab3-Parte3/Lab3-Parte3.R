library(readr)
library(reshape2)
library(ISLR)
library(caret)
library(dplyr)
library(rpart)
library(randomForest)

setwd("~/Documents/AD2/Lab3-Parte3")

lab3_kaggle_classificacao_treino <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_treino.csv")
lab3_kaggle_classificacao_teste <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_teste.csv")
lab3_kaggle_classificacao_sample_submissao <- read_csv("~/Documents/AD2/Lab3-Parte3/lab3_kaggle_classificacao_sample_submissao.csv")

kaggle_treino <- lab3_kaggle_classificacao_treino 
kaggle_teste <- lab3_kaggle_classificacao_teste

kaggle_teste$EVADIU <- 0

# Combine train and test datasets, name all_data 
all_data <- rbind(kaggle_treino,kaggle_teste)

all_data_cra <- all_data %>%
  group_by(MAT_ALU_MATRICULA, EVADIU) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*4) %>%
  summarise(cra = sum(cra.contrib)/24)


 all_data_reshaped <- all_data %>%
  group_by(MAT_ALU_MATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(MAT_ALU_MATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(MAT_ALU_MATRICULA ~ DISCIPLINA, mean) %>%
  merge(all_data_cra)

 summary(all_data_reshaped)

 # Fill missing values in lpt
 predicted_lpt <- rpart( Leitura.e.Produção.de.Textos ~ Álgebra.Vetorial.e.Geometria.Analítica 
                         + Cálculo.Diferencial.e.Integral.I 
                         + Introdução.à.Computação 
                         + Programação.I 
                         + Laboratório.de.Programação.I, 
                         data = all_data_reshaped[!is.na(all_data_reshaped$Leitura.e.Produção.de.Textos),], method = "anova")
 
 
 all_data_reshaped$Leitura.e.Produção.de.Textos[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos)] <- 
   predict(predicted_lpt, all_data_reshaped[is.na(all_data_reshaped$Leitura.e.Produção.de.Textos),])
 
summary(all_data_reshaped)

# Fill missing values in cal1
predicted_calc1 <- rpart( Cálculo.Diferencial.e.Integral.I ~ Álgebra.Vetorial.e.Geometria.Analítica 
                        + Leitura.e.Produção.de.Textos
                        + Introdução.à.Computação 
                        + Programação.I 
                        + Laboratório.de.Programação.I, 
                        data = all_data_reshaped[!is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I),], method = "anova")


all_data_reshaped$Cálculo.Diferencial.e.Integral.I[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Cálculo.Diferencial.e.Integral.I),])

summary(all_data_reshaped)

# Fill missing values in vetorial
predicted_calc1 <- rpart( Álgebra.Vetorial.e.Geometria.Analítica ~ Cálculo.Diferencial.e.Integral.I
                          + Leitura.e.Produção.de.Textos
                          + Introdução.à.Computação 
                          + Programação.I 
                          + Laboratório.de.Programação.I, 
                          data = all_data_reshaped[!is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica),], 
                         method = "anova")


all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica),])

summary(all_data_reshaped)

# Fill missing values in ic
predicted_ic <- rpart( Introdução.à.Computação ~ Cálculo.Diferencial.e.Integral.I
                          + Leitura.e.Produção.de.Textos
                          +  Laboratório.de.Programação.I
                          + Programação.I 
                          + Álgebra.Vetorial.e.Geometria.Analítica, 
                          data = all_data_reshaped[!is.na(all_data_reshaped$Introdução.à.Computação),], 
                          method = "anova")


all_data_reshaped$Introdução.à.Computação[is.na(all_data_reshaped$Introdução.à.Computação)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Introdução.à.Computação),])

summary(all_data_reshaped)

# Fill missing values in lab1
predicted_lab1 <- rpart( Laboratório.de.Programação.I ~ Cálculo.Diferencial.e.Integral.I
                         + Leitura.e.Produção.de.Textos
                         + Introdução.à.Computação 
                         + Programação.I 
                         + Álgebra.Vetorial.e.Geometria.Analítica, 
                         data = all_data_reshaped[!is.na(all_data_reshaped$Laboratório.de.Programação.I),], 
                         method = "anova")


all_data_reshaped$Laboratório.de.Programação.I[is.na(all_data_reshaped$Laboratório.de.Programação.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Laboratório.de.Programação.I),])

summary(all_data_reshaped)

# Fill missing values in p1
predicted_p1 <- rpart(  Programação.I ~ Cálculo.Diferencial.e.Integral.I
                         + Leitura.e.Produção.de.Textos
                         + Introdução.à.Computação 
                         + Laboratório.de.Programação.I  
                         + Álgebra.Vetorial.e.Geometria.Analítica, 
                         data = all_data_reshaped[!is.na(all_data_reshaped$Programação.I),], 
                         method = "anova")


all_data_reshaped$Programação.I[is.na(all_data_reshaped$Programação.I)] <- 
  predict(predicted_calc1, all_data_reshaped[is.na(all_data_reshaped$Programação.I),])

summary(all_data_reshaped)


# Fill missing values in cra
all_data_reshaped$cra <- (all_data_reshaped$Cálculo.Diferencial.e.Integral.I * 4
                         + all_data_reshaped$Leitura.e.Produção.de.Textos * 4
                         + all_data_reshaped$Introdução.à.Computação * 4
                         + all_data_reshaped$Laboratório.de.Programação.I  * 4
                         + all_data_reshaped$Álgebra.Vetorial.e.Geometria.Analítica * 4
                         + all_data_reshaped$Programação.I * 4
                         ) / 24


# Split the data back into train and test
split <- createDataPartition(y=all_data_reshaped$EVADIU, p = 0.90, list = FALSE)
train <- all_data_reshaped[split,]
test <- all_data_reshaped[-split,]

# set seed for reproducibility
set.seed(111)


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
