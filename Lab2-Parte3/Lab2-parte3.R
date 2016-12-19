setwd("~/Documents/ADII/Lab2-Parte3")

install.packages("ISLR")
install.packages("dplyr")
install.packages("caret")

library(readr)
library(dplyr)
library(ISLR)
library(caret)
library(readr)
library(reshape2)

#importando os dados
treino_base <- read_csv("~/Documents/ADII/Lab2-Parte3/treino_base.csv")
teste_base <- read_csv("~/Documents/ADII/Lab2-Parte3/teste_base.csv")


graduados_treino_base <- read_csv("treino_base.csv", col_types = cols(ALU_NOVAMATRICULA = col_character())) %>%
  mutate(ALU_NOVAMATRICULA = as.factor(ALU_NOVAMATRICULA))


str(graduados_treino_base)
summary(graduados_treino_base)
#View(graduados_treino_base)
graduados_treino_base <- graduados_treino_base %>%
  arrange(ALU_NOVAMATRICULA)

#remover NAs 
grad_treino_base_clean <- graduados_treino_base %>%
  filter(!is.na(MAT_MEDIA_FINAL))
summary(grad_treino_base_clean)
#View(grad_treino_base_clean)

#Calcular CRA e remover notas de reprovacoes
treino_base_cra <- grad_treino_base_clean %>%
  group_by(ALU_NOVAMATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))

head(treino_base_cra)




treino.model.input <- grad_treino_base_clean %>%
  group_by(ALU_NOVAMATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(ALU_NOVAMATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(treino_base_cra)

head(treino.model.input)
View(treino.model.input)
##---------------------------------------------------

##Pre processamento dados teste

graduados_teste_base <- read_csv("teste_base.csv", col_types = cols(ALU_NOVAMATRICULA = col_character())) %>%
  mutate(ALU_NOVAMATRICULA = as.factor(ALU_NOVAMATRICULA))


str(graduados_teste_base)
summary(graduados_teste_base)
#View(graduados_teste_base)
graduados_teste_base <- graduados_teste_base %>%
  arrange(ALU_NOVAMATRICULA)

#remover NAs 
grad_teste_base_clean <- graduados_teste_base %>%
  filter(!is.na(MAT_MEDIA_FINAL))
summary(grad_teste_base_clean)
#View(grad_teste_base_clean)

#calcular cra
teste_base_cra <- grad_teste_base_clean %>%
  group_by(ALU_NOVAMATRICULA) %>%
  mutate(cra.contrib = MAT_MEDIA_FINAL*CREDITOS) %>%
  summarise(cra = sum(cra.contrib)/sum(CREDITOS))

head(teste_base_cra)



#Calcular CRA e remover notas de reprovacoes
teste.model.input <- grad_teste_base_clean %>%
  group_by(ALU_NOVAMATRICULA,DISCIPLINA)  %>%
  filter(MAT_MEDIA_FINAL == max(MAT_MEDIA_FINAL)) %>%
  ungroup() %>%
  select(ALU_NOVAMATRICULA,DISCIPLINA,MAT_MEDIA_FINAL) %>% 
  mutate(DISCIPLINA = as.factor(gsub(" ",".",DISCIPLINA))) %>%
  dcast(ALU_NOVAMATRICULA ~ DISCIPLINA, mean) %>%
  merge(teste_base_cra)

head(teste.model.input)
#View(teste.model.input)
head(treino.model.input)
treino_reshaped <- data.frame(matricula = treino.model.input$ALU_NOVAMATRICULA,
                              calc1 = treino.model.input$Cálculo.Diferencial.e.Integral.I, 
                              vetorial = treino.model.input$Álgebra.Vetorial.e.Geometria.Analítica,
                              lpt = treino.model.input$Leitura.e.Produção.de.Textos,
                              p1 = treino.model.input$Programação.I,
                              ic = treino.model.input$Introdução.à.Computação,
                              lp1 = treino.model.input$Laboratório.de.Programação.I,
                              
                              calc2 = treino.model.input$Cálculo.Diferencial.e.Integral.II,
                              md = treino.model.input$Matemática.Discreta,
                              p2 = treino.model.input$Programação.II,
                              grafos = treino.model.input$Teoria.dos.Grafos,
                              classica = treino.model.input$Fundamentos.de.Física.Clássica,
                              lp2 = treino.model.input$Laboratório.de.Programação.II,
                              cra = treino.model.input$cra)
#View(treino_reshaped)

teste_reshaped <- data.frame(matricula = teste.model.input$ALU_NOVAMATRICULA,
                             calc1 = teste.model.input$Cálculo.Diferencial.e.Integral.I, 
                             vetorial = teste.model.input$Álgebra.Vetorial.e.Geometria.Analítica,
                             lpt = teste.model.input$Leitura.e.Produção.de.Textos,
                             p1 = teste.model.input$Programação.I,
                             ic = teste.model.input$Introdução.à.Computação,
                             lp1 = teste.model.input$Laboratório.de.Programação.I,
                             
                             calc2 = teste.model.input$Cálculo.Diferencial.e.Integral.II,
                             md = teste.model.input$Matemática.Discreta,
                             p2 = teste.model.input$Programação.II,
                             classica = teste.model.input$Fundamentos.de.Física.Clássica,
                             lp2 = teste.model.input$Laboratório.de.Programação.II,
                             grafos = teste.model.input$Teoria.dos.Grafos,
                             cra = teste.model.input$cra)
#View(teste_reshaped)

#write.csv(treino_reshaped, file = "treinoreshaped.csv", row.names = FALSE)
#write.csv(teste_reshaped, file = "testereshaped.csv", row.names = FALSE)


#Usando todas as variáveis disponíveis (disciplinas do primeiro e segundo período), use validação cruzada 
#(nos dados de treino) para tunar um modelo de regressão Ridge.

treino_reshaped <- na.omit(treino_reshaped)
teste_reshaped <- na.omit(teste_reshaped)



#cross validation
set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

ridge <- train(cra~., data = treino_reshaped,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid
)

#regressao da reta
ridge.pred <- predict(ridge, teste_reshaped)

#valores dos coeficientes
predict(ridge$finalModel, type='coef', mode='norm')$coefficients[12,]

#importancia de cada atributo
#lpt nao explica bem a variavel resposta cra
varImp(ridge)
sqrt(mean(ridge.pred - teste_reshaped$cra)^2)

#metodo lasso

lasso <- train(cra ~., treino_reshaped,
               method='lasso',
               trControl=fitControl)

predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, mode='fraction')

lasso.pred <- predict(lasso, teste_reshaped)
sqrt(mean(lasso.pred - teste_reshaped$cra)^2)


##linear model

lmfit <- train(cra ~., data = treino_reshaped,
               method='lm',
               trControl = fitControl)
lmfit
coef(lmfit$finalModel)
lmfit.pred <- predict(lmfit, teste_reshaped)
sqrt(mean(lmfit.pred - teste_reshaped$cra)^2)

#problema de poucas linhas no teste
#depois que rodar o lasso e descobrir as variaveis mais importantes
#nao retirar essas linhas mesmo que tenha NA
