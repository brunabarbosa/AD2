##colocar o codigo de importar os data frames

##Pre processamento
library(readr)
library(dplyr)
graduados <- read_csv("graduados.csv", col_types = cols(matricula = col_character())) %>%
  mutate(matricula = as.factor(matricula))
head(graduados)

summary(graduados)

graduados <- graduados %>%
  arrange(matricula)

graduados.clean <- graduados %>%
  filter(!is.na(media))

graduados.cra <- graduados.clean %>%
  group_by(matricula) %>%
  mutate(cra.contrib = media*creditos) %>%
  summarise(cra = sum(cra.contrib)/sum(creditos))

head(graduados.cra)
library(caret)
library(reshape2)

graduados.model.input <- graduados.clean %>%
  group_by(matricula,disciplina)  %>%
  filter(media == max(media)) %>%
  ungroup() %>%
  select(matricula,disciplina,media) %>% 
  mutate(disciplina = as.factor(gsub(" ",".",disciplina))) %>%
  dcast(matricula ~ disciplina, mean) %>%
  merge(graduados.cra)

disciplinas <- data.frame(calc1 = graduados.model.input$Cálculo.Diferencial.e.Integral.I, 
                          vetorial = graduados.model.input$Álgebra.Vetorial.e.Geometria.Analítica,
                          lpt = graduados.model.input$Leitura.e.Produção.de.Textos,
                          p1 = graduados.model.input$Programação.I,
                          ic = graduados.model.input$Introdução.à.Computação,
                          lp1 = graduados.model.input$Laboratório.de.Programação.I,
                          
                          calc2 = graduados.model.input$Cálculo.Diferencial.e.Integral.II,
                          md = graduados.model.input$Matemática.Discreta,
                          p2 = graduados.model.input$Programação.II,
                          classica = graduados.model.input$Fundamentos.de.Física.Clássica,
                          lp2 = graduados.model.input$Laboratório.de.Programação.II,
                          grafos = graduados.model.input$Teoria.dos.Grafos,
                          cra = graduados.model.input$cra)

disciplinas <- na.omit(disciplinas)


#Divide o data frame: 90% - train, 10% - teste 
split <- createDataPartition(y=disciplinas$cra, p = 0.90, list = FALSE)
train <- disciplinas[split,]
test <- disciplinas[-split,]


set.seed(113) # for reproducing these results
ridge <- train(cra ~., data = train,
               method='ridge',
               lambda = 4)

ridge.pred <- predict(ridge, test)

sqrt(mean(ridge.pred - test$cra)^2)


#cross validation
set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

ridge <- train(cra~., data = train,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid
)

#regressao da reta
ridge.pred <- predict(ridge, test)

#valores dos coeficientes
predict(ridge$finalModel, type='coef', mode='norm')$coefficients[12,]

#importancia de cada atributo
#lpt nao explica bem a variavel resposta cra
varImp(ridge)
sqrt(mean(ridge.pred - test$cra)^2)

#metodo lasso

lasso <- train(cra ~., train,
               method='lasso',
               trControl=fitControl)

predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, mode='fraction')

lasso.pred <- predict(lasso, test)
sqrt(mean(lasso.pred - test$cra)^2)


#problema de poucas linhas no teste
#depois que rodar o lasso e descobrir as variaveis mais importantes
#nao retirar essas linhas mesmo que tenha NA
