install.packages("ISLR")
library(ISLR)
library(caret)

Hitters <- na.omit(Hitters)

split <- createDataPartition(y=Hitters$Salary, p = 0.5, list = FALSE)
train <- Hitters[split,]
test <- Hitters[-split,]

set.seed(825) # for reproducing these results
ridge <- train(Salary ~., data = train,
               method='ridge',
               lambda = 4,
               preProcess=c('scale', 'center'))

ridge.pred <- predict(ridge, test)

sqrt(mean(ridge.pred - test$Salary)^2)

#validacao cruzada para achar o lambda