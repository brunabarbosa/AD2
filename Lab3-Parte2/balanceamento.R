#install packages
install.packages("ROSE")

library(ROSE)

data(hacide)
str(hacide.train)

prop.table(table(hacide.train$cls))

library(rpart)
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)

pred.treeimb[,2]

model <- glm(cls ~.,family=binomial, data=hacide.train)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

predict <- predict(model, type = 'response')

accuracy.meas(hacide.test$cls, pred.treeimb[,2])

